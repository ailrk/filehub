declare var htmx: any;
/* Long hold on a file to enter selection mode.
 * While selecting, tapping a row toggles it, and holding briefly then
 * dragging selects every row you pass over.
 * Tap the counter to clear the selection.
 *
 * Implementation notes:
 *  - One code path via Pointer Events (mouse, touch and pen).
 *  - Listeners are tied to an AbortController, so rebinding after an htmx
 *    swap can never leave stale listeners behind.
 *  - Selection is optimistic: rows highlight immediately, and one batched
 *    request syncs the server state.
 */

import {
  ObservableCell,
  createObservableCell
} from '../../containers/Observable.js'


/* selectedIds is a local buffer holding the currently selected entries on the table.
 * It does NOT hold selections across folders or targets.
 * The server side state is updated through the `X-Filehub-Selected-*` headers;
 * use that to render the UI.
 */
const selectedIds: Set<string> = new Set();

const HOLD_TIME = 500;           // ms to hold before selection starts
const HOLD_TIME_SELECTING = 250; // shorter hold once already selecting (drag-select)
const SLOP = 10;                 // px of movement that cancels a hold (it's a scroll)
const SYNC_DELAY = 60;           // ms to batch select requests
const CLICK_GUARD = 350;         // ms to swallow the click that follows a gesture

let ac = new AbortController();
let start: { x: number, y: number } | null = null;
let dragging = false;
let suppressClick = false;
let holdTimer: number | undefined;
let syncTimer: number | undefined;
let clickGuardTimer: number | undefined;
let bodyListenersInstalled = false;

/* The number of selected entries, should only be set by `X-Filehub-Selected-Count` */
let selectedCount: ObservableCell<number> = createObservableCell(0, _ => {});

const clearSelectedHandler: EventListener = _ => { selectedIds.clear() };


export function register() {
  if (!document.querySelector('#table')) return;
  if (!document.querySelector('#selected-counter')) return;

  if (!bodyListenersInstalled) {
    bodyListenersInstalled = true;

    document.body.addEventListener('TargetChanged', clearSelectedHandler);

    document.body.addEventListener('htmx:afterSettle', e => {
      const detail = (e as CustomEvent).detail;
      const table = document.querySelector('#table');

      // update selected count
      const header = detail?.xhr?.getResponseHeader('X-Filehub-Selected-Count');
      const count = parseInt(header);
      if (!Number.isNaN(count)) {
        selectedCount.set(count);
      }

      if (table &&
          // only rebind when the table's parents are re-rendered.
          detail?.elt?.contains(table) &&
          // no need to rebind when rendering modals
          !detail?.pathInfo?.responsePath?.startsWith('/modal')) {
        unbind();
        bind();
        clearSelectedHandler(e);
        collectFromHtml();
      }

      // make sure the counter sticks across page reloads
      showSelectedCounter(selectedCount.get());
    });
  }

  unbind();
  bind();
}


/* ---------- Binding ---------- */

function bind() {
  const table = document.querySelector<HTMLElement>('#table');
  const counter = document.querySelector('#selected-counter');
  if (!table || !counter) return;

  const o = { signal: ac.signal };

  counter.addEventListener('click', clearAll, o);

  table.addEventListener('pointerdown', onDown, o);
  table.addEventListener('pointermove', onMove, o);
  table.addEventListener('pointerup', onUp, o);
  table.addEventListener('pointercancel', cancelPress, o);
  table.addEventListener('contextmenu', e => e.preventDefault(), o);

  // Non-passive on purpose: this is what stops the page from scrolling
  // while drag-selecting.
  table.addEventListener('touchmove', e => {
    if (dragging) e.preventDefault();
  }, { ...o, passive: false });

  // Swallow the click that follows a long press or a selection tap, so the
  // file doesn't open.
  table.addEventListener('click', e => {
    if (suppressClick) {
      e.preventDefault();
      e.stopPropagation();
    }
  }, { ...o, capture: true });
}


/* Fully reset the handlers in the page. */
function unbind() {
  ac.abort();
  ac = new AbortController();
  cancelPress();
}


/* ---------- Gesture handling ---------- */

function rowOf(el: Element | null): HTMLElement | null {
  return el?.closest<HTMLElement>('tr[data-path]') ?? null;
}


function onDown(e: PointerEvent) {
  if (e.pointerType === 'mouse' && e.button !== 0) return;
  const tr = rowOf(e.target as Element);
  if (!tr) return;

  clearTimeout(holdTimer);
  start = { x: e.clientX, y: e.clientY };
  dragging = false;
  tr.classList.add('pressing');

  const delay = selectedIds.size > 0 ? HOLD_TIME_SELECTING : HOLD_TIME;
  holdTimer = window.setTimeout(() => {
    dragging = true;
    suppressClick = true;
    tr.classList.remove('pressing');
    navigator.vibrate?.(15); // Android only; iOS ignores it
    add(tr);
  }, delay);
}


function onMove(e: PointerEvent) {
  if (!start) return;

  if (!dragging) {
    // Moved before the hold completed: the user is scrolling.
    if (Math.hypot(e.clientX - start.x, e.clientY - start.y) > SLOP) {
      cancelPress();
    }
    return;
  }

  const tr = rowOf(document.elementFromPoint(e.clientX, e.clientY));
  if (tr) add(tr);
}


function onUp(e: PointerEvent) {
  const wasTap = start !== null && !dragging;
  const tr = rowOf(e.target as Element);

  // While selecting, a plain tap toggles the row.
  if (wasTap && selectedIds.size > 0 && tr) {
    suppressClick = true;
    selectedIds.has(tr.dataset.path!) ? remove(tr) : add(tr);
  }

  const swallow = suppressClick;
  cancelPress();
  if (swallow) armClickGuard();
}


/* Reset the in-progress gesture (not the selection). */
function cancelPress() {
  clearTimeout(holdTimer);
  document.querySelectorAll('#table tr.pressing')
    .forEach(r => r.classList.remove('pressing'));
  const wasDragging = dragging;
  start = null;
  dragging = false;
  if (wasDragging) armClickGuard();
}


function armClickGuard() {
  clearTimeout(clickGuardTimer);
  clickGuardTimer = window.setTimeout(() => { suppressClick = false }, CLICK_GUARD);
}


/* ---------- Selection state ---------- */

/* Optimistic UI plus one batched request. */
function add(tr: HTMLElement) {
  const id = tr.dataset.path!;
  if (selectedIds.has(id)) return;
  selectedIds.add(id);
  tr.classList.add('selected');
  scheduleSync();
}


function remove(tr: HTMLElement) {
  const id = tr.dataset.path!;
  if (!selectedIds.has(id)) return;
  selectedIds.delete(id);
  tr.classList.remove('selected');
  scheduleSync();
}


function clearAll() {
  selectedIds.clear();
  document.querySelectorAll('#table tr.selected')
    .forEach(r => r.classList.remove('selected'));
  scheduleSync();
}


function scheduleSync() {
  clearTimeout(syncTimer);
  syncTimer = window.setTimeout(() => {
    const values: Record<string, string[]> = { selected: Array.from(selectedIds) };
    htmx.ajax('POST', '/table/select', {
      values,
      headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
      target: '#control-panel',
      swap: 'outerHTML'
    }).catch((_: any) => {
      console.error('failed to select');
    });
  }, SYNC_DELAY);
}


/* Only show the counter when there is more than 0 selected item */
function showSelectedCounter(count: number) {
  const selectedCounter = document.querySelector('#selected-counter');
  if (!selectedCounter) return;
  const label = selectedCounter.firstChild as HTMLElement | null;

  if (count > 0) {
    if (label) label.innerText = count.toString();
    selectedCounter.classList.add('show');
  } else {
    if (label) label.innerText = '0';
    selectedCounter.classList.remove('show');
  }
}


/* Collect selected rows into `selectedIds`. Useful when the backend
 * updated the selected rows.
 */
function collectFromHtml() {
  const rows: NodeListOf<HTMLElement> = document.querySelectorAll('#table tr[data-path]');
  rows.forEach(row => {
    if (row.classList.contains('selected')) {
      selectedIds.add(row.dataset.path!);
    }
  });
}
