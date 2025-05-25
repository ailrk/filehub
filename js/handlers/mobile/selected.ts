declare var htmx: any;
/* Long hold on a file to enter selection mode
 * on selection mode, click on any file will select it
 * click again will unselect it.
 * scroll down will trigger multiple selections
 * click on toolbar to exit selection mode
 *
 * While there are things selected, we disable any actions on the table.
 * */

import {
  ObservableCell,
  createObservableCell
} from '../../containers/Observable.js'

/* selectedIds is a local buffer that holds the currently selected entries on the table.
 * Note it DOESN'T hold selected entries across folders nor across targets.
 * The sole purpose is to control the local highlight behavior.
 * The server side selected state is updated through the `X-Filehub-Selected-*` headers. Use
 * that to render the UI instead.
 * */
const selectedIds: Set<string> = new Set();

let holdTimer: number | null = null;
let mousedownHandler: EventListener | null;
let mouseupHandler: EventListener | null;
let touchstartHandler: EventListener | null;
let touchendHandler: EventListener | null;
let clearSelectedHandler: EventListener = _ => selectedIds.clear();
let mousePosition: [x: number, y: number, vx: number, vy: number, t: number][] = [];

/* The number of selected entries, should only be set by `X-File-Selected-Count` */
let selectedCount: ObservableCell<number> = createObservableCell(0, showSelectedCounter);


export function register() {
  document.body.addEventListener('TargetChanged', clearSelectedHandler)

  document.body.addEventListener('htmx:afterSettle', e => {
    let table = document.querySelector('#table')!;
    updateSelectedCount(e)
    if ((e as CustomEvent).detail.elt.contains(table)) { // only rebind when table's parents is re-rendered.
      unregisterAll()
      registerAll()
      clearSelectedHandler(e)
      collectFromHtml()
    }
    showSelectedCounter(selectedCount.get()) // make sure if selectedCount > 0 the counter stick across page reload.
  })
  registerAll()
}


function updateSelectedCount(e: Event) {
    let count: number = parseInt((e as CustomEvent).detail.xhr.getResponseHeader('X-Filehub-Selected-Count'));
    if (!Number.isNaN(count)) {
      selectedCount.set(count)
    }
}

function registerAll() {
  let selectedCounter = document.querySelector('#selected-counter')!
  let table = document.querySelector('#table')!;
  selectedCounter.addEventListener('click', clearSelectedHandler)
  table.removeEventListener('click', preventDefault)
  /* Because the handlers are installed on local elements (#table),
   * page updates can cause old listeners to be staled.
   * we need to install new one when there is page update.
   * */
  registerTableTouch()
  registerTableMouse()
}


/* Fully reset the handlers in the page. */
function unregisterAll() {
  let table = document.querySelector('#table')!;
  let selectedCounter = document.querySelector('#selected-counter')!
  table.removeEventListener('mousemove', guard)
  table.removeEventListener('mousemove', drag)
  table.removeEventListener('click', preventDefault)
  selectedCounter.removeEventListener('click', clearSelectedHandler)
  unregisterTableTouch()
  unregisterTableMouse()
}


/* Abort selection timer if acceleration is higher than a thresdshold */
function guard(e: Event) {
  let evt = e as MouseEvent
  let [x, y] = [evt.clientX, evt.clientY]
  let prev = mousePosition.pop()
  let t = performance.now()
  let acceleration = 0

  if (prev) {
    let [xprev, yprev, vxprev, vyprev, tprev] = prev
    let dx = x - xprev
    let dy = y - yprev
    let dt = t - tprev
    let vx = dx/dt
    let vy = dy/dt
    let ax = (vx - vxprev)/dt
    let ay = (vy - vyprev)/dt
    let a = Math.sqrt(ax * ax + ay * ay)
    acceleration = a
    mousePosition.push([x, y, vx, vy, t])
  } else {
    mousePosition.push([x, y, 0, 0, t])
  }

  if (holdTimer && acceleration > 0.03) {
    clearTimeout(holdTimer)
  }
}


function preventDefault(e: Event) {
  e.preventDefault()
}


function makeStarthandler(table: HTMLElement, movevt: string) {
  return (e: Event) => {
    // If mouse moved before holding is triggerd, cancel the current timer and start a new one.
    // We want to make sure you have to hold on one place for some time to trigger the selection.
    table.addEventListener(movevt, guard)
    table.addEventListener('onclick', preventDefault, true)

    holdTimer = setTimeout(() => {
      // Entering holding mode
      table.removeEventListener(movevt, guard);
      table.addEventListener(movevt, drag);
      // Suppress the click entirely by preventing mouseup propagation
      e.stopImmediatePropagation();
      e.preventDefault();
      let tr = (e.target as Element).closest('tr');
      if (tr) {
        select(tr)
      }
    }, 500);
  }
}


function makeEndHandler(table: HTMLElement, movevt: string) {
  return (_: Event) => {
    if (holdTimer) { clearTimeout(holdTimer) }
    table.removeEventListener(movevt, guard)
    table.removeEventListener(movevt, drag)
    table.removeEventListener('onclick', preventDefault)
  }
}


function registerTableMouse() {
  let table = document.querySelector('#table');
  mousedownHandler = makeStarthandler(table! as HTMLElement, 'mousemove')
  mouseupHandler = makeEndHandler(table! as HTMLElement, 'mousemove')
  table!.addEventListener('mousedown', mousedownHandler, { capture: true })
  table!.addEventListener('mouseup', mouseupHandler, { capture: true })
}


function unregisterTableMouse() {
  let table = document.querySelector('#table');
  if (mousedownHandler) {
    table!.removeEventListener('mousedown', mousedownHandler)
  }
  if (mouseupHandler) {
    table!.removeEventListener('mouseup', mouseupHandler)
  }
}


function registerTableTouch() {
  let table = document.querySelector('#table');
  touchstartHandler = makeStarthandler(table! as HTMLElement, 'touchmove')
  touchendHandler = makeEndHandler(table! as HTMLElement, 'touchmove')
  table!.addEventListener('touchstart', touchstartHandler, { capture: true })
  table!.addEventListener('touchend', touchendHandler, { capture: true })
}


function unregisterTableTouch() {
  let table = document.querySelector('#table');
  if (touchstartHandler) {
    table!.removeEventListener('touchstart', touchstartHandler)
  }
  if (touchendHandler) {
    table!.removeEventListener('touchend', touchendHandler)
  }
}


/* Only show the counter when there is more than 0 selected item */
function showSelectedCounter(count: number) {
  // watch selected id and display selected counter accordingly.
  let selectedCounter = document.querySelector('#selected-counter')!
  if (count > 0) {
    (selectedCounter.firstChild as HTMLElement).innerText = count.toString()
    selectedCounter.classList.add('show')
  } else {
    (selectedCounter.firstChild as HTMLElement).innerText = "0"
    selectedCounter.classList.remove('show')
  }
}


// Collect selected rows to the set `selectedIds`. This is useful when the backend
// updated selected rows.
function collectFromHtml() {
  let rows: NodeListOf<HTMLElement> = document.querySelectorAll('#table tr')
  rows.forEach(row => {
    let id = row.dataset.path!
    if (row.classList.contains('selected')) {
      selectedIds.add(id)
    }
  })
}


function drag(e: Event) {
  e.preventDefault()
  let tr = (e.target as Element).closest('tr');
  if (tr) {
    const id = tr.dataset.path!;
    if (!selectedIds.has(id))  {
      select(tr)
    }
  }
}


/* Select handler */
function select(row: HTMLElement) {
  let id = row.dataset.path!
  function select(
    hooks: {
      prepare: () => void,
      confirm: () => void,
      recover: () => void
    }) {
    hooks.prepare()
    let values: Record<string, string[]> = { selected: Array.from(selectedIds.values()) }
    htmx.ajax('POST',
      '/table/select',
      {
        values,
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded'
        },
        target: '#control-panel',
        swap: 'outerHTML'
      }
    ).then((_: any) => {
      hooks.confirm()
    }).catch((_: any) => {
      hooks.recover()
      console.error('failed to select')
    })
  }
  if (selectedIds.has(id)) {
    select({
      prepare: () => { selectedIds.delete(id) },
      confirm: () => { row.classList.remove('selected') },
      recover: () => { selectedIds.add(id) }
    })
  } else {
    select({
      prepare: () => { selectedIds.add(id) },
      confirm: () => { row.classList.add('selected') },
      recover: () => { selectedIds.delete(id) }
    })
  }
}
