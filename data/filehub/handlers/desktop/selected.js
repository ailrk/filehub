/*
 * Desktop multi-select:
 *   • Ctrl-click to cherry-pick individual items
 *   • Click-drag on blank area to rubber-band select
 *
 * All listeners are on `document` — no re-registration after htmx swaps.
 * main.ts calls collectFromHtml() after each htmx:afterSettle so the local
 * set stays in sync when the server re-renders selected rows.
 */
const selectedIds = new Set();
let selectionScreen = null;
let dragging = false;
let isMouseDown = false;
export function register() {
    document.addEventListener('mousedown', onMouseDown);
    document.addEventListener('mousemove', onMouseMove);
    document.addEventListener('mouseup', onMouseUp);
    document.addEventListener('click', onClick);
    document.body.addEventListener('TargetChanged', () => selectedIds.clear());
}
export function collectFromHtml() {
    const fileItems = document.querySelectorAll('#table .table-item');
    fileItems.forEach(item => {
        const id = item.dataset.path;
        if (item.classList.contains('selected'))
            selectedIds.add(id);
        else
            selectedIds.delete(id);
    });
}
function onMouseDown(e) {
    const view = document.getElementById('view');
    if (!view?.contains(e.target))
        return;
    if (e.target.closest('.thumbnail'))
        return;
    isMouseDown = true;
    dragging = false;
    selectionScreen?.elt.remove();
    const table = document.getElementById('table');
    if (!table)
        return;
    const elt = document.createElement('div');
    elt.id = 'selection-screen';
    elt.style.left = e.clientX + 'px';
    elt.style.top = e.clientY + 'px';
    elt.style.width = '0';
    elt.style.height = '0';
    selectionScreen = { x: e.clientX, y: e.clientY, elt };
    table.appendChild(elt);
}
function onMouseMove(e) {
    if (!isMouseDown || !selectionScreen)
        return;
    const x = e.clientX, y = e.clientY;
    const left = Math.min(selectionScreen.x, x);
    const top = Math.min(selectionScreen.y, y);
    const width = Math.abs(x - selectionScreen.x);
    const height = Math.abs(y - selectionScreen.y);
    if (width > 10 || height > 10) {
        dragging = true;
        selectionScreen.elt.style.top = top + 'px';
        selectionScreen.elt.style.left = left + 'px';
        selectionScreen.elt.style.width = width + 'px';
        selectionScreen.elt.style.height = height + 'px';
        const rect = selectionScreen.elt.getBoundingClientRect();
        document.querySelectorAll('#table .table-item').forEach(item => {
            const id = item.dataset.path;
            const ir = item.getBoundingClientRect();
            const hit = !(ir.left > rect.right || ir.right < rect.left ||
                ir.top > rect.bottom || ir.bottom < rect.top);
            if (hit && !selectedIds.has(id)) {
                selectedIds.add(id);
                item.classList.add('selected');
            }
        });
    }
}
async function onMouseUp(e) {
    if (!isMouseDown)
        return;
    isMouseDown = false;
    if (!dragging) {
        if (e.ctrlKey || e.metaKey) {
            const view = document.getElementById('view');
            if (!view?.contains(e.target))
                return;
            // Block the click that fires immediately after mouseup on this cycle.
            view.addEventListener('click', prevent, { capture: true, once: true });
            const item = e.target.closest('.table-item');
            if (!item)
                return;
            await selectByClicking(item);
            if (selectedIds.has(item.dataset.path))
                confirmSelected();
        }
    }
    else {
        await selectRequest();
        dragging = false;
        selectionScreen?.elt.remove();
        selectionScreen = null;
        confirmSelected();
    }
}
function onClick(e) {
    const view = document.getElementById('view');
    if (!view?.contains(e.target))
        return;
    if (e.target.closest('.dir'))
        return;
    if (e.target.closest('.dropdown-item'))
        return;
    if (selectedIds.size > 0 && !dragging) {
        htmx.ajax('POST', '/cancel', { target: 'body', swap: 'none' });
    }
}
function confirmSelected() {
    document.querySelectorAll('.selected').forEach(item => item.classList.add('confirmed'));
}
async function selectRequest() {
    const values = { selected: Array.from(selectedIds) };
    await htmx.ajax('POST', '/table/select', {
        values,
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        source: '#control-panel',
        target: '#control-panel',
        swap: 'outerHTML',
    });
}
async function selectByClicking(item) {
    const id = item.dataset.path;
    if (selectedIds.has(id)) {
        selectedIds.delete(id);
        item.classList.remove('selected', 'confirmed');
    }
    else {
        selectedIds.add(id);
        item.classList.add('selected');
    }
    try {
        await selectRequest();
    }
    catch {
        if (selectedIds.has(id)) {
            item.classList.add('selected');
        }
        else {
            item.classList.remove('selected', 'confirmed');
        }
    }
}
function prevent(e) {
    e.preventDefault();
    e.stopImmediatePropagation();
}
