/*
 * Ctrl_left-left_click to multi select
 * - Click on blank area to drag select mutiple files.
 * - holding ctrl, click on thumbnail to cherry pick.
 * */
const selectedIds = new Set();
let selectionScreen = null;
let clearSelectedHandler = _ => selectedIds.clear();
let dragging = false;
let isMouseDown = false;
let fileItems;
let table;
let view;
export function register() {
    if (!document.querySelector('#table'))
        return;
    if (!document.querySelector('#view'))
        return;
    registerAll();
    document.body.addEventListener('TargetChanged', _ => { selectedIds.clear(); });
    document.body.addEventListener('htmx:afterSettle', e => {
        unregisterAll();
        registerAll();
        clearSelectedHandler(e);
        collectFromHtml();
    });
}
function registerAll() {
    table = document.querySelector('#table');
    view = document.querySelector('#view');
    view.addEventListener('mousedown', handleMouseDown);
    view.addEventListener('mousemove', handleMouseMove);
    view.addEventListener('mouseup', handleMouseUp);
    view.addEventListener('click', cancel);
}
function unregisterAll() {
    view.removeEventListener('mousedown', handleMouseDown);
    view.removeEventListener('mousemove', handleMouseMove);
    view.removeEventListener('mouseup', handleMouseUp);
    view.removeEventListener('click', cancel);
}
function cancel(e) {
    if (e.target.closest('.dir'))
        return;
    if (e.target.closest('.dropdown-item'))
        return;
    if (selectedIds.size > 0 && !dragging) {
        htmx.ajax('POST', '/cancel', { target: '#index', swap: 'outerHTML' });
    }
}
// Collect selected rows to the set `selectedIds`
function collectFromHtml() {
    fileItems = document.querySelectorAll('#table .table-item');
    fileItems.forEach(item => {
        const id = item.dataset.path;
        if (item.classList.contains('selected')) {
            selectedIds.add(id);
        }
    });
}
// when ctrl-click-drag, draw a translucent rectangle #selection-screen
// use intersection observer to register callback on each file-item, once it
// intersects with the rectangle, select it.
// Once it's moved out of the rectangle, remove the selection
// Once mouse up, commit the selection
function handleMouseDown(e) {
    if (e.target.closest(".thumbnail"))
        return;
    if (!(e instanceof MouseEvent))
        return;
    isMouseDown = true;
    dragging = false;
    selectionScreen?.elt.remove();
    fileItems = document.querySelectorAll('#table .table-item'); // cache file items
    let x = e.clientX;
    let y = e.clientY;
    let elt = document.createElement('div');
    elt.id = 'selection-screen';
    elt.style.left = x.toString();
    elt.style.top = y.toString();
    elt.style.width = '0';
    elt.style.height = '0';
    selectionScreen = { x, y, elt };
    table.appendChild(selectionScreen.elt);
}
function handleMouseMove(e) {
    if (!isMouseDown)
        return;
    if (!(e instanceof MouseEvent))
        return;
    if (selectionScreen === null)
        return;
    let x = e.clientX;
    let y = e.clientY;
    const left = Math.min(selectionScreen.x, x);
    const top = Math.min(selectionScreen.y, y);
    const width = Math.abs(x - selectionScreen.x);
    const height = Math.abs(y - selectionScreen.y);
    dragging = false;
    if (width > 100 || height > 100) {
        dragging = true;
        selectionScreen.elt.style.top = top.toString();
        selectionScreen.elt.style.left = left.toString();
        selectionScreen.elt.style.width = width.toString();
        selectionScreen.elt.style.height = height.toString();
        let rect = selectionScreen.elt.getBoundingClientRect();
        Array
            .from(fileItems)
            .forEach(item => {
            const id = item.dataset.path;
            let itemRect = item.getBoundingClientRect();
            let inRect = !(itemRect.left > rect.right ||
                itemRect.right < rect.left ||
                itemRect.top > rect.bottom ||
                itemRect.bottom < rect.top);
            if (inRect && !item.classList.contains('selected')) {
                selectedIds.add(id);
                item.classList.add('selected');
            }
        });
    }
}
async function handleMouseUp(e) {
    isMouseDown = false;
    if (!(e instanceof MouseEvent))
        return;
    if (!dragging) { // ctrl + click
        if (e.ctrlKey || e.metaKey) {
            view.addEventListener('click', prevent, true);
            // 'click' is fired right after mouseup.
            // we block click on this event loop cycle. set timeout 0 will trigger the call back
            // on the next cycle, in which click is fired exactly once.
            setTimeout(() => { view.removeEventListener("click", prevent, true); }, 0);
            let item = e.target.closest('.table-item');
            const id = item.dataset.path;
            if (!item)
                return;
            await selectByClicking(item);
            // we only add .confirmed class if we are selecting an item. If we are unselecting we should
            // just skip.
            if (selectedIds.has(id))
                confirm();
            return;
        }
    }
    else { // dragging
        await selectRequest();
        dragging = false;
        selectionScreen?.elt.remove();
        selectionScreen = null;
        confirm();
    }
}
function confirm() {
    document.querySelectorAll('.selected').forEach(item => {
        item.classList.add('confirmed');
    });
}
async function selectRequest() {
    const values = { selected: Array.from(selectedIds) };
    await htmx.ajax('POST', '/table/select', {
        values,
        headers: {
            'Content-Type': 'application/x-www-form-urlencoded'
        },
        source: "#control-panel",
        target: '#control-panel',
        swap: 'outerHTML'
    });
}
async function selectByClicking(item) {
    const id = item.dataset.path;
    if (selectedIds.has(id)) {
        selectedIds.delete(id);
        item.classList.remove('selected');
        item.classList.remove('confirmed');
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
            selectedIds.add(id);
            item.classList.add('selected');
        }
        else {
            selectedIds.delete(id);
            item.classList.remove('selected');
            item.classList.remove('confirmed');
        }
    }
}
function prevent(e) {
    e.preventDefault();
    e.stopImmediatePropagation();
}
