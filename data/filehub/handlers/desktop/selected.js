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
function cancel() {
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
function selectN(item) {
    const id = item.dataset.path;
    if (item.classList.contains('selected'))
        return;
    selectGo({
        prepare: () => { selectedIds.add(id); },
        confirm: () => { item.classList.add('selected'); },
        recover: () => { selectedIds.delete(id); }
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
    if (!(e instanceof MouseEvent))
        return;
    if (selectionScreen === null)
        return;
    if (!isMouseDown)
        return;
    let x = e.clientX;
    let y = e.clientY;
    const left = Math.min(selectionScreen.x, x);
    const top = Math.min(selectionScreen.y, y);
    const width = Math.abs(x - selectionScreen.x);
    const height = Math.abs(y - selectionScreen.y);
    // ignore small mouse movement
    if (width > 50 || height > 50) {
        dragging = true;
        selectionScreen.elt.style.top = top.toString();
        selectionScreen.elt.style.left = left.toString();
        selectionScreen.elt.style.width = width.toString();
        selectionScreen.elt.style.height = height.toString();
    }
    let rect = selectionScreen.elt.getBoundingClientRect();
    Array
        .from(fileItems)
        .filter(item => {
        let itemRect = item.getBoundingClientRect();
        return !(itemRect.left > rect.right ||
            itemRect.right < rect.left ||
            itemRect.top > rect.bottom ||
            itemRect.bottom < rect.top);
    })
        .forEach(selectN);
}
function handleMouseUp(e) {
    isMouseDown = false;
    function handleClick(e) {
        if (!(e instanceof MouseEvent))
            return;
        let item = e.target.closest('.table-item');
        if (!item)
            return;
        select1(item);
    }
    if (!(e instanceof MouseEvent))
        return;
    if (!dragging) {
        if (e.ctrlKey || e.metaKey) {
            view.addEventListener('click', prevent, true);
            // 'click' is fired right after mouseup.
            // we block click on this event loop cycle. set timeout 0 will trigger the call back
            // on the next cycle, in which click is fired exactly once.
            setTimeout(() => { view.removeEventListener("click", prevent, true); }, 0);
            handleClick(e);
        }
        return;
    }
    // update sidebar
    htmx
        .ajax('GET', `/refresh?component=UIComponentSideBar`, { target: '#side-bar', swap: 'outerHtml' })
        .finally((_) => {
        dragging = false;
        selectionScreen?.elt.remove();
        selectionScreen = null;
    });
}
function prevent(e) {
    e.preventDefault();
    e.stopImmediatePropagation();
}
function selectGo(hooks) {
    hooks.prepare();
    const values = { selected: Array.from(selectedIds) };
    htmx.ajax('POST', '/table/select', {
        values,
        headers: {
            'Content-Type': 'application/x-www-form-urlencoded'
        },
        target: '#control-panel',
        swap: 'outerHTML'
    }).then((_) => {
        hooks.confirm();
    }).catch((_) => {
        hooks.recover();
        console.error('failed to select');
    });
}
function select1(item) {
    const id = item.dataset.path;
    if (selectedIds.has(id)) {
        selectGo({
            prepare: () => { selectedIds.delete(id); },
            confirm: () => { item.classList.remove('selected'); },
            recover: () => { selectedIds.add(id); }
        });
    }
    else {
        selectGo({
            prepare: () => { selectedIds.add(id); },
            confirm: () => { item.classList.add('selected'); },
            recover: () => { selectedIds.delete(id); }
        });
    }
}
