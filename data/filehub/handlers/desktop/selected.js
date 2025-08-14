/*
 * Ctrl_left-left_click to multi select
 * - Hold ctrl left and click on file will select it.
 * - Keep holding ctrl and click multiple files will select multiple.
 * - Keep holding ctrl , click on a file and drag will select files on the pass.
 * */
const selectedIds = new Set();
let selectionScreen = null;
let clearSelectedHandler = _ => selectedIds.clear();
let dragging = false;
let fileItems;
let table = document.querySelector('#table');
export function register() {
    document.body.addEventListener('TargetChanged', _ => { selectedIds.clear(); });
    document.body.addEventListener('htmx:afterSettle', e => {
        table = document.querySelector('#table');
        unregisterAll();
        registerAll();
        clearSelectedHandler(e);
        collectFromHtml();
    });
    registerAll();
}
function registerAll() {
    table.addEventListener('mousedown', handleMouseDown);
    table.addEventListener('mousemove', handleMouseMove);
    table.addEventListener('mouseup', handleMouseUp);
}
function unregisterAll() {
    table.removeEventListener('mousedown', handleMouseDown);
    table.removeEventListener('mousemove', handleMouseMove);
    table.removeEventListener('mouseup', handleMouseUp);
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
function handleClick(evt) {
    let e = evt;
    let item = e.target.closest('.table-item');
    if (!item)
        return;
    if (e.ctrlKey || e.metaKey) {
        select1(item);
    }
}
// when ctrl-click-drag, draw a translucent rectangle #selection-screen
// use intersection observer to register callback on each file-item, once it
// intersects with the rectangle, select it.
// Once it's moved out of the rectangle, remove the selection
// Once mouse up, commit the selection
function handleMouseDown(e) {
    let evt = e;
    selectionScreen?.elt.remove();
    if (!(evt.ctrlKey || evt.metaKey))
        return;
    evt.preventDefault();
    evt.stopImmediatePropagation();
    document.addEventListener('dragstart', prevent, true); // temporary disable drags
    fileItems = document.querySelectorAll('#table .table-item'); // cache file items
    dragging = false;
    let x = evt.clientX;
    let y = evt.clientY;
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
    let evt = e;
    if (!(evt.ctrlKey || evt.metaKey)) {
        selectionScreen?.elt.remove();
        return;
    }
    if (selectionScreen === null) {
        return;
    }
    let x = evt.clientX;
    let y = evt.clientY;
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
    let evt = e;
    if (!(evt.ctrlKey || evt.metaKey))
        return;
    table.addEventListener('click', prevent, true);
    // 'click' is fired right after mouseup.
    // we block click on this event loop cycle. set timeout 0 will trigger the call back
    // on the next cycle, in which click is fired exactly once.
    setTimeout(() => { table.removeEventListener("click", prevent, true); }, 0);
    if (!dragging) {
        handleClick(e);
    }
    document.removeEventListener('dragstart', prevent, true); // restore drags
    selectionScreen?.elt.remove();
    selectionScreen = null;
    dragging = false;
    // update sidebar
    htmx.ajax('GET', `/refresh?component=UIComponentSideBar`, { target: '#side-bar', swap: 'outerHtml' });
}
function prevent(e) {
    e.preventDefault();
    e.stopImmediatePropagation();
}
