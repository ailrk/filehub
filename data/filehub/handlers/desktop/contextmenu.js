export function register() {
    register1();
    document.body.addEventListener('htmx:afterSwap', _ => {
        register1();
    });
}
function register1() {
    const table = document.querySelector('#table');
    if (!table)
        return;
    document.addEventListener('contextmenu', prevent);
    table.addEventListener('contextmenu', onContextMenu); // no capture needed
    // document.querySelectorAll('#table .table-item').forEach(item => item.addEventListener('contextmenu', handle, { capture: true }))
}
function prevent(e) { e.preventDefault(); }
/* 1. fetch contextmenu for the current file
 * 2. show the context menu at the cursor position
 * 3. disable 'click' event handler on the table
 * NOTE: we restore the click event handler in closeDropdown.ts.
 * */
export function onContextMenu(e) {
    if (!(e instanceof MouseEvent))
        return;
    const table = e.currentTarget;
    const target = e.target;
    if (!target)
        return;
    // Find the row (.table-item) nearest to the click
    const item = target.closest('.table-item');
    if (!item || !table.contains(item))
        return;
    let contextMenu = document.querySelector('#contextmenu');
    const isSelected = item.classList.contains('selected');
    if (contextMenu) {
        contextMenu.remove();
    }
    const params = new URLSearchParams();
    if (isSelected) {
        let selected = document.querySelectorAll('.table-item.selected');
        selected.forEach(sel => {
            if (sel instanceof HTMLElement) {
                const path = sel.dataset.path;
                params.append('file', path);
            }
        });
    }
    else {
        const path = item.dataset.path;
        params.append('file', path);
    }
    fetch(`/contextmenu?${params.toString()}`)
        .then(res => res.text())
        .then(html => {
        const table = document.getElementById('table');
        if (table) {
            table.insertAdjacentHTML('afterend', html);
        }
        else {
            console.error('no table');
        }
        contextMenu = document.querySelector('#contextmenu');
        if (contextMenu) {
            init(contextMenu);
            const event = new CustomEvent('Show', {
                detail: {
                    pageX: e.pageX,
                    pageY: e.pageY,
                }
            });
            contextMenu.dispatchEvent(event);
            // disable click events on table.
            // it's re-enabled when the dropdown is closed.
            table.style.pointerEvents = "none";
        }
        else {
            console.error('contextmenu is not added');
        }
    });
}
function init(menu) {
    htmx.process(menu);
    menu.addEventListener('Show', (e) => {
        let { pageX, pageY, path } = e.detail;
        menu.style.left = `${pageX}px`;
        menu.style.top = `${pageY}px`;
        menu.style.position = 'absolute';
        menu.style.display = 'block';
        menu.dataset.path = path;
    });
    // wait for animation to end
    menu.addEventListener('Close', _ => {
        menu.classList.add('closing');
        menu.addEventListener('animationend', function h() {
            menu.removeEventListener('animationend', h); // Clean up the listener
            menu.remove();
        });
    });
}
