export function register() {
    register1();
    document.body.addEventListener('htmx:afterSwap', _ => {
        register1();
    });
}
function register1() {
    document.addEventListener('contextmenu', e => e.preventDefault());
    document.querySelectorAll('#table .table-item').forEach(item => item.addEventListener('contextmenu', e => handle(item, e)));
}
/* 1. fetch contextmenu for the current file
 * 2. show the context menu at the cursor position
 * 3. disable 'click' event handler on the table
 * NOTE: we restore the click event handler in closeDropdown.ts.
 * */
export function handle(item, e) {
    const path = item.dataset.path;
    let contextMenu = document.querySelector('#contextmenu');
    if (contextMenu) {
        contextMenu.remove();
    }
    fetch(`/contextmenu?file=${path}`)
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
                    path
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
