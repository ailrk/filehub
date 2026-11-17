/*
 * Desktop right-click context menu.
 * Registered once on document — no re-registration after htmx swaps.
 */
let currentController = null;
export function register() {
    document.addEventListener('contextmenu', onContextMenu);
}
function onContextMenu(e) {
    e.preventDefault();
    const table = document.getElementById('table');
    if (!table?.contains(e.target))
        return;
    const item = e.target.closest('.table-item');
    if (!item)
        return;
    document.getElementById('contextmenu')?.remove();
    const params = new URLSearchParams();
    if (item.classList.contains('selected')) {
        document.querySelectorAll('.table-item.selected').forEach(sel => {
            if (sel.dataset.path)
                params.append('file', sel.dataset.path);
        });
    }
    else {
        if (item.dataset.path)
            params.append('file', item.dataset.path);
    }
    fetchWithCancel(`/contextmenu?${params.toString()}`)
        .then(res => res.text())
        .then(html => {
        const tableEl = document.getElementById('table');
        if (!tableEl)
            return;
        tableEl.insertAdjacentHTML('afterend', html);
        const menu = document.getElementById('contextmenu');
        if (!menu)
            return;
        initMenu(menu, e.pageX, e.pageY);
        tableEl.style.pointerEvents = 'none';
    })
        .catch(() => { });
}
function initMenu(menu, pageX, pageY) {
    htmx.process(menu);
    // Clamp to viewport so menu never goes off-screen
    menu.style.position = 'absolute';
    menu.style.display = 'block';
    menu.style.left = pageX + 'px';
    menu.style.top = pageY + 'px';
    requestAnimationFrame(() => {
        const rect = menu.getBoundingClientRect();
        if (rect.right > window.innerWidth)
            menu.style.left = (pageX - rect.width) + 'px';
        if (rect.bottom > window.innerHeight)
            menu.style.top = (pageY - rect.height) + 'px';
    });
    menu.addEventListener('Close', () => {
        menu.classList.add('closing');
        menu.addEventListener('animationend', () => menu.remove(), { once: true });
    });
}
async function fetchWithCancel(url, options = {}) {
    currentController?.abort();
    currentController = new AbortController();
    const response = await fetch(url, { ...options, signal: currentController.signal });
    if (!response.ok)
        throw new Error(`HTTP ${response.status}`);
    return response;
}
