const defaultI18n = {
    delete: 'Delete', rename: 'Rename', copy: 'Copy', download: 'Download',
    details: 'Details', edit: 'Edit', view: 'View', play: 'Play', open: 'Open',
    cancel: 'Cancel', selected: 'selected',
    confirmDelete1: 'Delete {}?', confirmDeleteLocal: 'Delete {} items?',
    readOnly: false,
};
function i18n() { return window.filehubI18n ?? defaultI18n; }
/*
 * Desktop right-click context menu — built client-side from data-ftype/data-path
 * on each .table-item. No server round-trip needed.
 */
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
    const readOnly = table.hasAttribute('data-readonly');
    let menu;
    if (item.classList.contains('selected')) {
        const paths = Array.from(document.querySelectorAll('.table-item.selected'))
            .map(el => el.dataset.path)
            .filter(Boolean);
        menu = buildMenuMany(paths, readOnly);
    }
    else {
        menu = buildMenu1(item.dataset.path, item.dataset.ftype, readOnly);
    }
    table.insertAdjacentElement('afterend', menu);
    initMenu(menu, e.pageX, e.pageY);
    table.style.pointerEvents = 'none';
}
function buildMenu1(path, ftype, readOnly) {
    const t = i18n();
    const enc = encodeURIComponent(path);
    const menu = makeMenu();
    if (ftype === 'dir') {
        menu.appendChild(makeItem(t.open, 'bxs-folder-open', {
            'hx-get': `/cd?dir=${enc}`, 'hx-swap': 'none',
        }));
    }
    else if (ftype === 'image') {
        menu.appendChild(makeItem(t.view, 'bx-show', {
            'hx-get': `/files/open?target=OpenViewer&file=${enc}`,
            'hx-swap': 'none', 'hx-indicator': '#global-spinner',
        }));
    }
    else if (ftype === 'video' || ftype === 'audio') {
        menu.appendChild(makeItem(t.play, 'bx-play', {
            'hx-get': `/files/open?target=OpenViewer&file=${enc}`,
            'hx-swap': 'none', 'hx-indicator': '#global-spinner',
        }));
    }
    else if (ftype === 'pdf') {
        menu.appendChild(makeItem(t.view, 'bx-show', {
            'hx-get': `/files/open?target=OpenDOMBlank&file=${enc}`,
            'hx-swap': 'none', 'hx-indicator': '#global-spinner',
        }));
    }
    else {
        menu.appendChild(makeItem(t.edit, 'bxs-edit', {
            'hx-get': `/modal/editor?file=${enc}`,
            'hx-target': '#index', 'hx-swap': 'beforeend', 'hx-indicator': '#global-spinner',
        }));
    }
    menu.appendChild(makeItem(t.copy, 'bxs-copy-alt', {
        'hx-get': `/files/copy1?file=${enc}`, 'hx-target': '#index', 'hx-swap': 'outerHTML',
    }));
    const dl = document.createElement('a');
    dl.className = 'dropdown-item';
    dl.href = `/download?file=${enc}`;
    dl.innerHTML = `<i class="bx bx-download"></i><span>${t.download}</span>`;
    menu.appendChild(dl);
    if (!readOnly) {
        menu.appendChild(makeItem(t.rename, 'bxs-rename', {
            'hx-get': `/modal/rename?old=${enc}`, 'hx-target': '#index', 'hx-swap': 'beforeend',
        }));
        menu.appendChild(makeItem(t.delete, 'bxs-trash', {
            'hx-delete': `/files/delete?file=${enc}`,
            'hx-swap': 'none', 'hx-confirm': t.confirmDelete1.replace('{}', path),
        }));
    }
    menu.appendChild(makeItem(t.details, 'bx-detail', {
        'hx-get': `/modal/file/detail?file=${enc}`, 'hx-target': '#index', 'hx-swap': 'beforeend',
    }));
    htmx.process(menu);
    return menu;
}
function buildMenuMany(paths, readOnly) {
    const t = i18n();
    const menu = makeMenu();
    const countEl = document.createElement('div');
    countEl.className = 'dropdown-item no-effect';
    countEl.innerHTML = `<i class="bx bx-select-multiple"></i><span>${paths.length} ${t.selected}</span>`;
    menu.appendChild(countEl);
    menu.appendChild(document.createElement('hr'));
    if (!readOnly) {
        const fileParams = paths.map(p => `file=${encodeURIComponent(p)}`).join('&');
        menu.appendChild(makeItem(t.delete, 'bxs-trash', {
            'hx-delete': `/files/delete?${fileParams}`,
            'hx-swap': 'none', 'hx-confirm': t.confirmDeleteLocal.replace('{}', String(paths.length)),
        }));
        menu.appendChild(makeItem(t.copy, 'bxs-copy-alt', {
            'hx-get': '/files/copy', 'hx-target': '#control-panel', 'hx-swap': 'outerHTML',
        }));
    }
    const fileParams2 = paths.map(p => `file=${encodeURIComponent(p)}`).join('&');
    const dl = document.createElement('a');
    dl.className = 'dropdown-item';
    dl.href = `/download?${fileParams2}`;
    dl.innerHTML = `<i class="bx bx-download"></i><span>${t.download}</span>`;
    menu.appendChild(dl);
    menu.appendChild(makeItem(t.cancel, 'bx-message-alt-x', {
        'hx-post': '/cancel', 'hx-swap': 'none',
    }));
    htmx.process(menu);
    return menu;
}
function makeMenu() {
    const el = document.createElement('div');
    el.className = 'dropdown-content';
    el.id = 'contextmenu';
    return el;
}
function makeItem(label, icon, attrs) {
    const el = document.createElement('div');
    el.className = 'dropdown-item';
    el.innerHTML = `<i class="bx ${icon}"></i><span>${label}</span>`;
    for (const [k, v] of Object.entries(attrs))
        el.setAttribute(k, v);
    return el;
}
function initMenu(menu, pageX, pageY) {
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
