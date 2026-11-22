import { i18n } from '../../i18n.js';
import * as ModalBuilder from '../../modal-builder.js';
import Viewer from '../../viewer.js';
function openViewer(path) {
    const v = new Viewer(path);
    v.show();
}
export function register() {
    document.addEventListener('contextmenu', onContextMenu);
}
function onContextMenu(e) {
    e.preventDefault();
    document.getElementById('contextmenu')?.remove();
    const sidebar = document.getElementById('side-bar');
    if (sidebar?.contains(e.target)) {
        const tab = e.target.closest('.target-tab');
        if (!tab)
            return;
        const menu = buildMenuSidebar(tab);
        document.body.appendChild(menu);
        initMenuFixed(menu, e.clientX, e.clientY);
        return;
    }
    const view = document.getElementById('view');
    const table = document.getElementById('table');
    if (!view?.contains(e.target))
        return;
    if (!table)
        return;
    const item = e.target.closest('.table-item');
    if (!item) {
        if (table.hasAttribute('data-readonly'))
            return;
        const menu = buildMenuBackground();
        table.insertAdjacentElement('afterend', menu);
        initMenu(menu, e.pageX, e.pageY);
        table.style.pointerEvents = 'none';
        return;
    }
    const readOnly = table.hasAttribute('data-readonly');
    let menu;
    if (item.classList.contains('selected')) {
        const paths = Array.from(document.querySelectorAll('.table-item.selected'))
            .map(el => el.dataset.path)
            .filter(Boolean);
        menu = buildMenuMany(paths, readOnly);
    }
    else {
        menu = buildMenu1(item, readOnly);
    }
    table.insertAdjacentElement('afterend', menu);
    initMenu(menu, e.pageX, e.pageY);
    table.style.pointerEvents = 'none';
}
function buildMenu1(item, readOnly) {
    const t = i18n();
    const path = item.dataset.path;
    const ftype = item.dataset.ftype;
    const enc = encodeURIComponent(path);
    const menu = makeMenu();
    if (ftype === 'dir') {
        menu.appendChild(makeItem(t.open, 'bxs-folder-open', {
            'hx-get': `/cd?dir=${enc}`, 'hx-swap': 'none',
        }));
    }
    else if (ftype === 'image') {
        menu.appendChild(makeClickItem(t.view, 'bx-show', () => openViewer(path)));
    }
    else if (ftype === 'video' || ftype === 'audio') {
        menu.appendChild(makeClickItem(t.play, 'bx-play', () => openViewer(path)));
    }
    else if (ftype === 'pdf') {
        menu.appendChild(makeClickItem(t.view, 'bx-show', () => window.open(`/serve?file=${enc}`, '_blank')));
    }
    else {
        menu.appendChild(makeItem(t.edit, 'bxs-edit', {
            'hx-get': `/modal/editor?file=${enc}`,
            'hx-target': '#index', 'hx-swap': 'beforeend', 'hx-indicator': '#global-spinner',
        }));
    }
    menu.appendChild(makeItem(t.copy, 'bxs-copy-alt', {
        'hx-get': `/files/copy1?file=${enc}`, 'hx-target': '#control-panel', 'hx-swap': 'outerHTML',
    }));
    if (!readOnly) {
        menu.appendChild(makeItem(t.cut, 'bx-cut', {
            'hx-get': `/files/cut1?file=${enc}`, 'hx-target': '#control-panel', 'hx-swap': 'outerHTML',
        }));
    }
    const dl = document.createElement('a');
    dl.className = 'dropdown-item';
    dl.href = `/download?file=${enc}`;
    dl.innerHTML = `<i class="bx bx-download"></i><span>${t.download}</span>`;
    menu.appendChild(dl);
    if (!readOnly) {
        const renameEl = makeClickItem(t.rename, 'bxs-rename', () => {
            ModalBuilder.showRenameModal(path);
        });
        menu.appendChild(renameEl);
        menu.appendChild(makeItem(t.delete, 'bxs-trash', {
            'hx-delete': `/files/delete?file=${enc}`,
            'hx-swap': 'none', 'hx-confirm': t.confirmDelete1.replace('{}', path),
        }));
    }
    const shareEl = makeClickItem(t.share, 'bxs-share-alt', () => {
        ModalBuilder.showShareModal(path);
    });
    menu.appendChild(shareEl);
    const detailEl = makeClickItem(t.details, 'bx-detail', () => {
        ModalBuilder.showFileDetailModal(item.dataset.name ?? '', item.dataset.mtime ?? '', item.dataset.atime ?? '', item.dataset.size ?? '0', item.dataset.mime ?? '');
    });
    menu.appendChild(detailEl);
    const pluginItems = buildPluginItems(path, item.dataset.name ?? '', item.dataset.mime ?? '');
    if (pluginItems.length > 0) {
        menu.appendChild(document.createElement('hr'));
        for (const el of pluginItems)
            menu.appendChild(el);
    }
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
            'hx-get': `/files/copy?${fileParams}`, 'hx-target': '#control-panel', 'hx-swap': 'outerHTML',
        }));
        menu.appendChild(makeItem(t.cut, 'bx-cut', {
            'hx-get': `/files/cut?${fileParams}`, 'hx-target': '#control-panel', 'hx-swap': 'outerHTML',
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
function buildMenuBackground() {
    const t = i18n();
    const menu = makeMenu();
    menu.appendChild(makeClickItem(t.new_folder, 'bxs-folder-plus', () => {
        ModalBuilder.showNewFolderModal();
    }));
    menu.appendChild(makeClickItem(t.new_file, 'bxs-file-plus', () => {
        ModalBuilder.showNewFileModal();
    }));
    menu.appendChild(makeClickItem(t.upload, 'bx-upload', () => {
        document.getElementById('file-input')?.click();
    }));
    const hasPaste = !!document.querySelector('#control-panel [hx-post*="paste"]');
    if (hasPaste) {
        menu.appendChild(document.createElement('hr'));
        menu.appendChild(makeItem(t.paste, 'bxs-paste', {
            'hx-post': '/files/paste', 'hx-swap': 'none',
        }));
    }
    htmx.process(menu);
    return menu;
}
function buildMenuSidebar(tab) {
    const menu = makeMenu();
    const type = tab.dataset.targetType ?? '';
    if (type === 's3') {
        const bucket = tab.dataset.targetName ?? '';
        const copyEl = document.createElement('div');
        copyEl.className = 'dropdown-item';
        copyEl.innerHTML = `<i class="bx bxs-copy"></i><span>Copy bucket name</span>`;
        copyEl.addEventListener('click', () => {
            navigator.clipboard.writeText(bucket).catch(() => { });
            menu.dispatchEvent(new Event('Close'));
        });
        menu.appendChild(copyEl);
    }
    const hxGet = tab.getAttribute('hx-get') ?? '';
    const targetId = new URLSearchParams(hxGet.split('?')[1] ?? '').get('target');
    const url = targetId ? `/?target=${encodeURIComponent(targetId)}` : '/';
    const openEl = document.createElement('div');
    openEl.className = 'dropdown-item';
    openEl.innerHTML = `<i class="bx bx-link-external"></i><span>Open in new tab</span>`;
    openEl.addEventListener('click', () => {
        window.open(url, '_blank');
        menu.dispatchEvent(new Event('Close'));
    });
    menu.appendChild(openEl);
    return menu;
}
function buildPluginItems(filePath, fileName, fileMime) {
    const plugins = (typeof filehubPlugins !== 'undefined' && filehubPlugins) ? filehubPlugins : [];
    const items = [];
    for (const plugin of plugins) {
        for (const entry of plugin.contextMenu) {
            if (!fileMime.startsWith(entry.mime))
                continue;
            const needsShare = entry.url.includes('{share_url}');
            const el = makeClickItem(entry.label, 'bx-link-external', () => {
                if (needsShare) {
                    openWithShare(filePath, fileName, fileMime, entry.url);
                }
                else {
                    const url = substituteUrl(entry.url, filePath, fileName, fileMime, '');
                    window.open(url, '_blank');
                }
            });
            items.push(el);
        }
    }
    return items;
}
async function openWithShare(filePath, fileName, fileMime, urlTemplate) {
    const enc = encodeURIComponent(filePath);
    // Check if a no-passcode share link already exists for this file.
    const lookupRes = await fetch(`/share/for-file?file=${enc}`);
    let hash;
    if (lookupRes.ok) {
        hash = await lookupRes.text();
    }
    else {
        // No existing link — ask the user before creating one.
        if (!confirm(`"${fileName}" is not shared. Create a public share link to open it in this app?`))
            return;
        const createRes = await fetch(`/share/auto?file=${enc}`, { method: 'POST' });
        if (!createRes.ok) {
            alert('Failed to create share link.');
            return;
        }
        hash = await createRes.text();
    }
    const shareUrl = `${window.location.origin}/s/${hash}/file?file=${enc}`;
    const url = substituteUrl(urlTemplate, filePath, fileName, fileMime, shareUrl);
    window.open(url, '_blank');
}
function substituteUrl(template, filePath, fileName, fileMime, shareUrl) {
    const downloadUrl = `${window.location.origin}/download?file=${encodeURIComponent(filePath)}`;
    return template
        .replace('{file_path}', encodeURIComponent(filePath))
        .replace('{file_name}', encodeURIComponent(fileName))
        .replace('{mime_type}', encodeURIComponent(fileMime))
        .replace('{download_url}', encodeURIComponent(downloadUrl))
        .replace('{share_url}', encodeURIComponent(shareUrl));
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
function makeClickItem(label, icon, onClick) {
    const el = document.createElement('div');
    el.className = 'dropdown-item';
    el.innerHTML = `<i class="bx ${icon}"></i><span>${label}</span>`;
    el.addEventListener('click', () => {
        document.getElementById('contextmenu')?.dispatchEvent(new Event('Close'));
        const table = document.getElementById('table');
        if (table)
            table.style.pointerEvents = 'auto';
        onClick();
    });
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
function initMenuFixed(menu, clientX, clientY) {
    menu.style.position = 'fixed';
    menu.style.display = 'block';
    menu.style.left = clientX + 'px';
    menu.style.top = clientY + 'px';
    menu.style.zIndex = '500';
    requestAnimationFrame(() => {
        const rect = menu.getBoundingClientRect();
        if (rect.right > window.innerWidth)
            menu.style.left = (clientX - rect.width) + 'px';
        if (rect.bottom > window.innerHeight)
            menu.style.top = (clientY - rect.height) + 'px';
    });
    menu.addEventListener('Close', () => {
        menu.classList.add('closing');
        menu.addEventListener('animationend', () => menu.remove(), { once: true });
    });
}
