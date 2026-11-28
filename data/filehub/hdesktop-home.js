/*
 * Home canvas — renders HLink icons on the spatial Home target canvas.
 */
import * as Workspace from './hdesktop-workspace.js';
import { makeMenu, makeClickItem, initMenuFixed } from './hdesktop-contextmenu.js';
import * as Modal from './modal.js';
import Viewer from './viewer.js';
let canvasEl = null;
// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------
export function isMounted() {
    return canvasEl !== null;
}
export function mount(el) {
    canvasEl = el;
    Workspace.addSwitchListener(refresh);
    render();
}
export function unmount() {
    canvasEl = null;
}
export function refresh() {
    if (canvasEl)
        render();
}
// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------
function render() {
    if (!canvasEl)
        return;
    canvasEl.innerHTML = '';
    for (const lnk of Workspace.getHLinks()) {
        canvasEl.appendChild(makeIcon(lnk));
    }
}
function makeIcon(lnk) {
    const el = document.createElement('div');
    el.className = 'hlink-icon';
    el.dataset.hlinkId = lnk.id;
    el.style.left = lnk.x + 'px';
    el.style.top = lnk.y + 'px';
    el.appendChild(makePreview(lnk));
    const labelEl = document.createElement('span');
    labelEl.className = 'hlink-label';
    labelEl.textContent = lnk.label ?? basename(lnk.path);
    el.appendChild(labelEl);
    el.addEventListener('contextmenu', (e) => {
        e.preventDefault();
        e.stopPropagation();
        showHLinkMenu(lnk, e.clientX, e.clientY);
    });
    attachDrag(el, lnk, () => openHLink(lnk));
    return el;
}
// ---------------------------------------------------------------------------
// Preview element (mirrors the Haskell previewElement template)
// ---------------------------------------------------------------------------
function makePreview(lnk) {
    const serveUrl = '/serve?file=' + encodeURIComponent(lnk.clientPath) + '&target=' + encodeURIComponent(lnk.targetId);
    const wrapper = document.createElement('div');
    wrapper.className = 'thumbnail-preview';
    const inner = document.createElement('div');
    inner.className = 'image-wrapper';
    if (lnk.mime.startsWith('image/')) {
        const img = document.createElement('img');
        img.src = serveUrl;
        img.draggable = false;
        img.loading = 'lazy';
        img.setAttribute('fetchpriority', 'low');
        inner.appendChild(img);
    }
    else if (lnk.mime.startsWith('video/')) {
        const vid = document.createElement('video');
        vid.src = serveUrl;
        vid.muted = true;
        vid.loop = true;
        vid.setAttribute('preload', 'metadata');
        vid.setAttribute('playsinline', '');
        vid.className = 'thumbnail-video';
        vid.draggable = false;
        inner.appendChild(vid);
    }
    else {
        const icon = document.createElement('i');
        icon.className = lnk.icon;
        inner.appendChild(icon);
    }
    wrapper.appendChild(inner);
    return wrapper;
}
// ---------------------------------------------------------------------------
// Drag — onClick fires only when pointer didn't move (not a drag)
// ---------------------------------------------------------------------------
function attachDrag(el, lnk, onClick) {
    let active = false;
    let didMove = false;
    let startX = 0, startY = 0, origX = 0, origY = 0;
    el.addEventListener('pointerdown', (e) => {
        if (e.button !== 0)
            return;
        active = true;
        didMove = false;
        startX = e.clientX;
        startY = e.clientY;
        origX = lnk.x;
        origY = lnk.y;
        el.setPointerCapture(e.pointerId);
        e.preventDefault();
    });
    el.addEventListener('pointermove', (e) => {
        if (!active)
            return;
        const dx = e.clientX - startX;
        const dy = e.clientY - startY;
        if (Math.abs(dx) + Math.abs(dy) > 5) {
            didMove = true;
            el.classList.add('dragging');
        }
        if (didMove) {
            el.style.left = (origX + dx) + 'px';
            el.style.top = (origY + dy) + 'px';
        }
    });
    el.addEventListener('pointerup', (e) => {
        if (!active)
            return;
        active = false;
        el.classList.remove('dragging');
        if (didMove) {
            const dx = e.clientX - startX;
            const dy = e.clientY - startY;
            Workspace.moveHLink(lnk.id, origX + dx, origY + dy);
            lnk.x = origX + dx;
            lnk.y = origY + dy;
        }
        else {
            onClick();
        }
    });
}
// ---------------------------------------------------------------------------
// Open: behaves like clicking the file in storage
// ---------------------------------------------------------------------------
function openHLink(lnk) {
    const enc = encodeURIComponent(lnk.clientPath);
    const targetEnc = encodeURIComponent(lnk.targetId);
    const serveUrl = `/serve?file=${enc}&target=${targetEnc}`;
    if (lnk.mime === 'inode/directory') {
        goToSource(lnk);
        return;
    }
    if (lnk.mime.startsWith('image/') || lnk.mime.startsWith('video/') || lnk.mime.startsWith('audio/')) {
        new Viewer(lnk.clientPath, serveUrl, lnk.mime).show();
        return;
    }
    if (lnk.mime === 'application/pdf') {
        window.open(serveUrl, '_blank');
        return;
    }
    htmx.ajax('GET', `/modal/editor?file=${enc}&target=${targetEnc}`, { target: '#index', swap: 'beforeend' });
}
// ---------------------------------------------------------------------------
// Go to source: navigate to the file's storage location (no viewer)
// ---------------------------------------------------------------------------
function goToSource(lnk) {
    const tab = findTargetTab(lnk.targetId);
    if (!tab)
        return;
    const isCurrentTarget = tab.classList.contains('current-target');
    const targetDir = lnk.mime === 'inode/directory' ? lnk.clientPath : dirPath(lnk.clientPath);
    const doCD = () => fireHtmxGet('/cd?dir=' + encodeURIComponent(targetDir));
    if (isCurrentTarget) {
        doCD();
    }
    else {
        document.body.addEventListener('htmx:afterSettle', doCD, { once: true });
        htmx.trigger(tab, 'click');
    }
}
// ---------------------------------------------------------------------------
// Context menu
// ---------------------------------------------------------------------------
function showHLinkMenu(lnk, clientX, clientY) {
    document.getElementById('contextmenu')?.remove();
    const menu = makeMenu();
    const enc = encodeURIComponent(lnk.clientPath);
    menu.appendChild(makeClickItem('Go to source', 'bxs-navigation', () => goToSource(lnk)));
    const dl = document.createElement('a');
    dl.className = 'dropdown-item';
    dl.href = '/download?file=' + enc;
    dl.innerHTML = `<i class="bx bx-download"></i><span>Download</span>`;
    menu.appendChild(dl);
    menu.appendChild(makeClickItem('Share', 'bxs-share-alt', () => {
        Modal.showShareModal(lnk.clientPath);
    }));
    menu.appendChild(makeClickItem('Details', 'bx-detail', () => {
        Modal.showFileDetailModal(lnk.label ?? basename(lnk.path), lnk.mtime, lnk.atime, lnk.size, lnk.mime);
    }));
    menu.appendChild(document.createElement('hr'));
    menu.appendChild(makeClickItem('Rename label', 'bxs-rename', () => {
        const newLabel = prompt('Label', lnk.label ?? basename(lnk.path));
        if (newLabel !== null) {
            Workspace.relabelHLink(lnk.id, newLabel || null);
            render();
        }
    }));
    menu.appendChild(makeClickItem('Remove from Home', 'bx-trash', () => {
        Workspace.removeHLink(lnk.id);
        render();
    }));
    document.body.appendChild(menu);
    initMenuFixed(menu, clientX, clientY);
}
// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------
function findTargetTab(targetId) {
    for (const tab of document.querySelectorAll('.target-tab')) {
        const hxGet = tab.getAttribute('hx-get') ?? '';
        const tid = new URLSearchParams(hxGet.split('?')[1] ?? '').get('target');
        if (tid === targetId)
            return tab;
    }
    return null;
}
function fireHtmxGet(url) {
    const el = document.createElement('div');
    el.setAttribute('hx-get', url);
    el.setAttribute('hx-swap', 'none');
    document.body.appendChild(el);
    htmx.process(el);
    htmx.trigger(el, 'click');
    requestAnimationFrame(() => el.remove());
}
function dirPath(clientPath) {
    const parts = clientPath.split('/').filter(Boolean);
    parts.pop();
    return '/' + parts.join('/');
}
function basename(path) {
    return path.split('/').filter(Boolean).pop() ?? path;
}
