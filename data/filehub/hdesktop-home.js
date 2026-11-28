/*
 * Home canvas — renders HLink icons on the spatial Home target canvas.
 */
import * as Workspace from './hdesktop-workspace.js';
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
    const iconEl = document.createElement('i');
    iconEl.className = 'bx ' + fileIcon(lnk.path);
    const labelEl = document.createElement('span');
    labelEl.className = 'hlink-label';
    labelEl.textContent = lnk.label ?? basename(lnk.path);
    el.appendChild(iconEl);
    el.appendChild(labelEl);
    el.addEventListener('dblclick', () => openHLink(lnk));
    el.addEventListener('contextmenu', (e) => {
        e.preventDefault();
        e.stopPropagation();
        showIconMenu(lnk, e.clientX, e.clientY);
    });
    attachDrag(el, lnk);
    return el;
}
// ---------------------------------------------------------------------------
// Drag
// ---------------------------------------------------------------------------
function attachDrag(el, lnk) {
    let dragging = false;
    let startX = 0, startY = 0, origX = 0, origY = 0;
    el.addEventListener('pointerdown', (e) => {
        if (e.button !== 0)
            return;
        dragging = true;
        startX = e.clientX;
        startY = e.clientY;
        origX = lnk.x;
        origY = lnk.y;
        el.setPointerCapture(e.pointerId);
        el.classList.add('dragging');
        e.preventDefault();
    });
    el.addEventListener('pointermove', (e) => {
        if (!dragging)
            return;
        const dx = e.clientX - startX;
        const dy = e.clientY - startY;
        el.style.left = (origX + dx) + 'px';
        el.style.top = (origY + dy) + 'px';
    });
    el.addEventListener('pointerup', (e) => {
        if (!dragging)
            return;
        dragging = false;
        el.classList.remove('dragging');
        const dx = e.clientX - startX;
        const dy = e.clientY - startY;
        Workspace.moveHLink(lnk.id, origX + dx, origY + dy);
        lnk.x = origX + dx;
        lnk.y = origY + dy;
    });
}
// ---------------------------------------------------------------------------
// Open (navigate to target)
// ---------------------------------------------------------------------------
function openHLink(lnk) {
    const tab = document.querySelector(`.target-tab[hx-get*="target=${encodeURIComponent(lnk.targetId)}"]`);
    if (tab) {
        htmx.trigger(tab, 'click');
    }
}
// ---------------------------------------------------------------------------
// Icon context menu
// ---------------------------------------------------------------------------
function showIconMenu(lnk, clientX, clientY) {
    document.getElementById('hlink-menu')?.remove();
    const menu = document.createElement('div');
    menu.id = 'hlink-menu';
    menu.className = 'dropdown-content';
    menu.style.cssText = `position:fixed;display:block;left:${clientX}px;top:${clientY}px;z-index:600`;
    const removeEl = document.createElement('div');
    removeEl.className = 'dropdown-item';
    removeEl.innerHTML = `<i class="bx bx-trash"></i><span>Remove</span>`;
    removeEl.addEventListener('click', () => {
        Workspace.removeHLink(lnk.id);
        menu.remove();
        render();
    });
    const renameEl = document.createElement('div');
    renameEl.className = 'dropdown-item';
    renameEl.innerHTML = `<i class="bx bxs-rename"></i><span>Rename</span>`;
    renameEl.addEventListener('click', () => {
        menu.remove();
        const newLabel = prompt('Label', lnk.label ?? basename(lnk.path));
        if (newLabel !== null) {
            Workspace.relabelHLink(lnk.id, newLabel || null);
            render();
        }
    });
    menu.appendChild(renameEl);
    menu.appendChild(removeEl);
    document.body.appendChild(menu);
    requestAnimationFrame(() => {
        const rect = menu.getBoundingClientRect();
        if (rect.right > window.innerWidth)
            menu.style.left = (clientX - rect.width) + 'px';
        if (rect.bottom > window.innerHeight)
            menu.style.top = (clientY - rect.height) + 'px';
    });
    const close = (e) => {
        if (!menu.contains(e.target)) {
            menu.remove();
            document.removeEventListener('click', close, true);
        }
    };
    document.addEventListener('click', close, true);
}
// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------
function basename(path) {
    return path.split('/').filter(Boolean).pop() ?? path;
}
function fileIcon(path) {
    const ext = path.split('.').pop()?.toLowerCase() ?? '';
    if (['jpg', 'jpeg', 'png', 'gif', 'webp', 'svg', 'bmp'].includes(ext))
        return 'bxs-image';
    if (['mp4', 'webm', 'mkv', 'mov', 'avi'].includes(ext))
        return 'bx-movie';
    if (['mp3', 'ogg', 'flac', 'wav', 'aac'].includes(ext))
        return 'bxs-music';
    if (['pdf'].includes(ext))
        return 'bxs-file-pdf';
    if (['zip', 'tar', 'gz', 'bz2', 'xz', '7z'].includes(ext))
        return 'bxs-archive';
    if (['hs', 'ts', 'js', 'py', 'rs', 'go', 'c', 'cpp', 'java'].includes(ext))
        return 'bxs-file-js';
    return 'bxs-file-blank';
}
