/*
 * Home canvas — renders HLink icons on the spatial Home target canvas.
 */

import * as Workspace from './hdesktop-workspace.js';
import type { HLink } from './hdesktop-workspace.js';

declare var htmx: any;

let canvasEl: HTMLElement | null = null;

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export function isMounted(): boolean {
  return canvasEl !== null;
}

export function mount(el: HTMLElement): void {
  canvasEl = el;
  Workspace.addSwitchListener(refresh);
  render();
}

export function unmount(): void {
  canvasEl = null;
}

export function refresh(): void {
  if (canvasEl) render();
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

function render(): void {
  if (!canvasEl) return;
  canvasEl.innerHTML = '';
  for (const lnk of Workspace.getHLinks()) {
    canvasEl.appendChild(makeIcon(lnk));
  }
}

function makeIcon(lnk: HLink): HTMLElement {
  const el = document.createElement('div');
  el.className = 'hlink-icon';
  el.dataset.hlinkId = lnk.id;
  el.style.left = lnk.x + 'px';
  el.style.top  = lnk.y + 'px';

  const previewEl = makePreview(lnk);

  const labelEl = document.createElement('span');
  labelEl.className = 'hlink-label';
  labelEl.textContent = lnk.label ?? basename(lnk.path);

  el.appendChild(previewEl);
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
// Preview element (mirrors the Haskell previewElement template)
// ---------------------------------------------------------------------------

function makePreview(lnk: HLink): HTMLElement {
  const serveUrl = '/serve?file=' + encodeURIComponent(lnk.clientPath);
  const wrapper  = document.createElement('div');
  wrapper.className = 'thumbnail-preview';

  const inner = document.createElement('div');
  inner.className = 'image-wrapper';

  if (lnk.mime.startsWith('image/')) {
    const img = document.createElement('img');
    img.src              = serveUrl;
    img.draggable        = false;
    img.loading          = 'lazy';
    img.setAttribute('fetchpriority', 'low');
    inner.appendChild(img);
  } else if (lnk.mime.startsWith('video/')) {
    const vid = document.createElement('video');
    vid.src      = serveUrl;
    vid.muted    = true;
    vid.loop     = true;
    vid.setAttribute('preload', 'metadata');
    vid.setAttribute('playsinline', '');
    vid.className = 'thumbnail-video';
    vid.draggable = false;
    inner.appendChild(vid);
  } else {
    const icon = document.createElement('i');
    icon.className = 'bx ' + fileIcon(lnk.path);
    inner.appendChild(icon);
  }

  wrapper.appendChild(inner);
  return wrapper;
}

// ---------------------------------------------------------------------------
// Drag
// ---------------------------------------------------------------------------

function attachDrag(el: HTMLElement, lnk: HLink): void {
  let dragging = false;
  let startX = 0, startY = 0, origX = 0, origY = 0;

  el.addEventListener('pointerdown', (e) => {
    if (e.button !== 0) return;
    dragging = true;
    startX = e.clientX; startY = e.clientY;
    origX = lnk.x;      origY = lnk.y;
    el.setPointerCapture(e.pointerId);
    el.classList.add('dragging');
    e.preventDefault();
  });

  el.addEventListener('pointermove', (e) => {
    if (!dragging) return;
    const dx = e.clientX - startX;
    const dy = e.clientY - startY;
    el.style.left = (origX + dx) + 'px';
    el.style.top  = (origY + dy) + 'px';
  });

  el.addEventListener('pointerup', (e) => {
    if (!dragging) return;
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

function openHLink(lnk: HLink): void {
  const tab = document.querySelector<HTMLElement>(`.target-tab[hx-get*="target=${encodeURIComponent(lnk.targetId)}"]`);
  if (tab) {
    htmx.trigger(tab, 'click');
  }
}

// ---------------------------------------------------------------------------
// Icon context menu
// ---------------------------------------------------------------------------

function showIconMenu(lnk: HLink, clientX: number, clientY: number): void {
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
    if (rect.right  > window.innerWidth)  menu.style.left = (clientX - rect.width)  + 'px';
    if (rect.bottom > window.innerHeight) menu.style.top  = (clientY - rect.height) + 'px';
  });

  const close = (e: MouseEvent) => {
    if (!menu.contains(e.target as Node)) {
      menu.remove();
      document.removeEventListener('click', close, true);
    }
  };
  document.addEventListener('click', close, true);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function basename(path: string): string {
  return path.split('/').filter(Boolean).pop() ?? path;
}

function fileIcon(path: string): string {
  const ext = path.split('.').pop()?.toLowerCase() ?? '';
  if (['jpg','jpeg','png','gif','webp','svg','bmp'].includes(ext)) return 'bxs-image';
  if (['mp4','webm','mkv','mov','avi'].includes(ext))               return 'bx-movie';
  if (['mp3','ogg','flac','wav','aac'].includes(ext))               return 'bxs-music';
  if (['pdf'].includes(ext))                                        return 'bxs-file-pdf';
  if (['zip','tar','gz','bz2','xz','7z'].includes(ext))             return 'bxs-archive';
  if (['hs','ts','js','py','rs','go','c','cpp','java'].includes(ext)) return 'bxs-file-js';
  return 'bxs-file-blank';
}
