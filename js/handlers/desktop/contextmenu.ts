declare var htmx: any;

import { i18n } from '../../i18n.js';
import * as ModalBuilder from '../../modal-builder.js';

export function register(): void {
  document.addEventListener('contextmenu', onContextMenu);
}

function onContextMenu(e: MouseEvent): void {
  e.preventDefault();

  const view  = document.getElementById('view');
  const table = document.getElementById('table');
  if (!view?.contains(e.target as Node)) return;

  document.getElementById('contextmenu')?.remove();

  if (!table) return;

  const item = (e.target as Element).closest<HTMLElement>('.table-item');
  if (!item) {
    if (table.hasAttribute('data-readonly')) return;
    const menu = buildMenuBackground();
    table.insertAdjacentElement('afterend', menu);
    initMenu(menu, e.pageX, e.pageY);
    table.style.pointerEvents = 'none';
    return;
  }

  const readOnly = table.hasAttribute('data-readonly');
  let menu: HTMLElement;

  if (item.classList.contains('selected')) {
    const paths = Array.from(document.querySelectorAll<HTMLElement>('.table-item.selected'))
      .map(el => el.dataset.path)
      .filter(Boolean) as string[];
    menu = buildMenuMany(paths, readOnly);
  } else {
    menu = buildMenu1(item, readOnly);
  }

  table.insertAdjacentElement('afterend', menu);
  initMenu(menu, e.pageX, e.pageY);
  table.style.pointerEvents = 'none';
}


function buildMenu1(item: HTMLElement, readOnly: boolean): HTMLElement {
  const t   = i18n();
  const path = item.dataset.path!;
  const ftype = item.dataset.ftype!;
  const enc = encodeURIComponent(path);
  const menu = makeMenu();

  if (ftype === 'dir') {
    menu.appendChild(makeItem(t.open, 'bxs-folder-open', {
      'hx-get': `/cd?dir=${enc}`, 'hx-swap': 'none',
    }));
  } else if (ftype === 'image') {
    menu.appendChild(makeItem(t.view, 'bx-show', {
      'hx-get': `/files/open?target=OpenViewer&file=${enc}`,
      'hx-swap': 'none', 'hx-indicator': '#global-spinner',
    }));
  } else if (ftype === 'video' || ftype === 'audio') {
    menu.appendChild(makeItem(t.play, 'bx-play', {
      'hx-get': `/files/open?target=OpenViewer&file=${enc}`,
      'hx-swap': 'none', 'hx-indicator': '#global-spinner',
    }));
  } else if (ftype === 'pdf') {
    menu.appendChild(makeItem(t.view, 'bx-show', {
      'hx-get': `/files/open?target=OpenDOMBlank&file=${enc}`,
      'hx-swap': 'none', 'hx-indicator': '#global-spinner',
    }));
  } else {
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
    const renameEl = makeClickItem(t.rename, 'bxs-rename', () => {
      ModalBuilder.showRenameModal(path);
    });
    menu.appendChild(renameEl);

    menu.appendChild(makeItem(t.delete, 'bxs-trash', {
      'hx-delete': `/files/delete?file=${enc}`,
      'hx-swap': 'none', 'hx-confirm': t.confirmDelete1.replace('{}', path),
    }));
  }

  const detailEl = makeClickItem(t.details, 'bx-detail', () => {
    ModalBuilder.showFileDetailModal(
      item.dataset.name ?? '',
      item.dataset.mtime ?? '',
      item.dataset.atime ?? '',
      item.dataset.size ?? '0',
      item.dataset.mime ?? '',
    );
  });
  menu.appendChild(detailEl);

  htmx.process(menu);
  return menu;
}


function buildMenuMany(paths: string[], readOnly: boolean): HTMLElement {
  const t    = i18n();
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


function buildMenuBackground(): HTMLElement {
  const t    = i18n();
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


function makeMenu(): HTMLElement {
  const el = document.createElement('div');
  el.className = 'dropdown-content';
  el.id = 'contextmenu';
  return el;
}


function makeItem(label: string, icon: string, attrs: Record<string, string>): HTMLElement {
  const el = document.createElement('div');
  el.className = 'dropdown-item';
  el.innerHTML = `<i class="bx ${icon}"></i><span>${label}</span>`;
  for (const [k, v] of Object.entries(attrs)) el.setAttribute(k, v);
  return el;
}


function makeClickItem(label: string, icon: string, onClick: () => void): HTMLElement {
  const el = document.createElement('div');
  el.className = 'dropdown-item';
  el.innerHTML = `<i class="bx ${icon}"></i><span>${label}</span>`;
  el.addEventListener('click', () => {
    document.getElementById('contextmenu')?.dispatchEvent(new Event('Close'));
    const table = document.getElementById('table');
    if (table) table.style.pointerEvents = 'auto';
    onClick();
  });
  return el;
}


function initMenu(menu: HTMLElement, pageX: number, pageY: number): void {
  menu.style.position = 'absolute';
  menu.style.display  = 'block';
  menu.style.left = pageX + 'px';
  menu.style.top  = pageY + 'px';

  requestAnimationFrame(() => {
    const rect = menu.getBoundingClientRect();
    if (rect.right  > window.innerWidth)  menu.style.left = (pageX - rect.width)  + 'px';
    if (rect.bottom > window.innerHeight) menu.style.top  = (pageY - rect.height) + 'px';
  });

  menu.addEventListener('Close', () => {
    menu.classList.add('closing');
    menu.addEventListener('animationend', () => menu.remove(), { once: true });
  });
}
