declare var htmx: any;

type EntryAction = { type: 'url'; value: string } | { type: 'script'; value: string };
interface ContextMenuEntry { mime: string; label: string; action: EntryAction; }
interface ToolbarEntry    { mime: string; label: string; action: EntryAction; }
interface FilehubPlugin   { name: string; contextMenu: ContextMenuEntry[]; toolbar: ToolbarEntry[]; }
declare var filehubPlugins: FilehubPlugin[] | undefined;

import { i18n } from './i18n.js';
import * as Modal from './modal.js';
import Viewer from './viewer.js';
import { createPluginUI } from './hdesktop-pluginUI.js';
import * as NodeGraph from './hdesktop-nodeGraph.js';

function openViewer(path: string) {
  const v = new Viewer(path);
  v.show();
}

export function register(): void {
  document.addEventListener('contextmenu', onContextMenu);
}

function onContextMenu(e: MouseEvent): void {
  e.preventDefault();
  document.getElementById('contextmenu')?.remove();

  const sidebar = document.getElementById('side-bar');
  if (sidebar?.contains(e.target as Node)) {
    const tab = (e.target as Element).closest<HTMLElement>('.target-tab');
    if (!tab) return;
    const menu = buildMenuSidebar(tab);
    document.body.appendChild(menu);
    initMenuFixed(menu, e.clientX, e.clientY);
    return;
  }

  const view  = document.getElementById('view');
  const table = document.getElementById('table');
  if (!view?.contains(e.target as Node)) return;

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
    menu.appendChild(makeClickItem(t.view, 'bx-show', () => openViewer(path)));
  } else if (ftype === 'video' || ftype === 'audio') {
    menu.appendChild(makeClickItem(t.play, 'bx-play', () => openViewer(path)));
  } else if (ftype === 'pdf') {
    menu.appendChild(makeClickItem(t.view, 'bx-show', () => window.open(`/serve?file=${enc}`, '_blank')));
  } else {
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
      Modal.showRenameModal(path);
    });
    menu.appendChild(renameEl);

    menu.appendChild(makeItem(t.delete, 'bxs-trash', {
      'hx-delete': `/files/delete?file=${enc}`,
      'hx-swap': 'none', 'hx-confirm': t.confirmDelete1.replace('{}', path),
    }));
  }

  const shareEl = makeClickItem(t.share, 'bxs-share-alt', () => {
    Modal.showShareModal(path);
  });
  menu.appendChild(shareEl);

  const detailEl = makeClickItem(t.details, 'bx-detail', () => {
    Modal.showFileDetailModal(
      item.dataset.name ?? '',
      item.dataset.mtime ?? '',
      item.dataset.atime ?? '',
      item.dataset.size ?? '0',
      item.dataset.mime ?? '',
    );
  });
  menu.appendChild(detailEl);

  const pluginItems = buildPluginItems(path, item.dataset.name ?? '', item.dataset.mime ?? '');
  if (pluginItems.length > 0) {
    menu.appendChild(document.createElement('hr'));
    for (const el of pluginItems) menu.appendChild(el);
  }

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


function buildMenuBackground(): HTMLElement {
  const t    = i18n();
  const menu = makeMenu();

  menu.appendChild(makeClickItem(t.new_folder, 'bxs-folder-plus', () => {
    Modal.showNewFolderModal();
  }));
  menu.appendChild(makeClickItem(t.new_file, 'bxs-file-plus', () => {
    Modal.showNewFileModal();
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


function buildMenuSidebar(tab: HTMLElement): HTMLElement {
  const menu = makeMenu();
  const type = tab.dataset.targetType ?? '';

  if (type === 's3') {
    const bucket = tab.dataset.targetName ?? '';
    const copyEl = document.createElement('div');
    copyEl.className = 'dropdown-item';
    copyEl.innerHTML = `<i class="bx bxs-copy"></i><span>Copy bucket name</span>`;
    copyEl.addEventListener('click', () => {
      navigator.clipboard.writeText(bucket).catch(() => {});
      menu.dispatchEvent(new Event('Close'));
    });
    menu.appendChild(copyEl);
  }

  const hxGet    = tab.getAttribute('hx-get') ?? '';
  const targetId = new URLSearchParams(hxGet.split('?')[1] ?? '').get('target');
  const url      = targetId ? `/?target=${encodeURIComponent(targetId)}` : '/';

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


function buildPluginItems(filePath: string, fileName: string, fileMime: string): HTMLElement[] {
  const items: HTMLElement[] = [];

  const tomlPlugins: FilehubPlugin[] = (typeof filehubPlugins !== 'undefined' && filehubPlugins) ? filehubPlugins : [];
  for (const plugin of tomlPlugins) {
    for (const entry of plugin.contextMenu) {
      if (!fileMime.startsWith(entry.mime)) continue;
      const el = makeClickItem(entry.label, 'bx-link-external', () => {
        if (entry.action.type === 'script') {
          executePluginScript(entry.action.value, filePath, fileName, fileMime, plugin.name);
        } else {
          const needsShare = entry.action.value.includes('{share_url}');
          if (needsShare) {
            openWithShare(filePath, fileName, fileMime, entry.action.value);
          } else {
            const url = substituteUrl(entry.action.value, filePath, fileName, fileMime, '');
            window.open(url, '_blank');
          }
        }
      });
      items.push(el);
    }
  }

  const jsPlugins: JsPluginDef[] = window.filehub?._jsPlugins ?? [];
  for (const plugin of jsPlugins) {
    for (const entry of (plugin.contextMenu ?? [])) {
      if (!fileMime.startsWith(entry.mime)) continue;
      const el = makeClickItem(entry.label ?? plugin.name, 'bx-link-external', () => {
        if (typeof entry.script === 'function') {
          entry.script({ filePath, fileName, mimeType: fileMime }, createPluginUI(plugin.name));
        } else if (typeof entry.script === 'string') {
          executePluginScript(entry.script, filePath, fileName, fileMime, plugin.name);
        } else if (entry.url) {
          const url = substituteUrl(entry.url, filePath, fileName, fileMime, '');
          window.open(url, '_blank');
        }
      });
      items.push(el);
    }
  }

  // Node plugins — appear for every file, opt-in by plugin declaration
  const jsPluginsAll: JsPluginDef[] = window.filehub?._jsPlugins ?? [];
  const nodePlugins = jsPluginsAll.filter(p => !!p.node);
  if (nodePlugins.length > 0) {
    const sep = document.createElement('div');
    sep.className = 'context-menu-separator';
    items.push(sep);

    for (const plugin of nodePlugins) {
      const el = makeClickItem(plugin.name, 'bx-git-network', () => {
        const def = NodeGraph.getDef(plugin.name);
        if (!def) return;
        const { inputs, output } = NodeGraph.preparePorts(plugin.name, def);
        const ui = createPluginUI(plugin.name);
        def.run(inputs, output, ui);
      });
      items.push(el);
    }
  }

  return items;
}


async function openWithShare(filePath: string, fileName: string, fileMime: string, urlTemplate: string): Promise<void> {
  const enc = encodeURIComponent(filePath);

  // Check if a no-passcode share link already exists for this file.
  const lookupRes = await fetch(`/share/for-file?file=${enc}`);
  let hash: string;

  if (lookupRes.ok) {
    hash = await lookupRes.text();
  } else {
    // No existing link — ask the user before creating one.
    if (!confirm(`"${fileName}" is not shared. Create a public share link to open it in this app?`)) return;
    const createRes = await fetch(`/share/auto?file=${enc}`, { method: 'POST' });
    if (!createRes.ok) { alert('Failed to create share link.'); return; }
    hash = await createRes.text();
  }

  const shareUrl = `${window.location.origin}/s/${hash}/file?file=${enc}`;
  const url = substituteUrl(urlTemplate, filePath, fileName, fileMime, shareUrl);
  window.open(url, '_blank');
}


function executePluginScript(script: string, filePath: string, fileName: string, fileMime: string, pluginName: string): void {
  try {
    const run = new Function(script + '\nreturn run;')();
    if (typeof run !== 'function') {
      console.error('Plugin script must define a function named "run"');
      return;
    }
    run({ filePath, fileName, mimeType: fileMime }, createPluginUI(pluginName));
  } catch (e) {
    console.error('Plugin script error:', e);
  }
}


function substituteUrl(template: string, filePath: string, fileName: string, fileMime: string, shareUrl: string): string {
  const downloadUrl = `${window.location.origin}/download?file=${encodeURIComponent(filePath)}`;
  return template
    .replace('{file_path}',   encodeURIComponent(filePath))
    .replace('{file_name}',   encodeURIComponent(fileName))
    .replace('{mime_type}',   encodeURIComponent(fileMime))
    .replace('{download_url}', encodeURIComponent(downloadUrl))
    .replace('{share_url}',   encodeURIComponent(shareUrl));
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


function initMenuFixed(menu: HTMLElement, clientX: number, clientY: number): void {
  menu.style.position = 'fixed';
  menu.style.display  = 'block';
  menu.style.left = clientX + 'px';
  menu.style.top  = clientY + 'px';
  menu.style.zIndex = '500';

  requestAnimationFrame(() => {
    const rect = menu.getBoundingClientRect();
    if (rect.right  > window.innerWidth)  menu.style.left = (clientX - rect.width)  + 'px';
    if (rect.bottom > window.innerHeight) menu.style.top  = (clientY - rect.height) + 'px';
  });

  menu.addEventListener('Close', () => {
    menu.classList.add('closing');
    menu.addEventListener('animationend', () => menu.remove(), { once: true });
  });
}
