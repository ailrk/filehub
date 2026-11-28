'use strict';

declare var htmx: any;

declare global {
  interface HTMLVideoElement { _hoverInited?: boolean; }
}

import * as Balloon from './balloon.js';
import * as Cookie from './cookie.js';
import * as DesktopContextmenu from './hdesktop-contextmenu.js';
import * as DesktopDrag from './hdesktop-drag.js';
import * as DesktopScroll from './hdesktop-scroll.js';
import * as DesktopSelected from './hdesktop-selected.js';
import * as DesktopSort from './hdesktop-sort.js';
import * as MobileCloseSidebar from './hmobile-closeSidebar.js';
import * as MobilePanel from './hmobile-panel.js';
import * as MobileSelected from './hmobile-selected.js';
import * as Modal from './modal.js';
import * as NodeGraph from './hdesktop-nodeGraph.js';
import * as Workspace from './hdesktop-workspace.js';
import Viewer from './viewer.js';
import type { DeleteProgressed, MoveProgressed, PasteProgressed, TaskCompleted, TaskFailed, UploadProgressed } from './def.js';
import { Display } from './def.js';
import { createPluginUI } from './hdesktop-pluginUI.js';
import { i18n } from './i18n.js';

const balloonWaitTime = 2000;
let viewer: Viewer | null = null;
let display: Display = Cookie.getCookie('display')! as Display;
// Cookie can be stale when switching between desktop/mobile emulation in DevTools.
// Fall back to DOM: control-panel-btn only exists in the mobile template.
if (display === 'Desktop' && document.getElementById('control-panel-btn')) {
  display = 'Mobile';
}
let evtSource: EventSource | null;
const taskLabels: Map<number, string> = new Map();

document.addEventListener('DOMContentLoaded', () => {
  Modal.register();

  document.addEventListener('click', onActionClick);

  if (display === 'Desktop') {
    DesktopContextmenu.register();
    DesktopSelected.register();
    DesktopDrag.register();
    DesktopScroll.register();
    DesktopSort.register();
    document.addEventListener('click', closeDropdowns);
    document.addEventListener('htmx:afterSettle', closeDropdowns);
    document.addEventListener('click', openPluginScriptOnClick);
    Workspace.init();
    NodeGraph.init();
  }

  if (display === 'Mobile') {
    MobileCloseSidebar.register();
    MobileSelected.register();
    MobilePanel.register();
    DesktopSort.register();
    document.querySelector('#table')?.addEventListener('contextmenu', e => e.preventDefault());
  }

  document.addEventListener('filehub:themeChanged', handleThemeChanged);
  document.addEventListener('click', openViewerOnClick);
  document.addEventListener('click', openBlankOnClick);
  document.body.addEventListener('htmx:responseError', handleError);

  document.body.addEventListener('htmx:beforeRequest', (e: any) => {
    const targetIsIndex = (e.detail.target as HTMLElement)?.id === 'index';
    const triggerWantsSpinner = (e.detail.elt as HTMLElement)?.getAttribute('hx-indicator') === '#global-spinner';
    if (!targetIsIndex && !triggerWantsSpinner) return;
    document.getElementById('view')?.classList.add('view-loading');
    document.getElementById('global-spinner')?.classList.add('htmx-request');
  });

  const clearViewLoading = () => {
    document.getElementById('view')?.classList.remove('view-loading');
    document.getElementById('global-spinner')?.classList.remove('htmx-request');
  };

  document.body.addEventListener('htmx:afterSwap',    clearViewLoading);
  document.body.addEventListener('htmx:responseError', clearViewLoading);

  document.body.addEventListener('htmx:afterSettle', () => {
    if (display === 'Desktop') {
      DesktopScroll.rebind();
      DesktopSelected.collectFromHtml();
      updatePluginToolbarActions();
      patchRegisterPlugin();
    }
    initTomlEditors();
    initPluginToggles();
    if (display === 'Desktop') initVideoHover();
  });

  if (display === 'Desktop') patchRegisterPlugin();

  document.addEventListener('animationend', (e: AnimationEvent) => {
    const el = e.target as Element;
    if ((e as any).animationName === 'pastedFlash') {
      el.classList.remove('pasted-new');
    }
  });

  initTomlEditors();
  initPluginToggles();
  if (display === 'Desktop') initVideoHover();
});


if ('serviceWorker' in navigator && window.top === window.self) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('/static/serviceWorker.js', { type: 'module' })
      .then(reg => console.log('Service worker registered:', reg))
      .catch(err => console.error('Service worker registration failed:', err));
  });
}


window.addEventListener('beforeunload', async () => {
  await fetch('/init', {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    body: new URLSearchParams({ res: window.innerWidth + 'x' + window.innerHeight }),
  });
});


startListenSSE();
setInterval(() => {
  if (!evtSource || evtSource.readyState === EventSource.CLOSED) {
    evtSource = null;
    startListenSSE();
  }
}, 5000);


function handleThemeChanged() {
  const oldLink = document.querySelector('link[rel="stylesheet"][href*="/theme.css"]') as HTMLLinkElement | null;
  if (!oldLink) return;
  const newLink = oldLink.cloneNode() as HTMLLinkElement;
  newLink.href = '/theme.css?v=' + Date.now();
  newLink.onload = () => oldLink.remove();
  oldLink.parentNode!.insertBefore(newLink, oldLink.nextSibling);
}


function onActionClick(e: MouseEvent): void {
  const btn = (e.target as Element).closest<HTMLElement>('[data-action]');
  if (!btn) return;
  switch (btn.dataset.action) {
    case 'new-folder':     Modal.showNewFolderModal(); break;
    case 'new-file':       Modal.showNewFileModal();   break;
    case 'toggle-theme':   handleToggleTheme(btn);            break;
    case 'toggle-sidebar': handleToggleSidebar();             break;
  }
}


async function handleToggleTheme(btn: HTMLElement): Promise<void> {
  const current = btn.dataset.currentTheme;
  const next = current === 'light' ? 'dark' : 'light';
  btn.dataset.currentTheme = next;
  const icon = btn.querySelector('i');
  if (icon) icon.className = next === 'dark' ? 'bx bxs-sun' : 'bx bxs-moon';
  const t = i18n();
  btn.dataset.tooltip = next === 'dark' ? t.control_panel_light : t.control_panel_dark;
  await fetch('/theme/toggle');
  document.dispatchEvent(new Event('filehub:themeChanged'));
}


function handleToggleSidebar(): void {
  document.getElementById('index')?.classList.toggle('sidebar-collapsed');
  fetch('/sidebar/toggle');
}


function handleError(e: any) {
  const xhr = e.detail.xhr;
  const status = xhr.status;
  let message = xhr.responseText as string;
  if (!message) {
    const statusMessages: Record<number, string> = {
      400: 'Bad Request', 401: 'Unauthorized', 402: 'Payment required',
      403: 'Forbidden resource', 404: 'Resource not found', 500: 'Server internal error',
    };
    message = statusMessages[status] ?? 'Something went wrong';
  }
  Balloon.pushBalloon({ kind: 'ErrorMsg', msg: `${message} ${status}`, duration: balloonWaitTime });
}


function openViewerOnClick(e: MouseEvent) {
  const el = (e.target as Element).closest<HTMLElement>('[data-action="open-viewer"]');
  if (!el) return;
  const path = (el.closest<HTMLElement>('[data-path]') ?? el).dataset.path ?? '';
  if (!path) return;
  viewer = new Viewer(path);
  viewer.show();
}


function openPluginScriptOnClick(e: MouseEvent) {
  const el = (e.target as Element).closest<HTMLElement>('[data-action="plugin-script"]');
  if (!el) return;
  const script = el.dataset.pluginScript ?? '';
  if (!script) return;
  const row = el.closest<HTMLElement>('[data-path]');
  const filePath = row?.dataset.path ?? '';
  const fileName = row?.dataset.name ?? '';
  const fileMime = row?.dataset.mime ?? '';
  runPluginScript(script, filePath, fileName, fileMime, '');
}


function openBlankOnClick(e: MouseEvent) {
  const el = (e.target as Element).closest<HTMLElement>('[data-action="open-blank"]');
  if (!el) return;
  const path = (el.closest<HTMLElement>('[data-path]') ?? el).dataset.path ?? '';
  if (path) window.open(`/serve?file=${path}`, '_blank');
}


function initVideoHover() {
  document.querySelectorAll<HTMLVideoElement>('video.thumbnail-video').forEach(video => {
    if (video._hoverInited) return;
    video._hoverInited = true;
    const thumb = video.closest('.thumbnail');
    if (!thumb) return;
    thumb.addEventListener('mouseenter', () => { video.play().catch(() => {}); });
    thumb.addEventListener('mouseleave', () => { video.pause(); video.currentTime = 0; });
  });
}


function closeDropdowns(e: any) {
  const target = e?.target as Element | null;
  if (target?.matches('.dropdown *') || target?.matches('.dropdown-content *')) return;
  document.getElementById('contextmenu')?.dispatchEvent(new Event('Close'));
  const table = document.getElementById('table');
  if (table) table.style.pointerEvents = 'auto';
}


function htmxProcessOOB(data: { htmxResponse: string | null }) {
  if (data.htmxResponse) {
    htmx.swap(document.body, data.htmxResponse, { swapStyle: 'none' });
  }
}


// ---------------------------------------------------------------------------
// TOML editor
// ---------------------------------------------------------------------------

function highlightToml(raw: string): string {
  const esc = (s: string) =>
    s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

  const hlValue = (val: string): string => {
    const t = val.trim();
    if (t.startsWith('"') || t.startsWith("'"))
      return `<span class="hl-string">${esc(val)}</span>`;
    if (t === 'true' || t === 'false')
      return `<span class="hl-bool">${esc(val)}</span>`;
    if (/^-?\d/.test(t))
      return `<span class="hl-number">${esc(val)}</span>`;
    return esc(val);
  };

  return raw.split('\n').map(line => {
    if (/^\s*#/.test(line))
      return `<span class="hl-comment">${esc(line)}</span>`;
    if (/^\s*\[/.test(line))
      return `<span class="hl-section">${esc(line)}</span>`;
    const eqIdx = line.indexOf('=');
    if (eqIdx !== -1) {
      const keyPart = line.slice(0, eqIdx);
      const valPart = line.slice(eqIdx + 1);
      return `<span class="hl-key">${esc(keyPart)}</span>=${hlValue(valPart)}`;
    }
    return esc(line);
  }).join('\n') + '\n';
}

function autoResizeTextarea(textarea: HTMLTextAreaElement) {
  textarea.style.height = 'auto';
  textarea.style.height = textarea.scrollHeight + 'px';
}

function initTomlEditor(textarea: HTMLTextAreaElement) {
  if ((textarea as any)._tomlInited) return;
  (textarea as any)._tomlInited = true;
  const pre = textarea.previousElementSibling as HTMLPreElement | null;
  if (!pre?.classList.contains('toml-highlight')) return;
  const update = () => {
    pre.innerHTML = highlightToml(textarea.value);
    autoResizeTextarea(textarea);
  };
  textarea.addEventListener('input', update);
  textarea.addEventListener('keydown', (e: KeyboardEvent) => {
    if (e.key === 's' && (e.ctrlKey || e.metaKey)) {
      e.preventDefault();
      textarea.closest('form')?.requestSubmit();
    }
    if (e.key === 'Tab') {
      e.preventDefault();
      const start = textarea.selectionStart;
      const end = textarea.selectionEnd;
      textarea.value = textarea.value.slice(0, start) + '  ' + textarea.value.slice(end);
      textarea.selectionStart = textarea.selectionEnd = start + 2;
      update();
    }
  });
  update();
}

function initTomlEditors() {
  document.querySelectorAll<HTMLTextAreaElement>('textarea.toml-textarea')
    .forEach(initTomlEditor);
}

type PluginEntryAction = { type: 'url'; value: string } | { type: 'script'; value: string };
declare var filehubPlugins: { name: string; contextMenu: unknown[]; toolbar: { mime: string; label: string; action: PluginEntryAction }[] }[] | undefined;

function executeJsPluginToolbarEntry(entry: JsPluginEntry, filePath: string, fileName: string, fileMime: string, pluginName: string): void {
  if (typeof entry.script === 'function') {
    entry.script({ filePath, fileName, mimeType: fileMime }, createPluginUI(pluginName));
  } else if (typeof entry.script === 'string') {
    runPluginScript(entry.script, filePath, fileName, fileMime, pluginName);
  } else if (entry.url) {
    const url = entry.url
      .replace('{file_path}', encodeURIComponent(filePath))
      .replace('{file_name}', encodeURIComponent(fileName))
      .replace('{mime_type}', encodeURIComponent(fileMime));
    window.open(url, '_blank');
  }
}

function updatePluginToolbarActions() {
  const container = document.getElementById('plugin-toolbar-actions');
  if (!container) return;

  const plugins = (typeof filehubPlugins !== 'undefined' && filehubPlugins) ? filehubPlugins : [];
  const selected = Array.from(document.querySelectorAll<HTMLElement>('#table .table-item.selected'));
  const files = selected.filter(el => el.dataset.ftype !== 'dir');

  container.innerHTML = '';
  if (files.length !== 1) return;

  const file = files[0];
  const filePath = file.dataset.path ?? '';
  const fileName = file.dataset.name ?? '';
  const fileMime = file.dataset.mime ?? '';

  for (const plugin of plugins) {
    for (const entry of plugin.toolbar) {
      if (!fileMime.startsWith(entry.mime)) continue;
      const btn = document.createElement('button');
      btn.className = 'btn btn-control plugin-toolbar-btn';
      btn.textContent = entry.label;
      btn.addEventListener('click', async () => {
        if (entry.action.type === 'script') {
          runPluginScript(entry.action.value, filePath, fileName, fileMime, plugin.name);
          return;
        }
        const urlTemplate = entry.action.value;
        const needsShare = urlTemplate.includes('{share_url}');
        const enc = encodeURIComponent(filePath);
        const downloadUrl = `${window.location.origin}/download?file=${enc}`;
        if (needsShare) {
          const lookupRes = await fetch(`/share/for-file?file=${enc}`);
          let hash: string;
          if (lookupRes.ok) {
            hash = await lookupRes.text();
          } else {
            if (!confirm(`"${fileName}" is not shared. Create a public share link?`)) return;
            const createRes = await fetch(`/share/auto?file=${enc}`, { method: 'POST' });
            if (!createRes.ok) { alert('Failed to create share link.'); return; }
            hash = await createRes.text();
          }
          const shareUrl = `${window.location.origin}/s/${hash}/file?file=${enc}`;
          const url = urlTemplate
            .replace('{file_path}',    encodeURIComponent(filePath))
            .replace('{file_name}',    encodeURIComponent(fileName))
            .replace('{mime_type}',    encodeURIComponent(fileMime))
            .replace('{download_url}', encodeURIComponent(downloadUrl))
            .replace('{share_url}',    encodeURIComponent(shareUrl));
          window.open(url, '_blank');
        } else {
          const url = urlTemplate
            .replace('{file_path}',    encodeURIComponent(filePath))
            .replace('{file_name}',    encodeURIComponent(fileName))
            .replace('{mime_type}',    encodeURIComponent(fileMime))
            .replace('{download_url}', encodeURIComponent(downloadUrl))
            .replace('{share_url}',    '');
          window.open(url, '_blank');
        }
      });
      container.appendChild(btn);
    }
  }

  const jsPlugins: JsPluginDef[] = window.filehub?._jsPlugins ?? [];
  for (const plugin of jsPlugins) {
    for (const entry of (plugin.toolbar ?? [])) {
      if (!fileMime.startsWith(entry.mime)) continue;
      const btn = document.createElement('button');
      btn.className = 'btn btn-control plugin-toolbar-btn';
      btn.textContent = entry.label ?? plugin.name;
      btn.addEventListener('click', () => {
        executeJsPluginToolbarEntry(entry, filePath, fileName, fileMime, plugin.name);
      });
      container.appendChild(btn);
    }
  }
}

// Wrap window.filehub.registerPlugin so node defs are picked up automatically
// each time plugins are (re-)registered after an htmx swap.
function patchRegisterPlugin(): void {
  if (!window.filehub) return;
  const original = window.filehub.registerPlugin.bind(window.filehub);
  window.filehub.registerPlugin = (def: JsPluginDef) => {
    original(def);
    if (def.node) NodeGraph.registerDef(def.name, def.node as any);
  };
  // Re-register already-loaded plugins in case this runs after the IIFEs
  for (const p of window.filehub._jsPlugins) {
    if (p.node) NodeGraph.registerDef(p.name, p.node as any);
  }
}


function runPluginScript(script: string, filePath: string, fileName: string, fileMime: string, pluginName: string): void {
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


function initPluginToggles() {
  document.querySelectorAll<HTMLElement>('.plugin-item-header').forEach(header => {
    if ((header as any)._toggleInited) return;
    (header as any)._toggleInited = true;
    header.addEventListener('click', (e: MouseEvent) => {
      if ((e.target as Element).closest('button')) return;
      const item = header.closest('.plugin-item');
      if (!item) return;
      item.classList.toggle('expanded');
      if (item.classList.contains('expanded')) {
        initTomlEditors();
      }
    });
  });
}

// ---------------------------------------------------------------------------

function startListenSSE() {
  if (evtSource && evtSource.readyState !== EventSource.CLOSED) return;

  evtSource = new EventSource('/listen');

  evtSource.onerror = () => {
    // EventSource auto-reconnects on transient errors; only intervene when
    // it gives up and enters CLOSED state permanently.
    if (evtSource?.readyState === EventSource.CLOSED) {
      evtSource = null;
      setTimeout(startListenSSE, 3000);
    }
  };

  evtSource.onmessage = event => {
    console.log('New update:', JSON.parse(event.data));
  };

  evtSource.addEventListener('Pong', () => console.log('pong'));

  evtSource.addEventListener('TaskCompleted', e => {
    const data = JSON.parse(e.data) as TaskCompleted;
    const label = taskLabels.get(data.taskId) ?? 'Task';
    taskLabels.delete(data.taskId);
    htmxProcessOOB(data);
    Balloon.deleteLongLivedBalloon(data.taskId);
    Balloon.pushBalloon({ kind: 'InfoMsg', msg: `${label} complete`, duration: 3000 });
  });

  evtSource.addEventListener('TaskFailed', e => {
    const data = JSON.parse(e.data) as TaskFailed;
    const label = taskLabels.get(data.taskId) ?? 'Task';
    taskLabels.delete(data.taskId);
    htmxProcessOOB(data);
    Balloon.deleteLongLivedBalloon(data.taskId);
    Balloon.pushBalloon({ kind: 'ErrorMsg', msg: `${label} failed`, duration: 5000 });
  });

  evtSource.addEventListener('DeleteProgressed', e => {
    const data = JSON.parse(e.data) as DeleteProgressed;
    taskLabels.set(data.taskId, 'Delete');
    htmxProcessOOB(data);
    Balloon.pushBalloon({ kind: 'ProgressedMsg', msg: 'Deleting', taskId: data.taskId, progress: data.progress });
  });

  evtSource.addEventListener('PasteProgressed', e => {
    const data = JSON.parse(e.data) as PasteProgressed;
    taskLabels.set(data.taskId, 'Paste');
    htmxProcessOOB(data);
    Balloon.pushBalloon({ kind: 'ProgressedMsg', msg: 'Pasting', taskId: data.taskId, progress: data.progress });
  });

  evtSource.addEventListener('MoveProgressed', e => {
    const data = JSON.parse(e.data) as MoveProgressed;
    taskLabels.set(data.taskId, 'Move');
    htmxProcessOOB(data);
    Balloon.pushBalloon({ kind: 'ProgressedMsg', msg: 'Moving', taskId: data.taskId, progress: data.progress });
  });

  evtSource.addEventListener('UploadProgressed', e => {
    const data = JSON.parse(e.data) as UploadProgressed;
    taskLabels.set(data.taskId, 'Upload');
    htmxProcessOOB(data);
    Balloon.pushBalloon({ kind: 'ProgressedMsg', msg: 'Uploading', taskId: data.taskId, progress: data.progress });
  });
}
