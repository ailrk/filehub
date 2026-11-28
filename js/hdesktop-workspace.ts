/*
 * Workspace manager — owns the in-memory project state (workspaces + windows).
 * Renders and updates the taskbar. Designed so ProjectState can later be
 * serialized to a file for save/load without server involvement.
 */

import { createWindow, WindowHandle, WindowOptions } from './hdesktop-window.js';
import * as NodeGraph from './hdesktop-nodeGraph.js';

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

export interface WindowSaveState {
  id: string;
  pluginName: string;
  title: string;
  workspaceId: string;
  minimized: boolean;
  x: number; y: number;
  width: number; height: number;
}

export interface HLink {
  id: string;
  targetId: string;
  path: string;
  clientPath: string;
  mime: string;
  icon: string;
  mtime: string;
  atime: string;
  size: string;
  label: string | null;
  x: number;
  y: number;
}

export interface WorkspaceSaveState {
  id: string;
  name: string;
  hLinks: HLink[];
}

export interface CableSaveState {
  fromWindowId: string;
  toWindowId:   string;
  toPortName: string;
}

export interface ProjectState {
  version: number;
  workspaces: WorkspaceSaveState[];
  activeWorkspaceId: string;
  windows: WindowSaveState[];
  cables: CableSaveState[];
}

export const PROJECT_STATE_VERSION = 1;

interface ManagedWindow {
  handle: WindowHandle;
  pluginName: string;
  title: string;
  workspaceId: string;
  minimized: boolean;
}

interface Workspace {
  id: string;
  name: string;
  hLinks: Map<string, HLink>;
}

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

const workspaces = new Map<string, Workspace>();
const windows    = new Map<string, ManagedWindow>();
const chipEls    = new Map<string, HTMLElement>();
let activeWorkspaceId = '';
let focusedWindowId: string | null = null;
let wsCounter = 1;
let hlinkCounter = 1;
const switchListeners: Array<() => void> = [];

let chipContainer: HTMLElement | null = null;
let workspaceBtnEl: HTMLElement | null = null;
let wsPopup: HTMLElement | null = null;
let _pendingRestore: { windows: WindowSaveState[]; cables: CableSaveState[] } | null = null;
let _suppressSave = false;
let _forceWorkspaceId: string | null = null;

// ---------------------------------------------------------------------------
// Persistence
// ---------------------------------------------------------------------------

const STORAGE_KEY = 'filehub-project';

function serializeState(): ProjectState {
  return {
    version: PROJECT_STATE_VERSION,
    activeWorkspaceId,
    workspaces: Array.from(workspaces.values()).map(ws => ({
      id: ws.id,
      name: ws.name,
      hLinks: Array.from(ws.hLinks.values()),
    })),
    windows: Array.from(windows.entries()).flatMap(([id, mw]) => {
      try {
        const root = mw.handle.root;
        return [{
          id,
          pluginName: mw.pluginName,
          title: mw.title,
          workspaceId: mw.workspaceId,
          minimized: mw.minimized,
          x:      parseInt(root.style.left)   || 0,
          y:      parseInt(root.style.top)    || 0,
          width:  parseInt(root.style.width)  || 400,
          height: parseInt(root.style.height) || 300,
        }];
      } catch { return []; }
    }),
    cables: NodeGraph.getCablesState(),
  };
}

function saveState(): void {
  if (_suppressSave) return;
  try {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(serializeState()));
  } catch {}
}

export function beginBatchRestore(): void { _suppressSave = true; }
export function setRestoreWorkspaceId(id: string | null): void { _forceWorkspaceId = id; }
export function commitBatchRestore(): void { _suppressSave = false; saveState(); }
export function getActiveWorkspaceId(): string { return activeWorkspaceId; }

function loadState(): ProjectState | null {
  try {
    const raw = localStorage.getItem(STORAGE_KEY);
    if (!raw) return null;
    const state = JSON.parse(raw) as ProjectState;
    if (state.version !== PROJECT_STATE_VERSION) return null;
    return state;
  } catch {
    return null;
  }
}

function restoreState(state: ProjectState): void {
  for (const wsSave of state.workspaces) {
    const ws: Workspace = { id: wsSave.id, name: wsSave.name, hLinks: new Map() };
    for (const lnk of wsSave.hLinks) {
      ws.hLinks.set(lnk.id, lnk);
      const lnum = parseInt(lnk.id.slice('hlink-'.length)) || 0;
      if (lnum >= hlinkCounter) hlinkCounter = lnum + 1;
    }
    workspaces.set(ws.id, ws);
    const wnum = parseInt(ws.id.slice('ws-'.length)) || 0;
    if (wnum >= wsCounter) wsCounter = wnum + 1;
  }
  activeWorkspaceId = workspaces.has(state.activeWorkspaceId)
    ? state.activeWorkspaceId
    : state.workspaces[0]?.id ?? '';
}

// ---------------------------------------------------------------------------
// Init
// ---------------------------------------------------------------------------

export function init(): void {
  const saved = loadState();
  if (saved && saved.workspaces.length > 0) {
    restoreState(saved);
    if (saved.windows.length > 0 || (saved.cables?.length ?? 0) > 0) {
      _pendingRestore = { windows: saved.windows, cables: saved.cables ?? [] };
    }
  } else {
    const ws = addWorkspace('Workspace 1');
    activeWorkspaceId = ws.id;
  }

  const taskbar = document.createElement('div');
  taskbar.id = 'filehub-taskbar';

  workspaceBtnEl = document.createElement('button');
  workspaceBtnEl.id = 'filehub-workspace-btn';
  workspaceBtnEl.className = 'filehub-workspace-btn';
  renderWorkspaceLabel();
  workspaceBtnEl.addEventListener('click', toggleWsPopup);

  chipContainer = document.createElement('div');
  chipContainer.className = 'filehub-taskbar-chips';

  taskbar.appendChild(workspaceBtnEl);
  taskbar.appendChild(chipContainer);
  document.body.appendChild(taskbar);
  document.body.classList.add('has-taskbar');

  document.addEventListener('click', (e) => {
    if (wsPopup && !wsPopup.contains(e.target as Node) &&
        !workspaceBtnEl!.contains(e.target as Node)) {
      closeWsPopup();
    }
  });
}

// ---------------------------------------------------------------------------
// Workspace management
// ---------------------------------------------------------------------------

function addWorkspace(name: string): Workspace {
  const id = 'ws-' + (wsCounter++);
  const ws: Workspace = { id, name, hLinks: new Map() };
  workspaces.set(id, ws);
  return ws;
}

function switchWorkspace(toId: string): void {
  if (toId === activeWorkspaceId) return;
  const prevId = activeWorkspaceId;
  activeWorkspaceId = toId;

  for (const [id, mw] of windows) {
    if (mw.workspaceId === prevId) {
      if (!mw.minimized) mw.handle.setWorkspaceHidden(true)
      NodeGraph.onWindowVisible(id, false)
    } else if (mw.workspaceId === toId) {
      if (!mw.minimized) mw.handle.setWorkspaceHidden(false)
      NodeGraph.onWindowVisible(id, true)
    }
  }

  focusedWindowId = null;
  refreshChips();
  renderWorkspaceLabel();
  closeWsPopup();
  for (const cb of switchListeners) cb();
  saveState();
}

// ---------------------------------------------------------------------------
// Workspace popup
// ---------------------------------------------------------------------------

function toggleWsPopup(): void {
  if (wsPopup) { closeWsPopup(); return; }
  if (!workspaceBtnEl) return;

  wsPopup = document.createElement('div');
  wsPopup.className = 'filehub-ws-popup';

  for (const ws of workspaces.values()) {
    const item = document.createElement('button');
    item.className = 'filehub-ws-popup-item' + (ws.id === activeWorkspaceId ? ' active' : '');
    item.textContent = ws.name;
    item.addEventListener('click', () => switchWorkspace(ws.id));
    wsPopup.appendChild(item);
  }

  const sep = document.createElement('div');
  sep.className = 'filehub-ws-popup-sep';
  wsPopup.appendChild(sep);

  const newBtn = document.createElement('button');
  newBtn.className = 'filehub-ws-popup-item filehub-ws-popup-new';
  newBtn.innerHTML = `<i class="bx bx-plus"></i> New workspace`;
  newBtn.addEventListener('click', () => {
    const ws = addWorkspace('Workspace ' + wsCounter);
    switchWorkspace(ws.id);
  });
  wsPopup.appendChild(newBtn);

  const rect = workspaceBtnEl.getBoundingClientRect();
  wsPopup.style.left   = rect.left + 'px';
  wsPopup.style.bottom = (window.innerHeight - rect.top + 4) + 'px';

  document.body.appendChild(wsPopup);
}

function closeWsPopup(): void {
  wsPopup?.remove();
  wsPopup = null;
}

function renderWorkspaceLabel(): void {
  if (!workspaceBtnEl) return;
  const ws = workspaces.get(activeWorkspaceId);
  workspaceBtnEl.textContent = ws?.name ?? '';
}

// ---------------------------------------------------------------------------
// HLink operations
// ---------------------------------------------------------------------------

export function addHLink(
  targetId: string, path: string, clientPath: string,
  mime: string, icon: string, mtime: string, atime: string, size: string,
  label: string | null
): HLink {
  const ws = workspaces.get(activeWorkspaceId);
  if (!ws) throw new Error('no active workspace');
  const id = 'hlink-' + (hlinkCounter++);
  const hLink: HLink = { id, targetId, path, clientPath, mime, icon, mtime, atime, size, label, x: 80, y: 80 };
  ws.hLinks.set(id, hLink);
  saveState();
  return hLink;
}

export function removeHLink(id: string): void {
  workspaces.get(activeWorkspaceId)?.hLinks.delete(id);
  saveState();
}

export function moveHLink(id: string, x: number, y: number): void {
  const hLink = workspaces.get(activeWorkspaceId)?.hLinks.get(id);
  if (hLink) { hLink.x = x; hLink.y = y; saveState(); }
}

export function relabelHLink(id: string, label: string | null): void {
  const hLink = workspaces.get(activeWorkspaceId)?.hLinks.get(id);
  if (hLink) { hLink.label = label; saveState(); }
}

export function getHLinks(): HLink[] {
  return Array.from(workspaces.get(activeWorkspaceId)?.hLinks.values() ?? []);
}

export function addSwitchListener(cb: () => void): void {
  switchListeners.push(cb);
}

export function takePendingRestore(): { windows: WindowSaveState[]; cables: CableSaveState[] } | null {
  const v = _pendingRestore;
  _pendingRestore = null;
  return v;
}

export interface WindowInfo {
  pluginName:  string;
  title:       string;
  workspaceId: string;
  minimized:   boolean;
  handle:      WindowHandle;
}

export function getWindowIds(): string[] {
  return [...windows.keys()];
}

export function getWindowInfo(id: string): WindowInfo | undefined {
  const mw = windows.get(id);
  if (!mw) return undefined;
  return { pluginName: mw.pluginName, title: mw.title, workspaceId: mw.workspaceId, minimized: mw.minimized, handle: mw.handle };
}

export function closeWindowNow(id: string): void {
  const mw = windows.get(id);
  if (!mw) return;
  mw.handle.minimize();  // sets display:none so close() fires synchronously
  mw.handle.close();
}

export function applyWindowGeometry(id: string, x: number, y: number, w: number, h: number): void {
  const mw = windows.get(id);
  if (!mw) return;
  const root = mw.handle.root;
  root.style.left   = x + 'px';
  root.style.top    = y + 'px';
  root.style.width  = w + 'px';
  root.style.height = h + 'px';
  root.dispatchEvent(new CustomEvent('filehub:windowmoved'));
}

export function applyWindowMinimized(id: string, minimized: boolean): void {
  const mw = windows.get(id);
  if (!mw || mw.minimized === minimized) return;
  if (minimized) {
    mw.handle.minimize();
  } else {
    mw.minimized = false;
    mw.handle.root.style.display = '';
    void mw.handle.root.offsetWidth;
    mw.handle.root.dispatchEvent(new CustomEvent('filehub:windowmoved'));
    updateChip(id);
  }
}

// ---------------------------------------------------------------------------
// Window management
// ---------------------------------------------------------------------------

export function openWindow(pluginName: string, opts: WindowOptions = {}): WindowHandle {
  if (!chipContainer) {
    return createWindow(opts);
  }

  const handle = createWindow(opts);
  const id = handle.id;
  const workspaceId = _forceWorkspaceId ?? activeWorkspaceId;

  const mw: ManagedWindow = {
    handle,
    pluginName,
    title: opts.title ?? pluginName,
    workspaceId,
    minimized: false,
  };
  windows.set(id, mw);

  handle.onFocus = () => {
    focusedWindowId = id;
    updateAllChips();
  };

  if (NodeGraph.hasPending(pluginName)) {
    NodeGraph.onWindowCreated(id, handle.root, pluginName);
  }

  handle.onMinimize = () => {
    mw.minimized = true;
    if (focusedWindowId === id) focusedWindowId = null;
    updateChip(id);
    saveState();
  };

  handle.onClose = () => {
    windows.delete(id);
    removeChip(id);
    if (focusedWindowId === id) focusedWindowId = null;
    NodeGraph.onWindowClosed(id);
    saveState();
  };

  handle.root.addEventListener('mouseup', () => saveState(), true);

  if (workspaceId === activeWorkspaceId) {
    createChip(mw, id);
  }
  saveState();

  return handle;
}

// ---------------------------------------------------------------------------
// Taskbar chips
// ---------------------------------------------------------------------------

function createChip(mw: ManagedWindow, id: string): void {
  if (!chipContainer) return;

  const chip = document.createElement('div');
  chip.className = 'filehub-taskbar-chip';
  chip.dataset.windowId = id;

  const icon = document.createElement('i');
  icon.className = 'bx bx-window-alt';

  const label = document.createElement('span');
  label.textContent = mw.title;

  const closeBtn = document.createElement('button');
  closeBtn.className = 'filehub-taskbar-chip-close';
  closeBtn.setAttribute('aria-label', 'Close');
  closeBtn.innerHTML = `<i class="bx bx-x"></i>`;
  closeBtn.addEventListener('click', (e) => {
    e.stopPropagation();
    mw.handle.close();
  });

  chip.appendChild(icon);
  chip.appendChild(label);
  chip.appendChild(closeBtn);

  chip.addEventListener('click', (e) => {
    if ((e.target as Element).closest('.filehub-taskbar-chip-close')) return;
    if (mw.minimized) {
      mw.minimized = false;
      mw.handle.restore();         // fires onFocus → updateAllChips
    } else if (focusedWindowId === id) {
      mw.handle.minimize();        // fires onMinimize → updateChip
    } else {
      mw.handle.restore();         // bring to front without re-centering
    }
  });

  chipContainer.appendChild(chip);
  chipEls.set(id, chip);
  updateChip(id);
}

function updateChip(id: string): void {
  const chip = chipEls.get(id);
  const mw   = windows.get(id);
  if (!chip || !mw) return;

  chip.classList.toggle('chip-minimized', mw.minimized);
  chip.classList.toggle('chip-active',    !mw.minimized && focusedWindowId === id);
}

function updateAllChips(): void {
  for (const id of chipEls.keys()) updateChip(id);
}

function refreshChips(): void {
  if (!chipContainer) return;
  chipContainer.innerHTML = '';
  chipEls.clear();
  for (const [id, mw] of windows) {
    if (mw.workspaceId === activeWorkspaceId) {
      createChip(mw, id);
    }
  }
}

function removeChip(id: string): void {
  chipEls.get(id)?.remove();
  chipEls.delete(id);
}
