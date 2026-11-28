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

export interface WorkspaceSaveState {
  id: string;
  name: string;
}

export interface ProjectState {
  version: 1;
  workspaces: WorkspaceSaveState[];
  activeWorkspaceId: string;
  windows: WindowSaveState[];
}

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

let chipContainer: HTMLElement | null = null;
let workspaceBtnEl: HTMLElement | null = null;
let wsPopup: HTMLElement | null = null;

// ---------------------------------------------------------------------------
// Init
// ---------------------------------------------------------------------------

export function init(): void {
  const ws = addWorkspace('Workspace 1');
  activeWorkspaceId = ws.id;

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
  const ws: Workspace = { id, name };
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
// Window management
// ---------------------------------------------------------------------------

export function openWindow(pluginName: string, opts: WindowOptions = {}): WindowHandle {
  if (!chipContainer) {
    return createWindow(opts);
  }

  const handle = createWindow(opts);
  const id = handle.id;

  const mw: ManagedWindow = {
    handle,
    pluginName,
    title: opts.title ?? pluginName,
    workspaceId: activeWorkspaceId,
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
  };

  handle.onClose = () => {
    windows.delete(id);
    removeChip(id);
    if (focusedWindowId === id) focusedWindowId = null;
    NodeGraph.onWindowClosed(id);
  };

  createChip(mw, id);

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
