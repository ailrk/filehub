/*
 * Mobile panel / sidebar management — replaces _hyperscript attribute scripts.
 * Registered once on document at startup; survives all htmx swaps.
 */

declare var htmx: any;

import { showShareModal, showNewFileModal, showNewFolderModal } from '../modal-builder.js';

export function register() {
  document.addEventListener('click', onDocumentClick);
  document.body.addEventListener('htmx:beforeRequest', (e: any) => {
    if ((e.detail.elt as HTMLElement).closest('#control-panel')) {
      document.getElementById('overlay')?.classList.remove('show');
      document.getElementById('control-panel')?.classList.remove('show');
    }
  });
}

function onDocumentClick(e: MouseEvent) {
  const t = e.target as Element;

  if (t.closest('#overlay')) {
    document.getElementById('control-panel')?.classList.remove('show');
    document.getElementById('overlay')?.classList.remove('show');
    return;
  }

  if (t.closest('#control-panel-btn')) {
    toggle('overlay');
    toggle('control-panel');
    return;
  }

  if (t.closest('#sidebar-btn')) {
    toggle('overlay');
    setTimeout(() => toggle('side-bar'), 50);
    return;
  }

  if (t.closest('#locale-langauge-btn')) {
    toggle('locale');
    return;
  }

  if (t.closest('#scroll2top-btn')) {
    window.scrollTo({ top: 0, behavior: 'smooth' });
    return;
  }

  if (t.closest('#new-folder-btn')) {
    document.getElementById('overlay')?.classList.remove('show');
    document.getElementById('control-panel')?.classList.remove('show');
    setTimeout(() => showNewFolderModal(), 340);
    return;
  }

  if (t.closest('#new-file-btn')) {
    document.getElementById('overlay')?.classList.remove('show');
    document.getElementById('control-panel')?.classList.remove('show');
    setTimeout(() => showNewFileModal(), 340);
    return;
  }

  if (t.closest('[data-close-editor]')) {
    document.getElementById('editor-modal')?.remove();
    return;
  }

  if (t.closest('[data-action="share-links"]')) {
    document.getElementById('overlay')?.classList.remove('show');
    document.getElementById('control-panel')?.classList.remove('show');
    setTimeout(() => htmx.ajax('GET', '/share/links', { target: '#index', swap: 'beforeend' }), 340);
    return;
  }

  if (t.closest('[data-action="history"]')) {
    document.getElementById('overlay')?.classList.remove('show');
    document.getElementById('control-panel')?.classList.remove('show');
    setTimeout(() => htmx.ajax('GET', '/history', { target: '#index', swap: 'beforeend' }), 340);
    return;
  }

  if (t.closest('[data-action="share-selected"]')) {
    const row = document.querySelector('tr.selected[data-path]') as HTMLElement | null;
    const path = row?.dataset.path;
    if (path) {
      document.getElementById('overlay')?.classList.remove('show');
      document.getElementById('control-panel')?.classList.remove('show');
      setTimeout(() => showShareModal(path), 340);
    }
    return;
  }
}

function toggle(id: string) {
  document.getElementById(id)?.classList.toggle('show');
}

