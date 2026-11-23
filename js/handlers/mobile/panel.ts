/*
 * Mobile panel / sidebar management — replaces _hyperscript attribute scripts.
 * Registered once on document at startup; survives all htmx swaps.
 */

declare var htmx: any;

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
    promptNew('folder');
    return;
  }

  if (t.closest('#new-file-btn')) {
    promptNew('file');
    return;
  }

  if (t.closest('[data-close-editor]')) {
    document.getElementById('editor-modal')?.remove();
    return;
  }
}

function toggle(id: string) {
  document.getElementById(id)?.classList.toggle('show');
}

function promptNew(kind: 'folder' | 'file') {
  const name = prompt(`New ${kind} name`);
  if (!name) return;
  const path = kind === 'folder' ? '/folders/new' : '/files/new';
  const key  = kind === 'folder' ? 'new-folder' : 'new-file';
  htmx.ajax('POST', path, {
    target: '#view',
    values: { [key]: name },
    swap: 'outerHTML',
  });
}
