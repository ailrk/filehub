/*
 * Mobile panel / sidebar management — replaces _hyperscript attribute scripts.
 * Registered once on document at startup; survives all htmx swaps.
 */
import { showShareModal } from '../../modal-builder.js';
export function register() {
    document.addEventListener('click', onDocumentClick);
    document.body.addEventListener('htmx:beforeRequest', (e) => {
        if (e.detail.elt.closest('#control-panel')) {
            document.getElementById('overlay')?.classList.remove('show');
            document.getElementById('control-panel')?.classList.remove('show');
        }
    });
}
function onDocumentClick(e) {
    const t = e.target;
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
    if (t.closest('[data-action="share-links"]')) {
        document.getElementById('overlay')?.classList.remove('show');
        document.getElementById('control-panel')?.classList.remove('show');
        setTimeout(() => htmx.ajax('GET', '/share/links', { target: '#index', swap: 'beforeend' }), 340);
        return;
    }
    if (t.closest('[data-action="share-selected"]')) {
        const row = document.querySelector('tr.selected[data-path]');
        const path = row?.dataset.path;
        if (path) {
            document.getElementById('overlay')?.classList.remove('show');
            document.getElementById('control-panel')?.classList.remove('show');
            setTimeout(() => showShareModal(path), 340);
        }
        return;
    }
}
function toggle(id) {
    document.getElementById(id)?.classList.toggle('show');
}
function promptNew(kind) {
    const name = prompt(`New ${kind} name`);
    if (!name)
        return;
    const path = kind === 'folder' ? '/folders/new' : '/files/new';
    const key = kind === 'folder' ? 'new-folder' : 'new-file';
    htmx.ajax('POST', path, {
        target: '#view',
        values: { [key]: name },
        swap: 'outerHTML',
    });
}
