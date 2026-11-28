/*
 * Modal lifecycle and builders.
 *
 * Lifecycle rules:
 *   - Click .modal-underlay → close the containing .modal
 *   - Click [data-close-modal] → close the containing .modal
 *   - Dispatch a 'Close' event on any element inside .modal → close it
 *
 * Registered once on document at startup; survives all htmx swaps.
 */
import { i18n } from './i18n.js';
// ---------------------------------------------------------------------------
// Lifecycle
// ---------------------------------------------------------------------------
export function register() {
    document.addEventListener('click', onDocumentClick, { capture: true });
    document.addEventListener('Close', onClose, { capture: true });
}
function onDocumentClick(e) {
    const target = e.target;
    if (target.closest('[data-close-modal]') || target.closest('.modal-underlay')) {
        const modal = target.closest('.modal');
        if (modal) {
            e.stopPropagation();
            close(modal);
        }
    }
}
function onClose(e) {
    const target = e.target;
    const modal = target.matches('.modal')
        ? target
        : target.closest('.modal');
    if (modal)
        close(modal);
}
export function close(modal) {
    if (modal.classList.contains('closing'))
        return;
    modal.classList.add('closing');
    modal.addEventListener('animationend', () => modal.remove(), { once: true });
}
// ---------------------------------------------------------------------------
// Builders
// ---------------------------------------------------------------------------
function makeModal(id) {
    const wrap = document.createElement('div');
    wrap.className = 'modal';
    const underlay = document.createElement('div');
    underlay.className = 'modal-underlay';
    const content = document.createElement('div');
    content.className = 'modal-content';
    content.id = id;
    wrap.appendChild(underlay);
    wrap.appendChild(content);
    return wrap;
}
function appendModal(modal) {
    document.getElementById('index')?.appendChild(modal);
}
function esc(s) {
    return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}
function formatDate(iso) {
    if (!iso)
        return '-';
    try {
        const d = new Date(iso);
        const pad = (n) => String(n).padStart(2, '0');
        return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}`;
    }
    catch {
        return iso;
    }
}
function toReadableSize(bytes) {
    if (bytes === 0)
        return '0 B';
    const units = ['B', 'KB', 'MB', 'GB', 'TB'];
    const i = Math.min(Math.floor(Math.log2(bytes) / 10), units.length - 1);
    const val = bytes / Math.pow(1024, i);
    return (val < 10 ? val.toFixed(1) : Math.round(val).toString()) + ' ' + units[i];
}
export function showRenameModal(path) {
    const t = i18n();
    const filename = decodeURIComponent(path).split('/').pop() ?? path;
    const modal = makeModal('rename-modal');
    const content = modal.querySelector('.modal-content');
    content.innerHTML = `
    <div class="modal-title-bar">
      <span class="bold">${esc(t.modal_confirm)}</span>
      <div class="btn-modal-close" data-close-modal="true"><i class="bx bx-x"></i></div>
    </div>
    <form class="simple-modal-form" hx-post="/files/rename" hx-target="#view" hx-swap="outerHTML"
          hx-on::after-request="this.closest('.modal').dispatchEvent(new Event('Close'))">
      <input type="hidden" name="old" value="${esc(path)}">
      <input class="form-control" type="text" name="new" value="${esc(filename)}">
      <button class="btn btn-accent" type="submit">${esc(t.modal_confirm)}</button>
    </form>`;
    appendModal(modal);
    htmx.process(modal);
    modal.querySelector('input[name="new"]')?.select();
}
export function showNewFileModal() {
    const t = i18n();
    const modal = makeModal('new-file-modal');
    const content = modal.querySelector('.modal-content');
    content.innerHTML = `
    <div class="modal-title-bar">
      <span class="bold">${esc(t.modal_file)}</span>
      <div class="btn-modal-close" data-close-modal="true"><i class="bx bx-x"></i></div>
    </div>
    <form class="simple-modal-form" hx-post="/files/new" hx-swap="none"
          hx-on::after-request="this.closest('.modal').dispatchEvent(new Event('Close'))">
      <input class="form-control" type="text" name="new-file" placeholder="${esc(t.placeholder_newfile)}">
      <button class="btn btn-accent" type="submit">${esc(t.modal_create)}</button>
    </form>`;
    appendModal(modal);
    htmx.process(modal);
    modal.querySelector('input')?.focus();
}
export function showNewFolderModal() {
    const t = i18n();
    const modal = makeModal('new-folder-modal');
    const content = modal.querySelector('.modal-content');
    content.innerHTML = `
    <div class="modal-title-bar">
      <span class="bold">${esc(t.modal_folder)}</span>
      <div class="btn-modal-close" data-close-modal="true"><i class="bx bx-x"></i></div>
    </div>
    <form class="simple-modal-form" hx-post="/folders/new" hx-swap="none"
          hx-on::after-request="this.closest('.modal').dispatchEvent(new Event('Close'))">
      <input class="form-control" type="text" name="new-folder" placeholder="${esc(t.placeholder_newfoler)}">
      <button class="btn btn-accent" type="submit">${esc(t.modal_create)}</button>
    </form>`;
    appendModal(modal);
    htmx.process(modal);
    modal.querySelector('input')?.focus();
}
export function showShareModal(path) {
    const t = i18n();
    const displayName = decodeURIComponent(path).split('/').filter(Boolean).pop() ?? path;
    const modal = makeModal('share-modal');
    const content = modal.querySelector('.modal-content');
    content.innerHTML = `
    <div class="modal-title-bar">
      <span>${esc(t.share)}</span>
      <div class="modal-title-bar-control">
        <span class="btn-modal-close" onclick="this.closest('.modal').remove()"><i class="bx bx-x"></i></span>
      </div>
    </div>
    <form id="share-form"
          hx-post="/share"
          hx-target="#index"
          hx-swap="beforeend"
          hx-on::after-request="this.closest('.modal').remove()">
      <input type="hidden" name="file" value="${esc(path)}">
      <div class="share-modal-body">
        <div class="share-modal-filename"><i class="bx bxs-file" style="margin-right:6px"></i>${esc(displayName)}</div>
        <div class="share-modal-section">
          <div class="share-modal-section-label">Link type</div>
          <div class="share-link-type-group">
            <label class="share-radio-label">
              <input type="radio" name="linkType" value="indefinite" checked> ${esc(t.share_link_indefinite)}
            </label>
            <label class="share-radio-label">
              <input type="radio" name="linkType" value="singleuse"> ${esc(t.share_link_singleuse)}
            </label>
            <label class="share-radio-label">
              <input type="radio" name="linkType" value="expiry"> ${esc(t.share_link_expiry)}
              <input class="share-expiry-input" type="number" name="expiryHours"
                     min="1" max="8760" value="24" placeholder="hours">
              <span style="font-size:0.85rem">h</span>
            </label>
          </div>
        </div>
        <div class="share-modal-section">
          <div class="share-modal-section-label">Passcode</div>
          <input class="form-control" type="password" name="passcode" placeholder="${esc(t.share_passcode)}">
        </div>
        <label class="share-radio-label">
          <input type="checkbox" name="readonly" value="true"> ${esc(t.modal_readonly)}
        </label>
        <div class="share-modal-footer">
          <button class="btn btn-accent" type="submit">${esc(t.share_create)}</button>
        </div>
      </div>
    </form>`;
    appendModal(modal);
    htmx.process(modal);
}
export function showFileDetailModal(name, mtime, atime, size, mime) {
    const t = i18n();
    const modal = makeModal('file-detail-modal');
    const content = modal.querySelector('.modal-content');
    const readableSize = toReadableSize(parseInt(size, 10) || 0);
    content.innerHTML = `
    <div class="modal-title-bar">
      <span class="bold">${esc(t.modal_detail)}</span>
      <div class="btn-modal-close" data-close-modal="true"><i class="bx bx-x"></i></div>
    </div>
    <div class="file-detail-body">
      <div class="file-detail-row">
        <span class="file-detail-label">${esc(t.detail_filename)}</span>
        <span class="file-detail-value">${esc(name)}</span>
      </div>
      <div class="file-detail-row">
        <span class="file-detail-label">${esc(t.detail_content_type)}</span>
        <span class="file-detail-value">${esc(mime)}</span>
      </div>
      <div class="file-detail-row">
        <span class="file-detail-label">${esc(t.detail_size)}</span>
        <span class="file-detail-value">${esc(readableSize)}</span>
      </div>
      <div class="file-detail-row">
        <span class="file-detail-label">${esc(t.detail_modified)}</span>
        <span class="file-detail-value">${esc(formatDate(mtime))}</span>
      </div>
      <div class="file-detail-row">
        <span class="file-detail-label">${esc(t.detail_accessed)}</span>
        <span class="file-detail-value">${esc(formatDate(atime))}</span>
      </div>
    </div>`;
    appendModal(modal);
}
