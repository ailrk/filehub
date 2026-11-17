declare var htmx: any;

import { i18n } from './i18n.js';

function makeModal(id: string): HTMLElement {
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

function appendModal(modal: HTMLElement): void {
  document.getElementById('index')?.appendChild(modal);
}

function esc(s: string): string {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}

function formatDate(iso: string): string {
  if (!iso) return '-';
  try {
    const d = new Date(iso);
    const pad = (n: number) => String(n).padStart(2, '0');
    return `${d.getFullYear()}-${pad(d.getMonth() + 1)}-${pad(d.getDate())} ${pad(d.getHours())}:${pad(d.getMinutes())}`;
  } catch {
    return iso;
  }
}

function toReadableSize(bytes: number): string {
  if (bytes === 0) return '0 B';
  const units = ['B', 'KB', 'MB', 'GB', 'TB'];
  const i = Math.min(Math.floor(Math.log2(bytes) / 10), units.length - 1);
  const val = bytes / Math.pow(1024, i);
  return (val < 10 ? val.toFixed(1) : Math.round(val).toString()) + ' ' + units[i];
}

export function showRenameModal(path: string): void {
  const t = i18n();
  const filename = path.split('/').pop() ?? path;
  const modal = makeModal('rename-modal');
  const content = modal.querySelector('.modal-content')!;
  content.innerHTML = `
    <form hx-post="/files/rename" hx-target="#view" hx-swap="outerHTML"
          hx-on::after-request="this.closest('.modal').dispatchEvent(new Event('Close'))">
      <input type="hidden" name="old" value="${esc(path)}">
      <div style="display:flex">
        <input class="form-control " type="text" name="new" value="${esc(filename)}">
        <button class="btn btn-modal-confirm " type="submit">${esc(t.modal_confirm)}</button>
      </div>
    </form>`;
  appendModal(modal);
  htmx.process(modal);
  (modal.querySelector('input[name="new"]') as HTMLInputElement | null)?.select();
}

export function showNewFileModal(): void {
  const t = i18n();
  const modal = makeModal('new-file-modal');
  const content = modal.querySelector('.modal-content')!;
  content.innerHTML = `
    <span class="modal-title-bar ">
      <span class="bold">${esc(t.modal_file)}</span>
      <div class="btn-modal-close " data-close-modal="true"><i class="bx bx-x"></i></div>
    </span>
    <form hx-post="/files/new" hx-swap="none"
          hx-on::after-request="this.closest('.modal').dispatchEvent(new Event('Close'))">
      <div style="display:flex">
        <input class="form-control " type="text" name="new-file" placeholder="${esc(t.placeholder_newfile)}">
        <button class="btn btn-modal-confirm " type="submit">${esc(t.modal_create)}</button>
      </div>
    </form>`;
  appendModal(modal);
  htmx.process(modal);
  (modal.querySelector('input') as HTMLInputElement | null)?.focus();
}

export function showNewFolderModal(): void {
  const t = i18n();
  const modal = makeModal('new-folder-modal');
  const content = modal.querySelector('.modal-content')!;
  content.innerHTML = `
    <span class="modal-title-bar ">
      <span class="bold">${esc(t.modal_folder)}</span>
      <div class="btn-modal-close " data-close-modal="true"><i class="bx bx-x"></i></div>
    </span>
    <form hx-post="/folders/new" hx-swap="none"
          hx-on::after-request="this.closest('.modal').dispatchEvent(new Event('Close'))">
      <div style="display:flex">
        <input class="form-control " type="text" name="new-folder" placeholder="${esc(t.placeholder_newfoler)}">
        <button class="btn btn-modal-confirm " type="submit">${esc(t.modal_create)}</button>
      </div>
    </form>`;
  appendModal(modal);
  htmx.process(modal);
  (modal.querySelector('input') as HTMLInputElement | null)?.focus();
}

export function showFileDetailModal(
  name: string, mtime: string, atime: string, size: string, mime: string,
): void {
  const t = i18n();
  const modal = makeModal('file-detail-modal');
  const content = modal.querySelector('.modal-content')!;
  const readableSize = toReadableSize(parseInt(size, 10) || 0);
  content.innerHTML = `
    <span class="bold">${esc(t.modal_detail)}</span><br>
    <table><tbody>
      <tr><td>${esc(t.detail_filename)}</td><td>${esc(name)}</td></tr>
      <tr><td>${esc(t.detail_modified)}</td><td>${esc(formatDate(mtime))}</td></tr>
      <tr><td>${esc(t.detail_accessed)}</td><td>${esc(formatDate(atime))}</td></tr>
      <tr><td>${esc(t.detail_size)}</td><td>${esc(readableSize)}</td></tr>
      <tr><td>${esc(t.detail_content_type)}</td><td>${esc(mime)}</td></tr>
    </tbody></table>`;
  appendModal(modal);
}
