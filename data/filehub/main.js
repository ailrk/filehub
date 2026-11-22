'use strict';
import * as Balloon from './balloon.js';
import * as Cookie from './cookie.js';
import * as DesktopContextmenu from './handlers/desktop/contextmenu.js';
import * as DesktopDrag from './handlers/desktop/drag.js';
import * as DesktopScroll from './handlers/desktop/scroll.js';
import * as DesktopSelected from './handlers/desktop/selected.js';
import * as DesktopSort from './handlers/desktop/sort.js';
import * as MobileCloseSidebar from './handlers/mobile/closeSidebar.js';
import * as MobileSelected from './handlers/mobile/selected.js';
import * as MobilePanel from './handlers/mobile/panel.js';
import * as Modal from './modal.js';
import * as ModalBuilder from './modal-builder.js';
import Viewer from './viewer.js';
import { i18n } from './i18n.js';
const balloonWaitTime = 2000;
let viewer = null;
let display = Cookie.getCookie('display');
let evtSource;
const taskLabels = new Map();
document.addEventListener('DOMContentLoaded', () => {
    Modal.register();
    if (display === 'Desktop') {
        DesktopContextmenu.register();
        DesktopSelected.register();
        DesktopDrag.register();
        DesktopScroll.register();
        DesktopSort.register();
        document.addEventListener('click', closeDropdowns);
        document.addEventListener('click', onActionClick);
        document.addEventListener('htmx:afterSettle', closeDropdowns);
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
    document.body.addEventListener('htmx:beforeRequest', (e) => {
        const targetIsIndex = e.detail.target?.id === 'index';
        const triggerWantsSpinner = e.detail.elt?.getAttribute('hx-indicator') === '#global-spinner';
        if (!targetIsIndex && !triggerWantsSpinner)
            return;
        document.getElementById('view')?.classList.add('view-loading');
        document.getElementById('global-spinner')?.classList.add('htmx-request');
    });
    const clearViewLoading = () => {
        document.getElementById('view')?.classList.remove('view-loading');
        document.getElementById('global-spinner')?.classList.remove('htmx-request');
    };
    document.body.addEventListener('htmx:afterSwap', clearViewLoading);
    document.body.addEventListener('htmx:responseError', clearViewLoading);
    document.body.addEventListener('htmx:afterSettle', () => {
        if (display === 'Desktop') {
            DesktopScroll.rebind();
            DesktopSelected.collectFromHtml();
            updatePluginToolbarActions();
        }
        initTomlEditors();
        initPluginToggles();
        if (display === 'Desktop')
            initVideoHover();
    });
    initTomlEditors();
    initPluginToggles();
    if (display === 'Desktop')
        initVideoHover();
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
function handleThemeChanged() {
    const oldLink = document.querySelector('link[rel="stylesheet"][href*="/theme.css"]');
    if (!oldLink)
        return;
    const newLink = oldLink.cloneNode();
    newLink.href = '/theme.css?v=' + Date.now();
    newLink.onload = () => oldLink.remove();
    oldLink.parentNode.insertBefore(newLink, oldLink.nextSibling);
}
function onActionClick(e) {
    const btn = e.target.closest('[data-action]');
    if (!btn)
        return;
    switch (btn.dataset.action) {
        case 'new-folder':
            ModalBuilder.showNewFolderModal();
            break;
        case 'new-file':
            ModalBuilder.showNewFileModal();
            break;
        case 'toggle-theme':
            handleToggleTheme(btn);
            break;
        case 'toggle-sidebar':
            handleToggleSidebar();
            break;
    }
}
async function handleToggleTheme(btn) {
    const current = btn.dataset.currentTheme;
    const next = current === 'light' ? 'dark' : 'light';
    btn.dataset.currentTheme = next;
    const icon = btn.querySelector('i');
    if (icon)
        icon.className = next === 'dark' ? 'bx bxs-sun' : 'bx bxs-moon';
    const t = i18n();
    btn.dataset.tooltip = next === 'dark' ? t.control_panel_light : t.control_panel_dark;
    await fetch('/theme/toggle');
    document.dispatchEvent(new Event('filehub:themeChanged'));
}
function handleToggleSidebar() {
    document.getElementById('index')?.classList.toggle('sidebar-collapsed');
    fetch('/sidebar/toggle');
}
function handleError(e) {
    const xhr = e.detail.xhr;
    const status = xhr.status;
    let message = xhr.responseText;
    if (!message) {
        const statusMessages = {
            400: 'Bad Request', 401: 'Unauthorized', 402: 'Payment required',
            403: 'Forbidden resource', 404: 'Resource not found', 500: 'Server internal error',
        };
        message = statusMessages[status] ?? 'Something went wrong';
    }
    Balloon.pushBalloon({ kind: 'ErrorMsg', msg: `${message} ${status}`, duration: balloonWaitTime });
}
function openViewerOnClick(e) {
    const el = e.target.closest('[data-action="open-viewer"]');
    if (!el)
        return;
    const path = (el.closest('[data-path]') ?? el).dataset.path ?? '';
    if (!path)
        return;
    viewer = new Viewer(path);
    viewer.show();
}
function openBlankOnClick(e) {
    const el = e.target.closest('[data-action="open-blank"]');
    if (!el)
        return;
    const path = (el.closest('[data-path]') ?? el).dataset.path ?? '';
    if (path)
        window.open(`/serve?file=${path}`, '_blank');
}
function initVideoHover() {
    document.querySelectorAll('video.thumbnail-video').forEach(video => {
        if (video._hoverInited)
            return;
        video._hoverInited = true;
        const thumb = video.closest('.thumbnail');
        if (!thumb)
            return;
        thumb.addEventListener('mouseenter', () => { video.play().catch(() => { }); });
        thumb.addEventListener('mouseleave', () => { video.pause(); video.currentTime = 0; });
    });
}
function closeDropdowns(e) {
    const target = e?.target;
    if (target?.matches('.dropdown *') || target?.matches('.dropdown-content *'))
        return;
    document.getElementById('contextmenu')?.dispatchEvent(new Event('Close'));
    const table = document.getElementById('table');
    if (table)
        table.style.pointerEvents = 'auto';
}
function htmxProcessOOB(data) {
    if (data.htmxResponse) {
        htmx.swap(document.body, data.htmxResponse, { swapStyle: 'none' });
    }
}
// ─── TOML editor ─────────────────────────────────────────────────────────────
function highlightToml(raw) {
    const esc = (s) => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    const hlValue = (val) => {
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
function autoResizeTextarea(textarea) {
    textarea.style.height = 'auto';
    textarea.style.height = textarea.scrollHeight + 'px';
}
function initTomlEditor(textarea) {
    if (textarea._tomlInited)
        return;
    textarea._tomlInited = true;
    const pre = textarea.previousElementSibling;
    if (!pre?.classList.contains('toml-highlight'))
        return;
    const update = () => {
        pre.innerHTML = highlightToml(textarea.value);
        autoResizeTextarea(textarea);
    };
    textarea.addEventListener('input', update);
    textarea.addEventListener('keydown', (e) => {
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
    document.querySelectorAll('textarea.toml-textarea')
        .forEach(initTomlEditor);
}
function updatePluginToolbarActions() {
    const container = document.getElementById('plugin-toolbar-actions');
    if (!container)
        return;
    const plugins = (typeof filehubPlugins !== 'undefined' && filehubPlugins) ? filehubPlugins : [];
    const selected = Array.from(document.querySelectorAll('#table .table-item.selected'));
    const files = selected.filter(el => el.dataset.ftype !== 'dir');
    container.innerHTML = '';
    if (files.length !== 1)
        return;
    const file = files[0];
    const filePath = file.dataset.path ?? '';
    const fileName = file.dataset.name ?? '';
    const fileMime = file.dataset.mime ?? '';
    for (const plugin of plugins) {
        for (const entry of plugin.toolbar) {
            if (!fileMime.startsWith(entry.mime))
                continue;
            const btn = document.createElement('button');
            btn.className = 'btn btn-control plugin-toolbar-btn';
            btn.textContent = entry.label;
            btn.addEventListener('click', async () => {
                const needsShare = entry.url.includes('{share_url}');
                const enc = encodeURIComponent(filePath);
                const downloadUrl = `${window.location.origin}/download?file=${enc}`;
                if (needsShare) {
                    const lookupRes = await fetch(`/share/for-file?file=${enc}`);
                    let hash;
                    if (lookupRes.ok) {
                        hash = await lookupRes.text();
                    }
                    else {
                        if (!confirm(`"${fileName}" is not shared. Create a public share link?`))
                            return;
                        const createRes = await fetch(`/share/auto?file=${enc}`, { method: 'POST' });
                        if (!createRes.ok) {
                            alert('Failed to create share link.');
                            return;
                        }
                        hash = await createRes.text();
                    }
                    const shareUrl = `${window.location.origin}/s/${hash}/file?file=${enc}`;
                    const url = entry.url
                        .replace('{file_path}', encodeURIComponent(filePath))
                        .replace('{file_name}', encodeURIComponent(fileName))
                        .replace('{mime_type}', encodeURIComponent(fileMime))
                        .replace('{download_url}', encodeURIComponent(downloadUrl))
                        .replace('{share_url}', encodeURIComponent(shareUrl));
                    window.open(url, '_blank');
                }
                else {
                    const url = entry.url
                        .replace('{file_path}', encodeURIComponent(filePath))
                        .replace('{file_name}', encodeURIComponent(fileName))
                        .replace('{mime_type}', encodeURIComponent(fileMime))
                        .replace('{download_url}', encodeURIComponent(downloadUrl))
                        .replace('{share_url}', '');
                    window.open(url, '_blank');
                }
            });
            container.appendChild(btn);
        }
    }
}
function initPluginToggles() {
    document.querySelectorAll('.plugin-item-header').forEach(header => {
        if (header._toggleInited)
            return;
        header._toggleInited = true;
        header.addEventListener('click', (e) => {
            if (e.target.closest('button'))
                return;
            const item = header.closest('.plugin-item');
            if (!item)
                return;
            item.classList.toggle('expanded');
            if (item.classList.contains('expanded')) {
                initTomlEditors();
            }
        });
    });
}
// ─────────────────────────────────────────────────────────────────────────────
function startListenSSE() {
    if (!evtSource)
        evtSource = new EventSource('/listen');
    evtSource.onmessage = event => {
        console.log('New update:', JSON.parse(event.data));
    };
    evtSource.addEventListener('Pong', () => console.log('pong'));
    evtSource.addEventListener('TaskCompleted', e => {
        const data = JSON.parse(e.data);
        const label = taskLabels.get(data.taskId) ?? 'Task';
        taskLabels.delete(data.taskId);
        htmxProcessOOB(data);
        Balloon.deleteLongLivedBalloon(data.taskId);
        Balloon.pushBalloon({ kind: 'InfoMsg', msg: `${label} complete`, duration: 3000 });
    });
    evtSource.addEventListener('TaskFailed', e => {
        const data = JSON.parse(e.data);
        const label = taskLabels.get(data.taskId) ?? 'Task';
        taskLabels.delete(data.taskId);
        htmxProcessOOB(data);
        Balloon.deleteLongLivedBalloon(data.taskId);
        Balloon.pushBalloon({ kind: 'ErrorMsg', msg: `${label} failed`, duration: 5000 });
    });
    evtSource.addEventListener('DeleteProgressed', e => {
        const data = JSON.parse(e.data);
        taskLabels.set(data.taskId, 'Delete');
        htmxProcessOOB(data);
        Balloon.pushBalloon({ kind: 'ProgressedMsg', msg: 'Deleting', taskId: data.taskId, progress: data.progress });
    });
    evtSource.addEventListener('PasteProgressed', e => {
        const data = JSON.parse(e.data);
        taskLabels.set(data.taskId, 'Paste');
        htmxProcessOOB(data);
        Balloon.pushBalloon({ kind: 'ProgressedMsg', msg: 'Pasting', taskId: data.taskId, progress: data.progress });
    });
    evtSource.addEventListener('MoveProgressed', e => {
        const data = JSON.parse(e.data);
        taskLabels.set(data.taskId, 'Move');
        htmxProcessOOB(data);
        Balloon.pushBalloon({ kind: 'ProgressedMsg', msg: 'Moving', taskId: data.taskId, progress: data.progress });
    });
    evtSource.addEventListener('UploadProgressed', e => {
        const data = JSON.parse(e.data);
        taskLabels.set(data.taskId, 'Upload');
        htmxProcessOOB(data);
        Balloon.pushBalloon({ kind: 'ProgressedMsg', msg: 'Uploading', taskId: data.taskId, progress: data.progress });
    });
}
