'use strict';

declare var htmx: any;

import * as Balloon from './balloon.js';
import * as Cookie from './cookie.js';
import * as DesktopContextmenu from './handlers/desktop/contextmenu.js';
import * as DesktopDrag from './handlers/desktop/drag.js';
import * as DesktopScroll from './handlers/desktop/scroll.js';
import * as DesktopSelected from './handlers/desktop/selected.js';
import * as MobileCloseSidebar from './handlers/mobile/closeSidebar.js';
import * as MobileSelected from './handlers/mobile/selected.js';
import * as MobilePanel from './handlers/mobile/panel.js';
import * as Modal from './modal.js';
import Viewer from './viewer.js';
import type { DeleteProgressed, MoveProgressed, Opened, PasteProgressed, TaskCompleted, TaskFailed, UploadProgressed, ViewerInited } from './def.js';
import { Display } from './def.js';

const balloonWaitTime = 2000;
let viewer: Viewer | null = null;
let display: Display = Cookie.getCookie('display')! as Display;
let evtSource: EventSource | null;
const taskLabels: Map<number, string> = new Map();

document.addEventListener('DOMContentLoaded', () => {
  Modal.register();

  if (display === 'Desktop') {
    DesktopContextmenu.register();
    DesktopSelected.register();
    DesktopDrag.register();
    DesktopScroll.register();
    document.addEventListener('click', closeDropdowns);
    document.addEventListener('htmx:afterSettle', closeDropdowns);
  }

  if (display === 'Mobile') {
    MobileCloseSidebar.register();
    MobileSelected.register();
    MobilePanel.register();
    document.querySelector('#table')?.addEventListener('contextmenu', e => e.preventDefault());
  }

  document.addEventListener('filehub:sidebarToggled', handleSidebarToggled);
  document.addEventListener('filehub:themeChanged', handleThemeChanged);
  document.addEventListener('filehub:viewerInited', initViewer);
  document.addEventListener('filehub:opened', open);
  document.body.addEventListener('htmx:responseError', handleError);

  document.body.addEventListener('htmx:afterSettle', () => {
    if (display === 'Desktop') {
      DesktopScroll.rebind();
      DesktopSelected.collectFromHtml();
    }
  });
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
  const oldLink = document.querySelector('link[rel="stylesheet"][href*="/theme.css"]') as HTMLLinkElement | null;
  if (!oldLink) return;
  const newLink = oldLink.cloneNode() as HTMLLinkElement;
  newLink.href = '/theme.css?v=' + Date.now();
  newLink.onload = () => oldLink.remove();
  oldLink.parentNode!.insertBefore(newLink, oldLink.nextSibling);
}


function handleSidebarToggled() {
  document.getElementById('index')?.classList.toggle('sidebar-collapsed');
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


function initViewer(e: any) {
  const o: ViewerInited = e.detail;
  viewer = new Viewer(o.resources, { index: o.index });
  viewer.show();
  viewer.currentContent!.addEventListener('contextmenu', ev => {
    ev.stopImmediatePropagation();
  }, { capture: true });
}


function open(e: any) {
  const payload = e.detail as Opened;
  let target: string | null = null;
  switch (payload.tgt) {
    case 'OpenDOMSelf':        target = '_self';         break;
    case 'OpenDOMBlank':       target = '_blank';        break;
    case 'OpenDOMParent':      target = '_parent';       break;
    case 'OpenDOMTop':         target = '_top';          break;
    case 'OpenDOMUnfencedTop': target = '_unfencedTop';  break;
    case 'OpenViewer': {
      const query = new URLSearchParams({ file: payload.path });
      htmx.ajax('GET', `/viewer?${query.toString()}`, { target: 'body', source: 'body', swap: 'none' });
      return;
    }
  }
  window.open(`/serve?file=${payload.path}`, target);
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


function startListenSSE() {
  if (!evtSource) evtSource = new EventSource('/listen');

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
