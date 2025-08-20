'use strict';

declare var htmx: any;

import * as DesktopContextmenu from './handlers/desktop/contextmenu.js';
import * as DesktopSelected from './handlers/desktop/selected.js';
import * as DesktopDrag from './handlers/desktop/drag.js';
import * as MobileCloseSidebar from './handlers/mobile/closeSidebar.js';
import * as MobileSelected from './handlers/mobile/selected.js';
import * as Cookie from './cookie.js';
import { Display } from './def.js';
import type { Opened, UIComponent, ViewerInited } from './def.js';
import Viewer from './viewer.js';


// import * as Debug from './debug.js';
// Debug.init()
const ballonWaitTime = 2000;
let viewer: Viewer | null = null;
let display: Display = Cookie.getCookie('display')! as Display



if (display == 'Desktop') {
  document.addEventListener('click', closeDropdowns);
  DesktopContextmenu.register();
  DesktopSelected.register();
  DesktopDrag.register();
}


if (display == 'Mobile') {
  document.addEventListener('click', closePanel)
  MobileCloseSidebar.register();
  MobileSelected.register();
  const table = document.querySelector('#table'); // change selector to fit your layout
  table!.addEventListener('contextmenu', e => e.preventDefault());
}


/* HX-Trigger */
document.addEventListener('Dummy', (e: any) => { console.log("testing dummy event", e.detail); });
document.addEventListener('ViewerInited', (e: any) => initViewer(e.detail));
document.addEventListener('Opened', open);
document.addEventListener('ThemeChanged', reloadTheme);
document.addEventListener('UIComponentReloaded', reloadUIComponent);


/* Preserve scroll positions */
document.body.addEventListener('htmx:responseError', handleError);
document.addEventListener('htmx:afterOnLoad', restoreViewScrollTop);
document.addEventListener('htmx:afterOnLoad', restoreViewScrollTop);
document.addEventListener('htmx:beforeRequest', saveViewScrollTop);
document.addEventListener('htmx:afterSettle', closeDropdowns);


function reloadTheme() {
  const oldLink = document.querySelector('link[rel="stylesheet"][href*="/theme.css"]') as HTMLLinkElement;
  if (!oldLink) return;

  const newLink = oldLink.cloneNode() as HTMLLinkElement;
  newLink.href = '/theme.css?v=' + Date.now(); // cache-busting
  newLink.onload = () => oldLink.remove(); // remove old stylesheet after new one loads

  oldLink.parentNode!.insertBefore(newLink, oldLink.nextSibling);
}


function handleError(e: any) {
  const xhr = e.detail.xhr;
  const status = xhr.status;
  let message = xhr.responseText;
  if (message === undefined || message === ""){
    if (status === 400) {
      message = "Bad Request"
    } else if (status === 401) {
      message = "Unauthorized"
    } else if (status === 402) {
      message = "Payment required"
    } else if (status === 403) {
      message = "Forbidden resource"
    } else if (status === 404) {
      message = "Resource not found"
    } else if (status === 500) {
      message = "Server internal error"
    } else {
      message = "Something went wrong"
    }
  }
  showBalloon(`${message} ${status}`, ballonWaitTime);
}


function pathStartsWith(e: any, paths: string[]) {
  let result = false;
  for (let i = 0; i < paths.length; ++i) {
    let path = paths[i];
    let b = e.detail.pathInfo.finalRequestPath.startsWith(path) as boolean;
    result = result && b;
  }
  return result;
}


function saveViewScrollTop(e: any) {
  // We only save the scroll on mutations.
  let verb: string = e.detail.requestConfig.verb;
  if (verb == 'post' || verb == 'delete' || verb == 'put') {
    const scrollTop = document.querySelector('#view')!.scrollTop;
    localStorage.setItem("#view-scrollTop", `${scrollTop}`);
  }
}


function restoreViewScrollTop(e: any) {
  // Changing target or directory should not also preserve the scroll from the previous directory.
  const paths =
    [ '/target/change',
      '/cd'
    ];
  if (pathStartsWith(e, paths)) {
    localStorage.removeItem("#view-scrollTop");
    return;
  }
  const saved = localStorage.getItem("#view-scrollTop");
  localStorage.removeItem("#view-scrollTop");
  if (saved !== null) {
    document.querySelector('#view')!.scrollTop = parseInt(saved, 10);
  }
}


function initViewer(o: ViewerInited) {
  viewer = new Viewer(o.resources, { index: o.index });
  viewer.show();

  // Make sure the viewer content has context menu enabled.
  viewer!.currentContent!.addEventListener(
    "contextmenu", event => {
      event.stopImmediatePropagation();
    }, { capture: true }
  )
}


function open(e: any) {
  let payload = e.detail as Opened;
  let target = null;
  switch (payload.tgt)
  {
    case "OpenDOMSelf":
      target = "_self";
      break;
    case "OpenDOMBlank":
      target = "_blank";
      break;
    case "OpenDOMParent":
      target = "_parent";
      break;
    case "OpenDOMTop":
      target = "_top";
      break;
    case "OpenDOMUnfencedTop":
      target = "_unfencedTop";
      break;
    case "OpenViewer":
      let query = new URLSearchParams({ file: payload.path });
      htmx.ajax('GET', `/viewer?${query.toString()}`, { target: 'head', swap: 'none'});
      return;
  }
  window.open(`/serve?file=${payload.path}`, target)
}


function closeDropdowns(e: any) {
  const target = e.target as Element;
  const table = document.getElementById('table') as HTMLElement

  // only close when dropdown exists.
  if (document.querySelectorAll('.dropdown *').length == 0 &&
      document.querySelectorAll('.dropdown-content *').length == 0)
    return;

  if (!target.matches('.dropdown *') && !target.matches('.dropdown-content *')) {
    let dropdownContents = document.getElementsByClassName('dropdown-content') // close dropdown
    for (let i = 0; i < dropdownContents.length; ++i) {
      let ele = dropdownContents[i];
      ele.dispatchEvent(new Event('Close'));
    }
  }
  table.style.pointerEvents = "auto"; // restore click event on table.
}


function closePanel (e: MouseEvent) {
  if ((e.target as Element).closest('#control-panel-btn')) {
    return;
  }
  let controlPanel = document.getElementById('control-panel')
  let overlay = document.getElementById('overlay')
  if (controlPanel && controlPanel.classList.contains('show')) {
    controlPanel.classList.remove('show')
    overlay!.classList.remove('show')
  }
}


function reloadUIComponent (e: any) {
  let payload = e.detail as UIComponent;
  switch (payload) {
    case 'UIComponentView':
      htmx.ajax('GET', `/refresh?component=UIComponentView`, { target: '#view', swap: 'outerHtml'});
      break;
    case 'UIComponentSideBar':
      htmx.ajax('GET', `/refresh?component=UIComponentSideBar`, { target: '#side-bar', swap: 'outerHtml'});
      break;
    case 'UIComponentContronPanel':
      htmx.ajax('GET', `/refresh?component=UIComponentContronPanel`, { target: '#control-panel', swap: 'outerHtml'});
      break;
  }
}


function showBalloon(message: string, duration = 3000) {
  const body = document.getElementsByTagName('body')[0]
  const balloon = document.createElement('div')
  balloon.className = 'balloon'
  balloon.textContent = message

  body.appendChild(balloon)

  setTimeout(() => {
    balloon.style.opacity = '0'
    balloon.style.transition = 'opacity 0.5s'
    setTimeout(() => balloon.remove(), 500)
  }, duration)
}


/* Register service worker, required for PWA support. */
if ('serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register( '/static/serviceWorker.js', { type: "module" })
      .then(reg => console.log('Service worker registered:', reg))
      .catch(err => console.error('Service worker registration failed:', err));
  });
}


/* Reinitialize the resolution again before refresh the page
 * This happens before the browser sending the request to reload the page,
 * so we can guarantee when the page is reloaded the resolution is up to date.
 * */
window.addEventListener("beforeunload", async (_) => {
  await fetch('/init',
    { method: 'POST',
      headers: { "Content-Type": "application/x-www-form-urlencoded", },
      body: new URLSearchParams({ res: window.innerWidth + 'x' + window.innerHeight })
    })
})
