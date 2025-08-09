'use strict';
import * as Desktop from './handlers/desktop.js';
import * as Mobile from './handlers/mobile.js';
import * as Cookie from './cookie.js';
import { closeDropdowns } from './handlers/desktop/closeDropdown.js';
import Viewer from './viewer.js';
// import * as Debug from './debug.js';
// Debug.init()
const ballonWaitTime = 2000;
let viewer = null;
let display = Cookie.getCookie('display');
/* Install handlers */
switch (display) {
    case 'Desktop':
        Desktop.register();
        break;
    case 'Mobile':
        Mobile.register();
        break;
    default:
        console.error('implementation error, no valid display type');
        break;
}
document.addEventListener('ViewerInited', (e) => initViewer(e.detail));
document.addEventListener('Open', (e) => open(e.detail.path));
/* Error handling */
document.body.addEventListener('htmx:responseError', handleError);
document.addEventListener('ThemeChanged', reloadTheme);
/* Preserve scroll positions */
document.addEventListener('htmx:afterOnLoad', restoreViewScrollTop);
document.addEventListener('htmx:beforeRequest', saveViewScrollTop);
function reloadTheme() {
    const oldLink = document.querySelector('link[rel="stylesheet"][href*="/theme.css"]');
    console.log(oldLink);
    if (!oldLink)
        return;
    const newLink = oldLink.cloneNode();
    newLink.href = '/theme.css?v=' + Date.now(); // cache-busting
    newLink.onload = () => oldLink.remove(); // remove old stylesheet after new one loads
    oldLink.parentNode.insertBefore(newLink, oldLink.nextSibling);
}
function handleError(e) {
    const xhr = e.detail.xhr;
    const status = xhr.status;
    let message = xhr.responseText;
    if (message === undefined || message === "") {
        if (status === 400) {
            message = "Bad Request";
        }
        else if (status === 401) {
            message = "Unauthorized";
        }
        else if (status === 402) {
            message = "Payment required";
        }
        else if (status === 403) {
            message = "Forbidden resource";
        }
        else if (status === 404) {
            message = "Resource not found";
        }
        else if (status === 500) {
            message = "Server internal error";
        }
        else {
            message = "Something went wrong";
        }
    }
    showBalloon(`${message} ${status}`, ballonWaitTime);
}
function saveViewScrollTop() {
    const scrollTop = document.querySelector('#view').scrollTop;
    console.log("save, ", scrollTop);
    localStorage.setItem("#view-scrollTop", `${scrollTop}`);
}
function restoreViewScrollTop() {
    const saved = localStorage.getItem("#view-scrollTop");
    console.log("restore, ", saved);
    localStorage.removeItem("#view-scrollTop");
    if (saved !== null) {
        document.querySelector('#view').scrollTop = parseInt(saved, 10);
    }
}
function initViewer(o) {
    closeDropdowns();
    viewer = new Viewer(o.resources, { index: o.index });
    console.log(viewer.currentContent);
    viewer.show();
    // Make sure the viewer content has context menu enabled.
    viewer.currentContent.addEventListener("contextmenu", event => {
        event.stopImmediatePropagation();
    }, { capture: true });
}
/* Open a image. If the viewer is already initialized, show the image directly.
 * Otherwise request the backend for the image list to construct a new viewer.
 *
 * The path is already percent encoded, we don't need to encode it here.
 *
 * NOTE:
 * The target is set to 'head'. htmx ajax must specify a target, but this ajax
 * call don't need a target at all. We pick head to avoid interference on
 * other event handlers.
 * */
function open(path) {
    let query = new URLSearchParams({ file: path });
    htmx.ajax('GET', `/viewer?${query.toString()}`, { target: 'head', swap: 'none' });
}
function showBalloon(message, duration = 3000) {
    const body = document.getElementsByTagName('body')[0];
    const balloon = document.createElement('div');
    balloon.className = 'balloon';
    balloon.textContent = message;
    body.appendChild(balloon);
    setTimeout(() => {
        balloon.style.opacity = '0';
        balloon.style.transition = 'opacity 0.5s';
        setTimeout(() => balloon.remove(), 500);
    }, duration);
}
/* Register service worker, required for PWA support. */
if ('serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/static/serviceWorker.js', { type: "module" })
            .then(reg => console.log('Service worker registered:', reg))
            .catch(err => console.error('Service worker registration failed:', err));
    });
}
/* Reinitialize the resolution again before refresh the page
 * This happens before the browser sending the request to reload the page,
 * so we can guarantee when the page is reloaded the resolution is up to date.
 * */
window.addEventListener("beforeunload", async (_) => {
    await fetch('/init', { method: 'POST',
        headers: { "Content-Type": "application/x-www-form-urlencoded", },
        body: new URLSearchParams({ res: window.innerWidth + 'x' + window.innerHeight })
    });
});
