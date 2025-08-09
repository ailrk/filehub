'use strict';
/* Desktop */
import * as Desktop from './handlers/desktop.js';
/* Mobile */
import * as Mobile from './handlers/mobile.js';
import * as ErrorsHandlers from './handlers/errors.js';
import * as ViewerHandlers from './handlers/viewer.js';
import * as Cookie from './cookie.js';
// import * as Debug from './debug.js';
// Debug.init()
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
ErrorsHandlers.register();
ViewerHandlers.register();
document.addEventListener('ThemeChanged', _ => {
    const oldLink = document.querySelector('link[rel="stylesheet"][href*="/theme.css"]');
    console.log(oldLink);
    if (!oldLink)
        return;
    const newLink = oldLink.cloneNode();
    newLink.href = '/theme.css?v=' + Date.now(); // cache-busting
    newLink.onload = () => oldLink.remove(); // remove old stylesheet after new one loads
    oldLink.parentNode.insertBefore(newLink, oldLink.nextSibling);
});
/* Preserve scroll positions */
document.addEventListener('htmx:afterOnLoad', restoreViewScrollTop);
document.addEventListener('htmx:beforeRequest', saveViewScrollTop);
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
