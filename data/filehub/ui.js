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
/* Register service worker, required for PWA support. */
if ('serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/static/filehub/serviceWorker.js', { type: "module" })
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
