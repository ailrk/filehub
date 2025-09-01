'use strict';
import * as DesktopLocale from './handlers/desktop/locale.js';
import * as DesktopContextmenu from './handlers/desktop/contextmenu.js';
import * as DesktopSelected from './handlers/desktop/selected.js';
import * as DesktopDrag from './handlers/desktop/drag.js';
import * as DesktopScroll from './handlers/desktop/scroll.js';
import * as MobileCloseSidebar from './handlers/mobile/closeSidebar.js';
import * as MobileSelected from './handlers/mobile/selected.js';
import * as Cookie from './cookie.js';
import Viewer from './viewer.js';
// import * as Debug from './debug.js';
// Debug.init()
const ballonWaitTime = 2000;
let viewer = null;
let display = Cookie.getCookie('display');
document.addEventListener("DOMContentLoaded", () => {
    if (display == 'Desktop') {
        document.addEventListener('click', closeDropdowns);
        DesktopContextmenu.register();
        DesktopSelected.register();
        DesktopDrag.register();
        DesktopScroll.register();
        DesktopLocale.register();
    }
    if (display == 'Mobile') {
        document.addEventListener('click', closePanel);
        MobileCloseSidebar.register();
        MobileSelected.register();
        const table = document.querySelector('#table'); // change selector to fit your layout
        table.addEventListener('contextmenu', e => e.preventDefault());
    }
    /* HX-Trigger */
    document.addEventListener('Dummy', (e) => { console.log("testing dummy event", e.detail); });
    document.addEventListener('ViewerInited', (e) => initViewer(e.detail));
    document.addEventListener('Opened', open);
    document.addEventListener('ThemeChanged', reloadTheme);
    document.addEventListener('UIComponentReloaded', reloadUIComponent);
    /* Preserve scroll positions */
    document.body.addEventListener('htmx:responseError', handleError);
    document.addEventListener('htmx:afterOnLoad', restoreViewScrollTop);
    document.addEventListener('htmx:afterOnLoad', restoreViewScrollTop);
    document.addEventListener('htmx:beforeRequest', saveViewScrollTop);
    document.addEventListener('htmx:afterSettle', closeDropdowns);
    removeClassOnIndex();
});
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
function reloadTheme() {
    const oldLink = document.querySelector('link[rel="stylesheet"][href*="/theme.css"]');
    if (!oldLink)
        return;
    const newLink = oldLink.cloneNode();
    newLink.href = '/theme.css?v=' + Date.now(); // cache-busting
    newLink.onload = () => {
        oldLink.remove(); // remove old stylesheet after new one loads
        removeClassOnIndex(); // remove animation class.
    };
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
function pathStartsWith(e, paths) {
    let result = false;
    for (let i = 0; i < paths.length; ++i) {
        let path = paths[i];
        let b = e.detail.pathInfo.finalRequestPath.startsWith(path);
        result = result && b;
    }
    return result;
}
function saveViewScrollTop(e) {
    // We only save the scroll on mutations.
    let verb = e.detail.requestConfig.verb;
    if (verb == 'post' || verb == 'delete' || verb == 'put') {
        const scrollTop = document.querySelector('#view').scrollTop;
        localStorage.setItem("#view-scrollTop", `${scrollTop}`);
    }
}
function restoreViewScrollTop(e) {
    // Changing target or directory should not also preserve the scroll from the previous directory.
    const paths = ['/target/change',
        '/cd'
    ];
    if (pathStartsWith(e, paths)) {
        localStorage.removeItem("#view-scrollTop");
        return;
    }
    const saved = localStorage.getItem("#view-scrollTop");
    localStorage.removeItem("#view-scrollTop");
    if (saved !== null) {
        document.querySelector('#view').scrollTop = parseInt(saved, 10);
    }
}
function initViewer(o) {
    viewer = new Viewer(o.resources, { index: o.index });
    viewer.show();
    // Make sure the viewer content has context menu enabled.
    viewer.currentContent.addEventListener("contextmenu", event => {
        event.stopImmediatePropagation();
    }, { capture: true });
}
function open(e) {
    let payload = e.detail;
    let target = null;
    switch (payload.tgt) {
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
            htmx.ajax('GET', `/viewer?${query.toString()}`, { target: 'body',
                source: 'body',
                swap: 'none'
            });
            return;
    }
    window.open(`/serve?file=${payload.path}`, target);
}
function closeDropdowns(e) {
    const target = e.target;
    const table = document.getElementById('table');
    // only close when dropdown exists.
    if (document.querySelectorAll('.dropdown *').length == 0 &&
        document.querySelectorAll('.dropdown-content *').length == 0)
        return;
    if (!target.matches('.dropdown *') && !target.matches('.dropdown-content *')) {
        let dropdownContents = document.getElementsByClassName('dropdown-content'); // close dropdown
        for (let i = 0; i < dropdownContents.length; ++i) {
            let ele = dropdownContents[i];
            ele.dispatchEvent(new Event('Close'));
        }
    }
    table.style.pointerEvents = "auto"; // restore click event on table.
}
function closePanel(e) {
    if (e.target.closest('#control-panel-btn'))
        return;
    if (e.target.closest('#locale-langauge-btn'))
        return;
    let panels = document.querySelectorAll('.panel');
    let overlay = document.getElementById('overlay');
    panels.forEach(panel => {
        if (panel && panel.classList.contains('show')) {
            panel.classList.remove('show');
            overlay.classList.remove('show');
        }
    });
}
/* Remove all class on #index */
function removeClassOnIndex() {
    const el = document.querySelector("#index");
    if (el) {
        el.addEventListener("animationend", () => {
            el.classList = "";
        });
    }
}
function reloadUIComponent(e) {
    let payload = e.detail;
    switch (payload) {
        case 'UIComponentView':
            htmx.ajax('GET', `/refresh?component=UIComponentView`, { target: '#view',
                source: '#view',
                swap: 'outerHtml'
            });
            break;
        case 'UIComponentSideBar':
            htmx.ajax('GET', `/refresh?component=UIComponentSideBar`, { target: '#side-bar',
                source: '#side-bar',
                swap: 'outerHtml'
            });
            break;
        case 'UIComponentContronPanel':
            htmx.ajax('GET', `/refresh?component=UIComponentContronPanel`, { target: '#control-panel',
                source: '#control-panel',
                swap: 'outerHtml'
            });
            break;
    }
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
