'use strict';
import Viewer from './viewer.js';
/* States */
let viewer = null;
/* Event handlers  */
let eventHandlers = {
    closeDropdown: function (e) {
        // when clicking outside of any area outside a dropdown or dropdown-content, close all dropdowns.
        const target = e.target;
        if (!target.matches('.dropdown *') && !target.matches('.dropdown-content *')) {
            closeDropdowns();
        }
    },
    initViewer: (e) => initViewer(e.detail),
    open: function (e) {
        open(e.detail.path);
    }
};
/* Global event listener */
window.addEventListener('click', eventHandlers.closeDropdown);
window.addEventListener('contextmenu', e => e.preventDefault());
window.addEventListener('FileExists', () => alert("Error: file exists"));
window.addEventListener('InvalidPath', () => alert("Error: invalid path"));
window.addEventListener('InvalidDir', () => alert("Error: invalid directory"));
/* Image viewer events */
window.addEventListener('InitViewer', eventHandlers.initViewer);
window.addEventListener('Open', eventHandlers.open);
/* Close all dropdowns */
function closeDropdowns() {
    let dropdownContents = document.getElementsByClassName('dropdown-content');
    for (let i = 0; i < dropdownContents.length; ++i) {
        let ele = dropdownContents[i];
        ele.dispatchEvent(new Event('CLOSE'));
    }
}
function initViewer(o) {
    closeDropdowns();
    viewer = new Viewer(o.resources, { index: o.index });
    viewer.show();
}
/* Open a image. If the viewer is already initialized, show the image directly.
 * Otherwise request the backend for the image list to construct a new viewer.
 * */
function open(path) {
    let query = new URLSearchParams({
        file: encodeURIComponent(path)
    });
    htmx.ajax('GET', `/viewer?${query.toString()}`, { target: 'body', swap: 'none' });
}
