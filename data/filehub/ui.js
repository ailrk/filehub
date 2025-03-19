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
    initImageViewer: (e) => initImageViewer(e.detail),
    openImage: function (e) {
        openImage(e.detail.path, e.detail.index);
    }
};
/* Global event listener */
window.addEventListener('click', eventHandlers.closeDropdown);
window.addEventListener('contextmenu', e => e.preventDefault());
window.addEventListener('FileExists', () => alert("Error: file exists"));
window.addEventListener('InvalidPath', () => alert("Error: invalid path"));
window.addEventListener('InvalidDir', () => alert("Error: invalid directory"));
/* Image viewer events */
window.addEventListener('InitImageViewer', eventHandlers.initImageViewer);
window.addEventListener('OpenImage', eventHandlers.openImage);
/* Close all dropdowns */
function closeDropdowns() {
    let dropdownContents = document.getElementsByClassName('dropdown-content');
    for (let i = 0; i < dropdownContents.length; ++i) {
        let ele = dropdownContents[i];
        ele.dispatchEvent(new Event('CLOSE'));
    }
}
function updateImageViewer(o) {
    if (viewer !== null) {
        viewer.update(o.images);
        viewer.index = o.index;
    }
}
function initImageViewer(o) {
    console.log('hi', o);
    updateImageViewer(o);
    closeDropdowns();
    viewer = new Viewer(o.images, { index: o.index });
    viewer.show();
}
/* Open a image. If the viewer is already initialized, show the image directly.
 * Otherwise request the backend for the image list to construct a new viewer.
 * */
function openImage(path, index) {
    console.log(path, index);
    let query = new URLSearchParams({
        file: encodeURIComponent(path)
    });
    htmx.ajax('GET', `/img-viewer?${query.toString()}`, { target: 'body', swap: 'none' });
}
