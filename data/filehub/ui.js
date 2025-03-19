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
    initImageViewer: (e) => initImageViewer(e.detail)
};
/* Global event listener */
window.addEventListener('click', eventHandlers.closeDropdown);
window.addEventListener('contextmenu', e => e.preventDefault());
window.addEventListener('FileExists', () => alert("Error: file exists"));
window.addEventListener('InvalidPath', () => alert("Error: invalid path"));
window.addEventListener('InvalidDir', () => alert("Error: invalid directory"));
/* Image viewer events */
window.addEventListener('InitImageViewer', eventHandlers.initImageViewer);
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
    updateImageViewer(o);
    closeDropdowns();
    viewer = new Viewer(o.images, { index: o.index });
    viewer.show();
}
