'use strict';

import Viewer from './viewer.js';
declare var htmx: any;


/* States */
let viewer: Viewer | null = null;

/* Event handlers  */
let eventHandlers = {
  closeDropdown: function (e: any) {
    // when clicking outside of any area outside a dropdown or dropdown-content, close all dropdowns.
    const target = e.target as Element;
    if (!target.matches('.dropdown *') && !target.matches('.dropdown-content *')) {
      closeDropdowns()
    }
  },
  initImageViewer: (e: any) => initImageViewer(e.detail),
  openImage: function (e: any) {
    openImage(e.detail.path)
  }
}

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
  let dropdownContents = document.getElementsByClassName('dropdown-content')
  for (let i = 0; i < dropdownContents.length; ++i) {
    let ele = dropdownContents[i];
    ele.dispatchEvent(new Event('CLOSE'));
  }
}



function initImageViewer(o: { images: URL[], index: number}) {
  closeDropdowns();
  viewer = new Viewer(o.images, { index: o.index });
  viewer.show();
}


/* Open a image. If the viewer is already initialized, show the image directly.
 * Otherwise request the backend for the image list to construct a new viewer.
 * */
function openImage(path: string) {
  let query = new URLSearchParams({
    file: encodeURIComponent(path)
  });
  htmx.ajax('GET', `/img-viewer?${query.toString()}`, { target: 'body', swap: 'none'});
}
