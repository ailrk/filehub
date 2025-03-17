window.onload = function () {
  'use strict';

  /* Imports */
  let Viewer = window.Viewer;

  /* States */
  let viewer = null;
  let imgDiv = null;

  /* Event handlers  */
  let eventHandlers = {
    closeDropdown: function (e) {
      // when clicking outside of any area outside a dropdown or dropdown-content, close all dropdowns.
      if (!e.target.matches('.dropdown *') && !e.target.matches('.dropdown-content *')) {
        closeDropdowns()
      }
    },
    updateImageViewer: e => updateImageViewer(e.detail.html.trim()),
    startImageViewer: e => startImageViewer(e.detail.html.trim())
  }

  /* Global event listener */
  window.addEventListener('click', eventHandlers.closeDropdown);
  window.addEventListener('contextmenu', e => e.preventDefault());

  window.addEventListener('FileExists', () => alert("Error: file exists"));
  window.addEventListener('InvalidPath', () => alert("Error: invalid path"));
  window.addEventListener('InvalidDir', () => alert("Error: invalid directory"));

  /* Image viewer events */
  window.addEventListener('UpdateImageList', eventHandlers.updateImageViewer);
  window.addEventListener('RunImageViewer', eventHandlers.startImageViewer);


  /* Close all dropdowns */
  function closeDropdowns() {
    let dropdownContents = document.getElementsByClassName('dropdown-content')
    for (let i = 0; i < dropdownContents.length; ++i) {
      let ele = dropdownContents[i];
      if (ele.style['display'] == 'none') continue;
      ele.dispatchEvent(new Event('CLOSE'));
    }
  }


  function updateImageViewer(html) {
    if (imgDiv === null) {
      imgDiv = document.createElement('div');
    }
    imgDiv.innerHTML = html;
    if (viewer !== null) {
      viewer.update()
    }
  }


  function startImageViewer(html) {
    updateImageViewer(html)

    closeDropdowns();

    viewer = new Viewer(imgDiv, {
      keyboard: true,
      initialViewIndex: (() => {
        let ul = imgDiv.querySelector("ul");
        return Math.floor(ul.children.length / 2);
      }) (),
      toolbar: {
        zoomIn: 4,
        zoomOut: 4,
        oneToOne: 4,
        reset: 0,
        prev: 1,
        next: 1,
        rotateLeft: 0,
        rotateRight: 0,
        flipHorizontal: 0,
        flipVertical: 0,
      },
    });

    viewer.show();

    imgDiv.addEventListener('hidden', () => {
      viewer.destroy()
      viewer = null;
    });

    imgDiv.addEventListener('viewed', () => {
      let ul = imgDiv.querySelector("ul");
      let img = ul.children[viewer.index];
      let isLast = viewer.index >= ul.children.length - 1;
      let isFirst = viewer.index == 0;

      if (img) {
        if (isFirst || isLast) {
          let path = encodeURIComponent(img.getAttribute('src'));
          htmx.ajax('GET', `/img-viewer/update?file=${path}`, { target: 'body', swap: 'none' })
            .then(_ => {
              viewer.update();
              viewer.view(Math.floor(ul.children.length / 2));
            });
        }
      }
    });
  }
}
