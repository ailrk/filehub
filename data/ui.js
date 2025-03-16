window.onload = function () {
  'use strict';

  /* Imports */
  let Viewer = window.Viewer;

  /* States */
  let viewer = null;
  let imgDiv = null;

  /* Global event listener */
  window.addEventListener('click', closeDropdown);
  window.addEventListener('contextmenu', e => e.preventDefault());

  /* HX-Trigger events */
  window.addEventListener('ViewerJS', startImageViewer);

  window.addEventListener('FileExists', () => alert("Error: file exists"));
  window.addEventListener('InvalidPath', () => alert("Error: invalid path"));
  window.addEventListener('InvalidDir', () => alert("Error: invalid directory"));

  function closeDropdown(e) {
    // when clicking outside of any area outside a dropdown or dropdown-content, close all dropdowns.
    if (!e.target.matches('.dropdown *') && !e.target.matches('.dropdown-content *')) {
      closeDropdowns()
    }
  }

  /* Close all dropdowns */
  function closeDropdowns() {
    let dropdownContents = document.getElementsByClassName('dropdown-content')
    for (let i = 0; i < dropdownContents.length; ++i) {
      let ele = dropdownContents[i];
      if (ele.style['display'] == 'none') continue;
      ele.dispatchEvent(new Event('close'));
    }
  }

  function startImageViewer(e) {
    closeDropdowns();
    imgDiv = document.createElement('div');
    imgDiv.id = e.detail.elementId;
    imgDiv.innerHTML = e.detail.html.trim();

    function closeImageViewer () {
      console.log("close image viewer");
      if (viewer) {
        viewer.hide();
        viewer = null;
      }

      if (imgDiv) {
        imgDiv = null;
      }
    }

    imgDiv.addEventListener('hidden', closeImageViewer);

    viewer = new Viewer(imgDiv, {
      keyboard: true,
      toolbar: {
        zoomIn: 4,
        zoomOut: 4,
        oneToOne: 4,
        reset: 0,
        prev: 0,
        next: 0,
        rotateLeft: 0,
        rotateRight: 0,
        flipHorizontal: 0,
        flipVertical: 0,
      },
    });

    viewer.show();
  }
}
