'use strict';
declare var htmx: any;

import * as ControlPanel from './handlers/desktop/controlpanel.js';


document.addEventListener("DOMContentLoaded", () => {
  document.addEventListener('ThemeChanged', reloadTheme);
  ControlPanel.register();
});


function reloadTheme() {
  const oldLink = document.querySelector('link[rel="stylesheet"][href*="/theme.css"]') as HTMLLinkElement;
  if (!oldLink) return;

  const newLink = oldLink.cloneNode() as HTMLLinkElement;
  newLink.href = '/theme.css?v=' + Date.now(); // cache-busting
  newLink.onload = () => {
    oldLink.remove(); // remove old stylesheet after new one loads
  }
  oldLink.parentNode!.insertBefore(newLink, oldLink.nextSibling);
}
