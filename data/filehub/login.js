'use strict';
import * as ControlPanel from './handlers/desktop/controlpanel.js';
document.addEventListener("DOMContentLoaded", () => {
    document.addEventListener('ThemeChanged', reloadTheme);
    ControlPanel.register();
});
function reloadTheme() {
    const oldLink = document.querySelector('link[rel="stylesheet"][href*="/theme.css"]');
    if (!oldLink)
        return;
    const newLink = oldLink.cloneNode();
    newLink.href = '/theme.css?v=' + Date.now(); // cache-busting
    newLink.onload = () => {
        oldLink.remove(); // remove old stylesheet after new one loads
    };
    oldLink.parentNode.insertBefore(newLink, oldLink.nextSibling);
}
