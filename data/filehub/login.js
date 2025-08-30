'use strict';
import * as Locale from './handlers/desktop/locale.js';
document.addEventListener("DOMContentLoaded", () => {
    document.addEventListener('ThemeChanged', reloadTheme);
    Locale.register();
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
