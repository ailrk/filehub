import { closeDropdowns } from './desktop/closeDropdown.js';
import Viewer from '../viewer.js';
/* States */
let viewer = null;
export function register() {
    document.addEventListener('ViewerInited', (e) => initViewer(e.detail));
    document.addEventListener('Open', (e) => open(e.detail.path));
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
    let query = new URLSearchParams({ file: encodeURIComponent(path) });
    htmx.ajax('GET', `/viewer?${query.toString()}`, { target: 'body', swap: 'none' });
}
