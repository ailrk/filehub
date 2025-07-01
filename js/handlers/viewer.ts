declare var htmx: any;
import type { ViewerInited } from '../def.js';
import { closeDropdowns } from './desktop/closeDropdown.js';
import Viewer from '../viewer.js';

/* States */
let viewer: Viewer | null = null;


export function register() {
  document.addEventListener('ViewerInited', (e: any) => initViewer(e.detail));
  document.addEventListener('Open', (e: any) => open(e.detail.path));
}


function initViewer(o: ViewerInited) {
  closeDropdowns();
  viewer = new Viewer(o.resources, { index: o.index });

  console.log(viewer.currentContent)
  viewer.show();

  // Make sure the viewer content has context menu enabled.
  viewer!.currentContent!.addEventListener(
    "contextmenu", event => {
      event.stopImmediatePropagation();
    }, { capture: true }
  )
}



/* Open a image. If the viewer is already initialized, show the image directly.
 * Otherwise request the backend for the image list to construct a new viewer.
 *
 * The path is already percent encoded, we don't need to encode it here.
 *
 * NOTE:
 * The target is set to 'head'. htmx ajax must specify a target, but this ajax
 * call don't need a target at all. We pick head to avoid interference on
 * other event handlers.
 * */
function open(path: string) {
  let query = new URLSearchParams({ file: path });
  htmx.ajax('GET', `/viewer?${query.toString()}`, { target: 'head', swap: 'none'});
}
