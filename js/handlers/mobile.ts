import * as ClosePanel from './mobile/closePanel.js';
import * as CloseSidebar from './mobile/closeSidebar.js';
import * as Selected from './mobile/selected.js';

export function register() {
  ClosePanel.register();
  CloseSidebar.register();
  Selected.register();
  const table = document.querySelector('#table'); // change selector to fit your layout
  table!.addEventListener('contextmenu', e => e.preventDefault());
}
