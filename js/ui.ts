'use strict';

/* Desktop */
import * as CloseDropdownHandlers from './handlers/desktop/closeDropdown.js';
import * as ConextmenuHandlers from './handlers/desktop/contextmenu.js';


/* Mobile */
import * as ClosePanel from './handlers/mobile/closePanel.js';
import * as CloseSidebar from './handlers/mobile/closeSidebar.js';

import * as SelectedHandlers from './handlers/selected.js';
import * as ErrorsHandlers from './handlers/errors.js';
import * as ViewerHandlers from './handlers/viewer.js';
import * as Cookie from './cookie.js';
import { Display } from './def.js';

console.info("registering event listeners")

let display: Display = Cookie.getCookie('display')! as Display

/* Install handlers */
switch (display) {
  case 'Desktop':
    CloseDropdownHandlers.register();
    ConextmenuHandlers.register();
    break;
  case 'Mobile':
    ClosePanel.register();
    CloseSidebar.register();
    break;
  default:
    console.error('implementation error, no valid display type')
    break;
}

ErrorsHandlers.register();
ViewerHandlers.register();
SelectedHandlers.register();

/* Reinitialize the resolution again before refresh the page
 * This happens before the browser sending the request to reload the page,
 * so we can guarantee when the page is reloaded the resolution is up to date.
 * */
window.addEventListener("beforeunload", async (_) => {
  await fetch('/init',
    { method: 'POST',
      headers: { "Content-Type": "application/x-www-form-urlencoded", },
      body: new URLSearchParams({ res: window.innerWidth + 'x' + window.innerHeight })
    })
})
