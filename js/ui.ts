'use strict';

import * as MultiSelectHandlers from './handlers/multiselect.js';
import * as ErrorsHandlers from './handlers/errors.js';
import * as CloseDropdownHandlers from './handlers/closeDropdown.js';
import * as ConextmenuHandlers from './handlers/contextmenu.js';
import * as ViewerHandlers from './handlers/viewer.js';


/* Install handlers */
CloseDropdownHandlers.register();
ErrorsHandlers.register();
ViewerHandlers.register();
ConextmenuHandlers.register();
MultiSelectHandlers.register();


/* Attach handlers on htmx swap */
document.body.addEventListener('htmx:afterSwap', _ => {
  ConextmenuHandlers.register();
  MultiSelectHandlers.register();
});
