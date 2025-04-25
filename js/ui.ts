'use strict';

import * as SelectedHandlers from './handlers/selected.js';
import * as ErrorsHandlers from './handlers/errors.js';
import * as CloseDropdownHandlers from './handlers/closeDropdown.js';
import * as ConextmenuHandlers from './handlers/contextmenu.js';
import * as ViewerHandlers from './handlers/viewer.js';


/* Install handlers */
CloseDropdownHandlers.register();
ErrorsHandlers.register();
ViewerHandlers.register();
ConextmenuHandlers.register();
SelectedHandlers.register();
