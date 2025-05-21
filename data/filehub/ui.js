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
let display = Cookie.getCookie('display');
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
        console.error('implementation error, no valid display type');
        break;
}
ErrorsHandlers.register();
ViewerHandlers.register();
SelectedHandlers.register();
