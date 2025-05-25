/* Desktop */
import * as CloseDropdownHandlers from './desktop/closeDropdown.js';
import * as ConextmenuHandlers from './desktop/contextmenu.js';
import * as Selected from './desktop/selected.js';
export function register() {
    CloseDropdownHandlers.register();
    ConextmenuHandlers.register();
    Selected.register();
}
