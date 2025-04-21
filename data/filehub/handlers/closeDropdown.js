/* Close all dropdowns */
export function closeDropdowns() {
    let dropdownContents = document.getElementsByClassName('dropdown-content');
    for (let i = 0; i < dropdownContents.length; ++i) {
        let ele = dropdownContents[i];
        ele.dispatchEvent(new Event('Close'));
    }
}
// when clicking outside of any area outside a dropdown or dropdown-content, close all dropdowns.
function handler(e) {
    const target = e.target;
    if (!target.matches('.dropdown *') && !target.matches('.dropdown-content *')) {
        closeDropdowns();
    }
}
export function register() {
    document.addEventListener('click', handler);
}
