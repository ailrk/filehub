const observer = new MutationObserver(register1);
export function register() {
    if (!document.querySelector('#locale'))
        return;
    register1();
    observer.observe(document.body, { childList: true, subtree: true });
}
function register1() {
    const locale = document.querySelector("#locale");
    if (!locale.dataset.bound) {
        locale.addEventListener("mouseenter", onEnter);
        locale.addEventListener("mouseleave", onLeave);
        locale.dataset.bound = "true"; // mark it bound
    }
}
function onEnter(_) {
    const dropdown = document.querySelector("#locale .dropdown-content");
    ;
    if (!dropdown)
        return;
    dropdown.style.display = "block";
}
function onLeave(_) {
    const dropdown = document.querySelector("#locale .dropdown-content");
    ;
    if (!dropdown)
        return;
    dropdown.style.display = "none";
}
