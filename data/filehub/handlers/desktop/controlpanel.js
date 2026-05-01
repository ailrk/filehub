const observer = new MutationObserver(register1);
export function register() {
    if (!document.querySelectorAll('.control-panel-dropdown-btn'))
        return;
    register1();
    observer.observe(document.body, { childList: true, subtree: true });
}
function register1() {
    const btns = document.querySelectorAll(".control-panel-dropdown-btn");
    for (let btn of btns) {
        if (!btn.dataset.bound) {
            btn.addEventListener("mouseenter", onEnter);
            btn.addEventListener("mouseleave", onLeave);
            btn.dataset.bound = "true"; // mark it bound
        }
    }
}
function onEnter(e) {
    let target = e.target;
    let dropdown = target.querySelector('.dropdown-content');
    console.log(target, dropdown);
    if (dropdown) {
        dropdown.style.display = "block";
    }
}
function onLeave(e) {
    let target = e.target;
    let dropdown = target.querySelector('.dropdown-content');
    if (dropdown) {
        dropdown.style.display = "none";
    }
}
