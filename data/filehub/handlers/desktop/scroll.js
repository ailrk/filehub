let ticking = false; // prevent throttling.
let scrollTimeout;
const observer = new MutationObserver(register1);
export function register() {
    register1();
    observer.observe(document.body, { childList: true, subtree: true });
}
function register1() {
    const view = document.querySelector("#view");
    if (view && !view.dataset.scrollBound) {
        view.addEventListener("scroll", onScroll);
        view.dataset.scrollBound = "true"; // mark it bound
    }
}
function onScroll() {
    function onScrollEnds() {
        let toolBar = document.querySelector("#tool-bar");
        if (toolBar) {
            toolBar.style.boxShadow = "";
        }
    }
    clearTimeout(scrollTimeout);
    scrollTimeout = setTimeout(onScrollEnds);
    if (!ticking) {
        window.requestAnimationFrame(() => {
            let toolBar = document.querySelector("#tool-bar");
            if (toolBar) {
                toolBar.style.boxShadow = "0 3px 0 0 rgba(0, 0, 0, 0.3)";
            }
            ticking = false;
        });
        ticking = true;
    }
}
