let ticking = false; // prevent throttling.
let scrollTimeout: number;


const observer = new MutationObserver(register1);

export function register() {
  if (!document.querySelector("#view")) return;

  register1();
  observer.observe(document.body, { childList: true, subtree: true });
}


function register1() {
  const view = document.querySelector("#view") as HTMLElement | null;
  if (view && !view.dataset.scrollBound) {
    view.addEventListener("scroll", onScroll);
    view.dataset.scrollBound = "true"; // mark it bound
  }
}

function onScroll() {
  function onScrollEnds() {
    let toolBar = document.querySelector("#tool-bar") as HTMLElement | null;
    if (toolBar) {
      toolBar.style.boxShadow = "";
    }
  }
  clearTimeout(scrollTimeout);
  scrollTimeout = setTimeout(onScrollEnds, 150);
  if (!ticking) {
    window.requestAnimationFrame(() => {
      let toolBar = document.querySelector("#tool-bar") as HTMLElement | null;
      if (toolBar) {
        toolBar.style.boxShadow = "0 3px 0 0 rgba(0, 0, 0, 0.3)";
      }
      ticking = false;
    });
    ticking = true;
  }
}
