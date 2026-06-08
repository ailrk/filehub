const observer = new MutationObserver(register1);


export function register() {
  if (!document.querySelectorAll('.control-panel-dropdown-btn')) return;

  register1();
  observer.observe(document.body, { childList: true, subtree: true });
}


function register1() {
  const btns = document.querySelectorAll(".control-panel-dropdown-btn") as NodeListOf<HTMLElement>;
  for (let btn of btns) {
    if (!btn.dataset.bound) {
      btn.addEventListener("mouseenter", onEnter);
      btn.addEventListener("mouseleave", onLeave);
      btn.dataset.bound = "true"; // mark it bound
    }
  }
}


function onEnter(e: Event) {
  let target = e.target as HTMLElement;
  let dropdown = target.querySelector('.dropdown-content') as HTMLElement | null;
  if (dropdown) {
    dropdown.style.display = "block";
  }
}


function onLeave(e: Event) {
  let target = e.target as HTMLElement;
  let dropdown = target.querySelector('.dropdown-content') as HTMLElement | null;
  if (dropdown) {
    dropdown.style.display = "none";
  }
}
