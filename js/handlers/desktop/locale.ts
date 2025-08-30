const observer = new MutationObserver(register1);


export function register() {
  register1();
  observer.observe(document.body, { childList: true, subtree: true });
}


function register1() {
  const locale = document.querySelector("#locale") as HTMLElement | null;

  if (locale && !locale.dataset.bound) {
    locale.addEventListener("mouseenter", onEnter);
    locale.addEventListener("mouseleave", onLeave);
    locale.dataset.bound = "true"; // mark it bound
  }
}


function onEnter(_: any) {
  const dropdown = document.querySelector("#locale .dropdown-content") as HTMLElement | null;;
  if (!dropdown) return;
  dropdown.style.display = "block";
}


function onLeave(_: any) {
  const dropdown = document.querySelector("#locale .dropdown-content") as HTMLElement | null;;
  if (!dropdown) return;
  dropdown.style.display = "none";
}
