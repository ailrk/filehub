/* Close all dropdowns */
export function closeDropdowns() {
  let dropdownContents = document.getElementsByClassName('dropdown-content')
  for (let i = 0; i < dropdownContents.length; ++i) {
    let ele = dropdownContents[i];
    ele.dispatchEvent(new Event('Close'));
  }
}


/* Cleanup the dropdown effects to make sure the app goes back to the normal state. */
function cleanup() {
  const table = document.getElementById('table') as HTMLElement
  table.style.pointerEvents = "auto";
}


/* when clicking outside of any area outside a dropdown or dropdown-content, close all dropdowns. */
function handler(e: any) {
  const target = e.target as Element;
  if (!target.matches('.dropdown *') && !target.matches('.dropdown-content *')) {
    closeDropdowns()
    cleanup()
  }
}


export function register()  {
  document.addEventListener('click', handler);
}
