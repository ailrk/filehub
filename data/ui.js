window.addEventListener('click', closeDropdown);
window.addEventListener('contextmenu', e => e.preventDefault());

/* Error handling */
document.addEventListener('FileExists', () => alert("Error: file exists"));
document.addEventListener('InvalidPath', () => alert("Error: invalid path"));
document.addEventListener('InvalidDir', () => alert("Error: invalid directory"));

function closeDropdown (e) {
  // when clicking outside of any area outside a dropdown or dropdown-content, close all dropdowns.
  if (!e.target.matches('.dropdown *') && !e.target.matches('.dropdown-content *')) {
    closeDropdowns()
  }
}

function closeDropdowns () {
  let dropdownContents = document.getElementsByClassName('dropdown-content')
  for (let i = 0; i < dropdownContents.length; ++i) {
    let ele = dropdownContents[i];
    if (ele.style['display'] == 'none') continue;
    ele.dispatchEvent(new Event('close'));
  }
}
