window.addEventListener('click', closeDropdown);

function closeDropdown (e) {
  // when clicking outside of any area outside a dropdown or dropdown-content, close all dropdowns.
  if (!e.target.matches('.dropdown *') && !e.target.matches('.dropdown-content *')) {
    let dropdownContents = document.getElementsByClassName('dropdown-content')
    for (let i = 0; i < dropdownContents.length; ++i) {
      let ele = dropdownContents[i];
      if (ele.style['display'] == 'none') continue;
      ele.dispatchEvent(new Event('close'));
    }
  }
}
