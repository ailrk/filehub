export function register() {
  document.addEventListener('click', handle)
}


function handle(e: MouseEvent) {
  if ((e.target as Element).closest('#menu-btn')) {
    return;
  }
  let sidebar = document.getElementById('side-bar')
  let overlay = document.getElementById('overlay')
  if (sidebar && sidebar.classList.contains('show')) {
    sidebar.classList.remove('show')
    overlay!.classList.remove('show')
  }
}
