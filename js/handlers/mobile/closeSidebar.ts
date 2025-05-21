export function register() {
  document.addEventListener('click', e => {
    if ((e.target as Element).matches('#menu-btn *')) {
      return;
    }
    let sidebar = document.getElementById('side-bar')
    if (sidebar && sidebar.classList.contains('show')) {
      console.log('hi ', sidebar.classList)
      sidebar.classList.remove('show')
    }
  })
}
