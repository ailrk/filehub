/*
 * Close the sidebar when clicking outside it.
 * panel.ts handles toggling it open.
 */

export function register() {
  document.addEventListener('click', handle);
}


function handle(e: MouseEvent) {
  if ((e.target as Element).closest('#sidebar-btn')) return;
  const sidebar = document.getElementById('side-bar');
  const overlay = document.getElementById('overlay');
  if (sidebar?.classList.contains('show')) {
    sidebar.classList.remove('show');
    overlay?.classList.remove('show');
  }
}
