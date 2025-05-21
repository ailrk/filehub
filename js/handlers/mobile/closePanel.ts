export function register() {
  document.addEventListener('click', e => {
    if ((e.target as Element).matches('#control-panel-btn *')) {
      return;
    }
    let controlPanel = document.getElementById('control-panel')
    if (controlPanel && controlPanel.classList.contains('show')) {
      controlPanel.classList.remove('show')
    }
  })
}
