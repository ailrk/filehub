export function register() {
    document.addEventListener('click', handle);
}
function handle(e) {
    if (e.target.closest('#control-panel-btn')) {
        return;
    }
    let controlPanel = document.getElementById('control-panel');
    let overlay = document.getElementById('overlay');
    if (controlPanel && controlPanel.classList.contains('show')) {
        controlPanel.classList.remove('show');
        overlay.classList.remove('show');
    }
}
