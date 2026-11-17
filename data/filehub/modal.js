/*
 * Modal lifecycle — replaces _hyperscript close-modal incantations.
 *
 * Rules:
 *   • Click .modal-underlay → close the containing .modal
 *   • Click [data-close-modal] → close the containing .modal
 *   • Dispatch a 'Close' event on any element inside .modal → close it
 *
 * Registered once on document at startup; survives all htmx swaps.
 */
export function register() {
    document.addEventListener('click', onDocumentClick, { capture: true });
    document.addEventListener('Close', onClose, { capture: true });
}
function onDocumentClick(e) {
    const target = e.target;
    if (target.closest('[data-close-modal]') || target.closest('.modal-underlay')) {
        const modal = target.closest('.modal');
        if (modal) {
            e.stopPropagation();
            close(modal);
        }
    }
}
function onClose(e) {
    const target = e.target;
    const modal = target.matches('.modal')
        ? target
        : target.closest('.modal');
    if (modal)
        close(modal);
}
export function close(modal) {
    if (modal.classList.contains('closing'))
        return;
    modal.classList.add('closing');
    modal.addEventListener('animationend', () => modal.remove(), { once: true });
}
