/*
 * Shadow on the toolbar when the view is scrolled.
 * scroll events don't bubble, so we must attach directly to #view.
 * register() attaches once on startup; rebind() re-attaches after htmx
 * replaces #view — called from main.ts on htmx:afterSettle.
 *
 * Scroll position is saved before OOB swaps that replace #view (cancel,
 * delete) and restored in htmx:oobAfterSwap, which fires synchronously in
 * the same task as the DOM swap — before the browser can paint the
 * intermediate scrollTop=0 state. Navigation via cd does a main swap, not
 * OOB, so it intentionally resets scroll to the top.
 */
let ticking = false;
let scrollTimeout;
let savedScroll;
export function register() {
    document.addEventListener('htmx:oobBeforeSwap', (e) => {
        if (e.detail?.target?.id === 'view') {
            savedScroll = e.detail.target.scrollTop;
        }
    });
    document.addEventListener('htmx:oobAfterSwap', (e) => {
        if (savedScroll !== undefined && e.detail?.target?.id === 'view') {
            const view = document.getElementById('view');
            if (view)
                view.scrollTop = savedScroll;
            savedScroll = undefined;
        }
    });
    rebind();
}
export function rebind() {
    const view = document.getElementById('view');
    if (!view || view.dataset.scrollBound === 'true')
        return;
    view.addEventListener('scroll', onScroll);
    view.dataset.scrollBound = 'true';
}
function onScroll() {
    clearTimeout(scrollTimeout);
    scrollTimeout = window.setTimeout(() => {
        const toolbar = document.getElementById('tool-bar');
        if (toolbar)
            toolbar.style.boxShadow = '';
    }, 150);
    if (!ticking) {
        requestAnimationFrame(() => {
            const toolbar = document.getElementById('tool-bar');
            if (toolbar)
                toolbar.style.boxShadow = '0 3px 0 0 rgba(0,0,0,0.25)';
            ticking = false;
        });
        ticking = true;
    }
}
