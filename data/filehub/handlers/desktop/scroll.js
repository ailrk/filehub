/*
 * Shadow on the toolbar when the view is scrolled.
 * scroll events don't bubble, so we must attach directly to #view.
 * register() attaches once on startup; rebind() re-attaches after htmx
 * replaces #view — called from main.ts on htmx:afterSettle.
 */
let ticking = false;
let scrollTimeout;
export function register() {
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
