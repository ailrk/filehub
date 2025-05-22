export function register() {
    document.body.addEventListener("htmx:afterOnLoad", (event) => {
        const path = event.detail.pathInfo.finalRequestPath;
        if (window.location.pathname + window.location.search !== path) {
            history.pushState({}, "", path);
        }
    });
}
window.addEventListener("popstate", (_) => {
    const path = window.location.pathname + window.location.search;
    htmx.ajax("GET", path, "#main");
});
