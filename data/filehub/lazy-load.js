const MAX_CONCURRENT = 2;
let inFlight = 0;
let generation = 0;
const queue = [];
let controller = new AbortController();
let rafPending = false;
function tryLoadNext() {
    while (inFlight < MAX_CONCURRENT && queue.length > 0) {
        const img = queue.shift();
        if (!img.isConnected || !img.dataset.src)
            continue;
        const gen = generation;
        const src = img.dataset.src;
        inFlight++;
        fetch(src, { signal: controller.signal })
            .then(res => res.blob())
            .then(blob => {
            if (gen !== generation)
                return;
            const blobUrl = URL.createObjectURL(blob);
            img.src = blobUrl;
            img.addEventListener('load', () => URL.revokeObjectURL(blobUrl), { once: true });
            img.addEventListener('error', () => URL.revokeObjectURL(blobUrl), { once: true });
        })
            .catch(() => { })
            .finally(() => {
            if (gen !== generation)
                return;
            inFlight--;
            scheduleNext();
        });
    }
}
function scheduleNext() {
    if (rafPending)
        return;
    rafPending = true;
    requestAnimationFrame(() => { rafPending = false; tryLoadNext(); });
}
const observer = new IntersectionObserver(entries => {
    for (const entry of entries) {
        if (!entry.isIntersecting)
            continue;
        observer.unobserve(entry.target);
        queue.push(entry.target);
    }
    scheduleNext();
}, { rootMargin: '200px' });
export function init() {
    document.querySelectorAll('img[data-src]').forEach(img => observer.observe(img));
}
export function reset() {
    generation++;
    queue.length = 0;
    inFlight = 0;
    rafPending = false;
    controller.abort();
    controller = new AbortController();
}
