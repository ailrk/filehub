const MAX_CONCURRENT = 4;
let inFlight = 0;
let generation = 0;
let active = false;
type MediaEl = HTMLImageElement | HTMLVideoElement;
const queue: MediaEl[] = [];
let controller = new AbortController();
let rafPending = false;

function tryLoadNext() {
  while (inFlight < MAX_CONCURRENT && queue.length > 0) {
    const el = queue.shift()!;
    if (!el.isConnected || !el.dataset.src) continue;
    const gen = generation;
    const src = el.dataset.src;
    inFlight++;

    fetch(src, { signal: controller.signal, priority: 'low' } as RequestInit)
      .then(res => res.blob())
      .then(blob => {
        const blobUrl = URL.createObjectURL(blob);
        el.src = blobUrl;
        const cleanup = () => URL.revokeObjectURL(blobUrl);
        const doneEvt = el instanceof HTMLVideoElement ? 'loadedmetadata' : 'load';
        el.addEventListener(doneEvt, cleanup, { once: true });
        el.addEventListener('error',   cleanup, { once: true });
      })
      .catch(_ => {})
      .finally(() => {
        if (gen !== generation) return;
        inFlight--;
        scheduleNext();
      });
  }
}

function scheduleNext() {
  if (rafPending) return;
  rafPending = true;
  requestAnimationFrame(() => { rafPending = false; tryLoadNext(); });
}

const observer = new IntersectionObserver(entries => {
  let pushed = false;
  for (const entry of entries) {
    if (!entry.isIntersecting || !active) continue;
    observer.unobserve(entry.target);
    queue.push(entry.target as MediaEl);
    pushed = true;
  }
  if (pushed) scheduleNext();
}, { rootMargin: '200px' });

export function init() {
  active = true;
  document.querySelectorAll<MediaEl>('img[data-src], video[data-src]').forEach(el => observer.observe(el));
}

export function reset() {
  active = false;
  generation++;
  queue.length = 0;
  inFlight = 0;
  rafPending = false;
  controller.abort();
  controller = new AbortController();
  observer.disconnect();
}
