const OFFLINE_URL = '/offline';
self.addEventListener('install', (event) => {
    event.waitUntil(caches.open('offline-cache').then(cache => {
        return cache.addAll([
            OFFLINE_URL,
        ]);
    }));
});
self.addEventListener('fetch', (event) => {
    // Only intercept navigation (HTML) requests
    if (event.request.mode === 'navigate') {
        event.respondWith((async () => {
            try {
                return await fetch(event.request);
            }
            catch {
                const cached = await caches.match(OFFLINE_URL);
                return cached ?? new Response('Offline fallback not available', { status: 503 });
            }
        })());
    }
});
export {};
