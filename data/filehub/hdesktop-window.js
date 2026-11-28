/*
 * Filehub floating window — a standalone draggable/resizable desktop surface.
 * Completely independent of the modal system. No backdrop, no blur, no
 * click-outside-to-close. Multiple windows stack via shared z-index counter.
 */
let _topZ = 800;
// Each window occupies a band of Z_BAND z-index slots:
//   +0  window body
//   +1  per-window overlays (ports, etc.)
const Z_BAND = 10;
function bringToFront(el) {
    _topZ += Z_BAND;
    el.style.zIndex = String(_topZ);
    el.dispatchEvent(new CustomEvent('filehub:windowmoved'));
}
export function createWindow(opts = {}) {
    const id = opts.id ?? crypto.randomUUID();
    const win = document.createElement('div');
    win.className = 'filehub-window';
    /* --- title bar --- */
    const titlebar = document.createElement('div');
    titlebar.className = 'filehub-window-titlebar';
    const titleEl = document.createElement('span');
    titleEl.className = 'filehub-window-title';
    titleEl.textContent = opts.title ?? '';
    const controls = document.createElement('div');
    controls.className = 'filehub-window-controls';
    /* --- minimize --- */
    const minBtn = document.createElement('button');
    minBtn.className = 'filehub-window-btn';
    minBtn.setAttribute('aria-label', 'Minimize');
    minBtn.innerHTML = `<i class="bx bx-minus"></i>`;
    minBtn.addEventListener('click', () => handle.minimize());
    let saved = null;
    const maxBtn = document.createElement('button');
    maxBtn.className = 'filehub-window-btn';
    maxBtn.setAttribute('aria-label', 'Maximize');
    maxBtn.innerHTML = `<i class="bx bx-expand"></i>`;
    maxBtn.addEventListener('click', () => {
        const isMax = win.classList.toggle('filehub-window-maximized');
        const icon = maxBtn.querySelector('i');
        if (isMax) {
            saved = { left: win.style.left, top: win.style.top,
                width: win.style.width, height: win.style.height };
            icon.className = 'bx bx-collapse';
        }
        else {
            if (saved) {
                win.style.left = saved.left;
                win.style.top = saved.top;
                win.style.width = saved.width;
                win.style.height = saved.height;
            }
            icon.className = 'bx bx-expand';
        }
    });
    /* --- close --- */
    const closeBtn = document.createElement('button');
    closeBtn.className = 'filehub-window-btn';
    closeBtn.setAttribute('aria-label', 'Close');
    closeBtn.innerHTML = `<i class="bx bx-x"></i>`;
    closeBtn.addEventListener('click', () => handle.close());
    controls.appendChild(minBtn);
    controls.appendChild(maxBtn);
    controls.appendChild(closeBtn);
    titlebar.appendChild(titleEl);
    titlebar.appendChild(controls);
    /* --- body --- */
    const body = document.createElement('div');
    body.className = 'filehub-window-body';
    win.appendChild(titlebar);
    win.appendChild(body);
    /* Declare handle early so event listeners can reference it safely.
     * All listeners fire asynchronously so handle is always assigned by then. */
    let handle;
    /* --- bring to front on any click --- */
    win.addEventListener('mousedown', () => {
        bringToFront(win);
        handle.onFocus?.();
    }, { capture: true });
    /* --- drag --- */
    titlebar.addEventListener('mousedown', (e) => {
        if (e.target.closest('button'))
            return;
        if (win.classList.contains('filehub-window-maximized'))
            return;
        e.preventDefault();
        const ox = e.clientX - win.offsetLeft;
        const oy = e.clientY - win.offsetTop;
        const onMove = (ev) => {
            win.style.left = (ev.clientX - ox) + 'px';
            win.style.top = (ev.clientY - oy) + 'px';
            win.dispatchEvent(new CustomEvent('filehub:windowmoved'));
        };
        const onUp = () => {
            document.removeEventListener('mousemove', onMove);
            document.removeEventListener('mouseup', onUp);
        };
        document.addEventListener('mousemove', onMove);
        document.addEventListener('mouseup', onUp);
    });
    handle = {
        id,
        root: win,
        body,
        show() {
            if (opts.width)
                win.style.width = opts.width + 'px';
            if (opts.height)
                win.style.height = opts.height + 'px';
            win.style.visibility = 'hidden';
            document.body.appendChild(win);
            if (opts.x != null && opts.y != null) {
                win.style.left = opts.x + 'px';
                win.style.top = opts.y + 'px';
            }
            else {
                const w = win.offsetWidth;
                const h = win.offsetHeight;
                win.style.left = Math.round((window.innerWidth - w) / 2) + 'px';
                win.style.top = Math.round((window.innerHeight - h) / 2) + 'px';
            }
            win.style.visibility = '';
            bringToFront(win);
            handle.onFocus?.();
            win.dispatchEvent(new CustomEvent('filehub:windowmoved'));
            // zoomIn animation uses transform:scale which skews getBoundingClientRect;
            // fire again after animation so ports get the correct final rect.
            win.addEventListener('animationend', () => {
                win.dispatchEvent(new CustomEvent('filehub:windowmoved'));
            }, { once: true });
        },
        close() {
            if (win.style.display === 'none') {
                win.remove();
                handle.onClose?.();
                return;
            }
            win.classList.add('filehub-window-closing');
            win.addEventListener('animationend', () => {
                win.remove();
                handle.onClose?.();
            }, { once: true });
        },
        minimize() {
            win.style.display = 'none';
            handle.onMinimize?.();
        },
        restore() {
            win.style.display = '';
            // Force reflow so getBoundingClientRect is accurate when ports reposition
            void win.offsetWidth;
            bringToFront(win);
            handle.onFocus?.();
            win.dispatchEvent(new CustomEvent('filehub:windowmoved'));
        },
        setWorkspaceHidden(hidden) {
            win.style.visibility = hidden ? 'hidden' : '';
            win.style.pointerEvents = hidden ? 'none' : '';
        },
    };
    return handle;
}
