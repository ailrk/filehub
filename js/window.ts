/*
 * Filehub floating window — a standalone draggable/resizable desktop surface.
 * Completely independent of the modal system. No backdrop, no blur, no
 * click-outside-to-close. Multiple windows stack via shared z-index counter.
 */

let _topZ = 800;

function bringToFront(el: HTMLElement) {
  _topZ++;
  el.style.zIndex = String(_topZ);
}

export interface WindowHandle {
  readonly body: HTMLElement;
  show(): void;
  close(): void;
}

export interface WindowOptions {
  title?:  string;
  width?:  number;
  height?: number;
}

export function createWindow(opts: WindowOptions = {}): WindowHandle {
  const win = document.createElement('div');
  win.className = 'filehub-window';

  /* ── title bar ─────────────────────────────────────────────────────────── */
  const titlebar = document.createElement('div');
  titlebar.className = 'filehub-window-titlebar';

  const titleEl = document.createElement('span');
  titleEl.className = 'filehub-window-title';
  titleEl.textContent = opts.title ?? '';

  const controls = document.createElement('div');
  controls.className = 'filehub-window-controls';

  /* ── maximize / restore ─────────────────────────────────────────────────── */
  type SavedGeom = { left: string; top: string; width: string; height: string; transform: string };
  let saved: SavedGeom | null = null;

  const maxBtn = document.createElement('button');
  maxBtn.className = 'filehub-window-btn';
  maxBtn.setAttribute('aria-label', 'Maximize');
  maxBtn.innerHTML = `<i class="bx bx-expand"></i>`;
  maxBtn.addEventListener('click', () => {
    const isMax = win.classList.toggle('filehub-window-maximized');
    const icon  = maxBtn.querySelector('i')!;
    if (isMax) {
      saved = { left: win.style.left, top: win.style.top,
                width: win.style.width, height: win.style.height,
                transform: win.style.transform };
      icon.className = 'bx bx-collapse';
    } else {
      if (saved) {
        win.style.left      = saved.left;
        win.style.top       = saved.top;
        win.style.width     = saved.width;
        win.style.height    = saved.height;
        win.style.transform = saved.transform;
      }
      icon.className = 'bx bx-expand';
    }
  });

  /* ── close ──────────────────────────────────────────────────────────────── */
  const closeBtn = document.createElement('button');
  closeBtn.className = 'filehub-window-btn';
  closeBtn.setAttribute('aria-label', 'Close');
  closeBtn.innerHTML = `<i class="bx bx-x"></i>`;
  closeBtn.addEventListener('click', () => handle.close());

  controls.appendChild(maxBtn);
  controls.appendChild(closeBtn);
  titlebar.appendChild(titleEl);
  titlebar.appendChild(controls);

  /* ── body ───────────────────────────────────────────────────────────────── */
  const body = document.createElement('div');
  body.className = 'filehub-window-body';

  win.appendChild(titlebar);
  win.appendChild(body);

  /* ── bring to front on any click ────────────────────────────────────────── */
  win.addEventListener('mousedown', () => bringToFront(win), { capture: true });

  /* ── drag ───────────────────────────────────────────────────────────────── */
  titlebar.addEventListener('mousedown', (e: MouseEvent) => {
    if ((e.target as Element).closest('button')) return;
    if (win.classList.contains('filehub-window-maximized')) return;
    e.preventDefault();

    // Switch from initial transform-centering to explicit coords on first drag
    if (win.style.transform && win.style.transform !== 'none') {
      const r        = win.getBoundingClientRect();
      win.style.left      = r.left + 'px';
      win.style.top       = r.top  + 'px';
      win.style.transform = 'none';
    }

    const ox = e.clientX - win.offsetLeft;
    const oy = e.clientY - win.offsetTop;

    const onMove = (ev: MouseEvent) => {
      win.style.left = (ev.clientX - ox) + 'px';
      win.style.top  = (ev.clientY - oy) + 'px';
    };
    const onUp = () => {
      document.removeEventListener('mousemove', onMove);
      document.removeEventListener('mouseup',   onUp);
    };
    document.addEventListener('mousemove', onMove);
    document.addEventListener('mouseup',   onUp);
  });

  /* ── public handle ──────────────────────────────────────────────────────── */
  const handle: WindowHandle = {
    body,

    show() {
      if (opts.width)  win.style.width  = opts.width  + 'px';
      if (opts.height) win.style.height = opts.height + 'px';
      win.style.left      = '50%';
      win.style.top       = '50%';
      win.style.transform = 'translate(-50%, -50%)';
      bringToFront(win);
      document.body.appendChild(win);
    },

    close() {
      win.classList.add('filehub-window-closing');
      win.addEventListener('animationend', () => win.remove(), { once: true });
    },
  };

  return handle;
}
