/*
 * Desktop drag-and-drop for moving files into folders,
 * and dragging files from the OS into #view to upload.
 * Registered once on document — no re-registration after htmx swaps.
 */
declare var htmx: any;

export function register() {
  document.addEventListener('dragstart',  onDragStart);
  document.addEventListener('drop',       onDrop);
  document.addEventListener('dragover',   onDragOver);
  document.addEventListener('dragenter',  onDragEnter);
  document.addEventListener('dragleave',  onDragLeave);
}


function hasExternalFiles(e: DragEvent): boolean {
  return Array.from(e.dataTransfer?.types ?? []).includes('Files');
}


function onDragOver(e: DragEvent) {
  if ((e.target as Element).closest('.dir')) { e.preventDefault(); return; }
  if (hasExternalFiles(e) && (e.target as Element).closest('#view')) {
    e.preventDefault();
    e.dataTransfer!.dropEffect = 'copy';
  }
}


let dragDepth = 0;

function showDropOverlay() {
  const view = document.getElementById('view');
  if (!view) return;
  view.classList.add('drop-target');
  let overlay = document.getElementById('drop-overlay');
  if (!overlay) {
    overlay = document.createElement('div');
    overlay.id = 'drop-overlay';
    document.body.appendChild(overlay);
  }
  const r = view.getBoundingClientRect();
  overlay.style.left   = r.left   + 'px';
  overlay.style.top    = r.top    + 'px';
  overlay.style.width  = r.width  + 'px';
  overlay.style.height = r.height + 'px';
}

function hideDropOverlay() {
  document.getElementById('view')?.classList.remove('drop-target');
  document.getElementById('drop-overlay')?.remove();
}

function onDragEnter(e: DragEvent) {
  if (!hasExternalFiles(e)) return;
  if (!(e.target as Element).closest('#view')) return;
  dragDepth++;
  showDropOverlay();
}


function onDragLeave(e: DragEvent) {
  if (!hasExternalFiles(e)) return;
  if (!(e.target as Element).closest('#view') && dragDepth > 0) {
    dragDepth = 0;
    hideDropOverlay();
    return;
  }
  dragDepth = Math.max(0, dragDepth - 1);
  if (dragDepth === 0) hideDropOverlay();
}


function onDragStart(e: DragEvent) {
  const item = (e.target as Element).closest<HTMLElement>('.table-item');
  if (!item) return;
  const path = item.dataset.path;
  if (!path) return;

  e.dataTransfer!.clearData();
  e.dataTransfer!.dropEffect = 'move';

  if (item.classList.contains('selected')) {
    const allSelected = Array.from(document.querySelectorAll<HTMLElement>('.selected'));
    const heads = allSelected.slice(0, 6)
      .map(el => el.querySelector<HTMLElement>('.image-wrapper'))
      .filter((el): el is HTMLElement => el !== null);

    drawIconAsync(heads).then(img => e.dataTransfer!.setDragImage(img, 180, 180));
    const paths = allSelected.map(el => el.dataset.path).filter(Boolean) as string[];
    e.dataTransfer!.setData('application/json', JSON.stringify(paths));
  } else {
    e.dataTransfer!.setData('application/json', JSON.stringify(path));
  }
}


function onDrop(e: DragEvent) {
  // OS → browser upload
  if (hasExternalFiles(e) && (e.target as Element).closest('#view')) {
    e.preventDefault();
    dragDepth = 0;
    hideDropOverlay();
    if (e.dataTransfer && e.dataTransfer.items.length > 0) uploadFiles(e.dataTransfer.items);
    return;
  }

  // Internal move into a folder
  const dir = (e.target as Element).closest<HTMLElement>('.dir');
  if (!dir) return;
  e.preventDefault();

  const tgt = dir.dataset.path;
  if (!tgt) return;

  let src: string | string[];
  try { src = JSON.parse(e.dataTransfer!.getData('application/json')); }
  catch { return; }

  if (typeof src === 'string') {
    if (src === tgt) return;
  }

  htmx.ajax('POST', '/files/move', {
    values: { src, tgt },
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    target: 'body',
    swap: 'none',
  });
}


function uploadFiles(items: DataTransferItemList): void {
  collectFiles(items).then(files => {
    if (files.length === 0) return;
    const form = new FormData();
    for (const f of files) form.append('file', f, f.name);
    fetch('/upload', { method: 'POST', body: form })
      .then(res => res.ok ? res.text() : Promise.reject(res.status))
      .then(html => {
        const doc      = new DOMParser().parseFromString(html, 'text/html');
        const newIndex = doc.getElementById('index');
        const oldIndex = document.getElementById('index');
        if (newIndex && oldIndex) {
          oldIndex.replaceWith(newIndex);
          htmx.process(newIndex);
        }
      })
      .catch(err => console.warn('[filehub] drag-upload failed', err));
  });
}


function collectFiles(items: DataTransferItemList): Promise<File[]> {
  const entries: FileSystemEntry[] = [];
  for (let i = 0; i < items.length; i++) {
    const entry = items[i].webkitGetAsEntry?.();
    if (entry) entries.push(entry);
  }
  return Promise.all(entries.map(readEntry)).then(nested => nested.flat());
}


function readEntry(entry: FileSystemEntry): Promise<File[]> {
  if (entry.isFile) {
    return new Promise(resolve => {
      (entry as FileSystemFileEntry).file(f => resolve([f]), () => resolve([]));
    });
  }
  if (entry.isDirectory) {
    return new Promise(resolve => {
      const reader = (entry as FileSystemDirectoryEntry).createReader();
      const all: FileSystemEntry[] = [];
      const read = () => reader.readEntries(batch => {
        if (batch.length === 0) {
          Promise.all(all.map(readEntry)).then(nested => resolve(nested.flat()));
        } else {
          all.push(...batch);
          read();
        }
      }, () => resolve([]));
      read();
    });
  }
  return Promise.resolve([]);
}


async function drawIconAsync(wrappers: HTMLElement[]): Promise<HTMLImageElement> {
  const canvas  = document.createElement('canvas');
  const theme   = getComputedStyle(document.documentElement);
  const ctx     = canvas.getContext('2d')!;
  const imgs: HTMLImageElement[] = [];
  const shift   = Math.min(wrappers.length * 10, 50);

  wrappers.forEach(wrapper => {
    const el = wrapper.children[0];
    if (el?.tagName === 'IMG') {
      const img = el as HTMLImageElement;
      img.removeAttribute('loading');
      imgs.push(img);
    }
  });

  canvas.width = canvas.height = 500;

  await Promise.all(imgs.map(img =>
    img.complete ? Promise.resolve()
    : new Promise<void>(res => img.addEventListener('load', () => res(), { once: true }))
  ));
  await document.fonts.ready;

  fillRoundedRect(ctx, 0, 0, 500, 500, 16, theme.getPropertyValue('--background3'));

  wrappers
    .sort((a, b) => {
      const ta = a.children[0]?.tagName ?? '';
      const tb = b.children[0]?.tagName ?? '';
      if (ta === 'IMG' && tb === 'I') return -1;
      if (ta === 'I'   && tb === 'IMG') return 1;
      return 0;
    })
    .forEach((wrapper, i) => {
      ctx.save();
      ctx.translate(250 + 8 * i, 250);
      ctx.rotate((4 * i * Math.PI) / 180);

      const el = wrapper.children[0];
      if (el?.tagName === 'IMG') {
        const img   = el as HTMLImageElement;
        const scale = Math.min(300 / img.naturalWidth, 300 / img.naturalHeight);
        const w = img.naturalWidth * scale, h = img.naturalHeight * scale;
        ctx.drawImage(img, (-w / 2) - shift, -h / 2, w, h);
      } else if (el?.tagName === 'I') {
        const style    = getComputedStyle(el as HTMLElement, '::before');
        const fontSize = 2 * parseFloat(style.fontSize);
        ctx.font          = `${style.fontWeight} ${fontSize}px ${style.fontFamily}`;
        ctx.fillStyle     = style.color;
        ctx.textBaseline  = 'middle';
        ctx.textAlign     = 'center';
        ctx.fillText(style.content.replace(/^"(.*)"$/, '$1') || '□', 0, 0);
      }
      ctx.restore();
    });

  const img = new Image();
  img.src = canvas.toDataURL();
  img.style.borderRadius = '6px';
  return img;
}


function fillRoundedRect(
  ctx: CanvasRenderingContext2D,
  x: number, y: number, w: number, h: number, r: number, fill: string
) {
  ctx.beginPath();
  ctx.moveTo(x + r, y);
  ctx.lineTo(x + w - r, y);    ctx.arcTo(x + w, y,     x + w, y + r,     r);
  ctx.lineTo(x + w, y + h - r); ctx.arcTo(x + w, y + h, x + w - r, y + h, r);
  ctx.lineTo(x + r, y + h);     ctx.arcTo(x,     y + h, x,     y + h - r, r);
  ctx.lineTo(x, y + r);         ctx.arcTo(x,     y,     x + r, y,         r);
  ctx.closePath();
  ctx.fillStyle = fill;
  ctx.fill();
}
