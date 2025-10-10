declare var htmx: any;


export function register() {
  if (!document.querySelector('#table')) return;

  register1()
  document.body.addEventListener('htmx:afterSettle', _ => {
    register1()
  })
}


function register1() {
  const items = document.querySelectorAll('.table-item')
  const dirs = document.querySelectorAll('.dir')
  for (let i = 0; i < items.length; ++i) {
    const item = items[i];
    item.removeEventListener('dragstart', handleDragStart);
    item.addEventListener('dragstart', handleDragStart);
  }

  for (let i = 0; i < dirs.length; ++i) {
    const dir = dirs[i];
    dir.removeEventListener('drop', handleDrop)
    dir.removeEventListener('dragover', handleDragOver)
    dir.addEventListener('drop', handleDrop)
    dir.addEventListener('dragover', handleDragOver)
  }
}


function handleDragOver(e: Event) {
  e.preventDefault()
}


function handleDragStart(e: Event) {
  if (!(e instanceof DragEvent)) return;
  if (e.target instanceof HTMLElement) {
    const path = e.target.dataset.path;
    if (path === undefined) return;
    e.dataTransfer!.clearData()
    e.dataTransfer!.dropEffect = 'move'

    if (e.target.classList.contains('selected')) {
      let allSelected: NodeListOf<HTMLElement> = document.querySelectorAll('.selected')
      const heads =
        allSelected
        .values()
        .take(6)
        .map(elt => elt.querySelector('.image-wrapper') as HTMLElement)
        .toArray()

      drawIconAsync(heads).
        then(img => {
          e.dataTransfer!.setDragImage(img, 180, 180);
        });
      let paths = allSelected.values().map(elt => elt.dataset.path).toArray();
      e.dataTransfer!.setData('application/json', JSON.stringify(paths))
    } else {
      e.dataTransfer!.setData('application/json', JSON.stringify(path))
    }
  }
}


async function drawIconAsync(wrappers: HTMLElement[]): Promise<HTMLImageElement> {
  const canvas = document.createElement('canvas');
  const theme = getComputedStyle(document.querySelector(':root')!);
  const dx = 8;
  const dr = 4 * Math.PI / 180;
  const ctx = canvas.getContext('2d')!;
  const imgs: HTMLImageElement[] = [];
  const shift = Math.min (wrappers.length * 10, 50);
  wrappers.forEach(wrapper => {
    const ele = wrapper.children[0];
    if (ele.tagName === 'IMG') {
      const img = ele as HTMLImageElement;
      img.removeAttribute('loading');
      imgs.push(img);
    }
  });

  canvas.width = 500;
  canvas.height = 500;

  // Wait for all images to load
  await Promise.all(imgs.map(img =>
    img.complete
    ? Promise.resolve()
    : new Promise<void>(res => img.addEventListener('load', () => res(), { once: true }))
  ));
  await document.fonts.ready;

  fillRoundedRect(ctx, 0, 0, canvas.width, canvas.height, 16, theme.getPropertyValue('--background3'))
  wrappers
  .sort((w1, w2)=> {
    let t1 = w1.children[0].tagName
    let t2 = w2.children[0].tagName
    if (t1 == 'IMG' && t2 == 'I') { return -1; }
    if (t1 == 'I' && t2 == 'IMG') { return 1; }
    return 0;
  })
  .forEach((wrapper, i) => {
    ctx.save();
    ctx.translate(canvas.width / 2 + dx * i, canvas.height / 2);
    ctx.rotate(dr * i);

    const ele = wrapper.children[0];
    if (ele.tagName === 'IMG') {
      const img = ele as HTMLImageElement;
      const scale = Math.min(300 / img.naturalWidth, 300 / img.naturalHeight);
      const w = img.naturalWidth * scale;
      const h = img.naturalHeight * scale;
      ctx.drawImage(img, (-w/2) - shift, -h/2, w, h);

    } else if (ele.tagName === 'I') {
      const icon = ele as HTMLElement;
      const style = getComputedStyle(icon, '::before');
      const fontSize = 2 * parseFloat(style.fontSize);
      ctx.font = `${style.fontWeight} ${fontSize}px ${style.fontFamily}`;
      ctx.fillStyle = style.color;
      ctx.textBaseline = 'middle';
      ctx.textAlign = 'center';
      ctx.fillText(style.content.replace(/^"(.*)"$/, '$1') || 'X', 0, 0);
    }

    ctx.restore();
  });

  const img = new Image();
  img.src = canvas.toDataURL();
  img.style.borderRadius = '6px';
  return img;
}


function fillRoundedRect(ctx: CanvasRenderingContext2D, x: number, y: number, width: number, height: number, radius: number, fillStyle: string) {
  ctx.beginPath();
  ctx.moveTo(x + radius, y);
  ctx.lineTo(x + width - radius, y);
  ctx.arcTo(x + width, y, x + width, y + radius, radius);
  ctx.lineTo(x + width, y + height - radius);
  ctx.arcTo(x + width, y + height, x + width - radius, y + height, radius);
  ctx.lineTo(x + radius, y + height);
  ctx.arcTo(x, y + height, x, y + height - radius, radius);
  ctx.lineTo(x, y + radius);
  ctx.arcTo(x, y, x + radius, y, radius);
  ctx.closePath();
  ctx.fillStyle = fillStyle;
  ctx.fill();
}


function handleDrop(e: Event) {
  if (!(e instanceof DragEvent)) return;
  e.preventDefault();
  if (e.target instanceof HTMLElement) {
    const tgt = e.target.dataset.path;
    if (tgt === undefined) return;
    const json = e.dataTransfer!.getData('application/json')
    let src;
    try {
      src =  JSON.parse(json)
    } catch {
      console.error('invalid drag data')
      return;
    }

    if (typeof src === 'string') {
      if (src === tgt) return; // ignore recursive drop
      const values: Record<string, string> = { src, tgt }
      htmx.ajax('POST',
        '/files/move',
        {
          values,
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded'
          },
          target: '#index',
          swap: 'outerHTML'
        }
      )
    }

    if (Array.isArray(src)) {
      const values: { src: string[], tgt: string } = { src, tgt }
      htmx.ajax('POST',
        '/files/move',
        {
          values,
          headers: {
            'Content-Type': 'application/x-www-form-urlencoded'
          },
          target: '#index',
          swap: 'outerHTML'
        }
      )
    }
  }
}
