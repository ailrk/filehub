declare var htmx: any;


let dragging: 'single' | 'multiple' | null = null


export function register() {
  document.body.addEventListener('htmx:afterSettle', _ => {
    register1()
  })
}


function register1() {
  const items = document.querySelectorAll('.table-item')
  const dirs = document.querySelectorAll('.dir')
  for (let i = 0; i < items.length; ++i) {
    const item = items[i];
    item.addEventListener('dragstart', handleDragStart, { once: true });
  }

  for (let i = 0; i < dirs.length; ++i) {
    const dir = dirs[i];
    dir.addEventListener('drop', handleDrop, { once: true })
    dir.addEventListener('dragover', handleDragOver, { once: true })
  }
}


function handleDragOver(e: Event) {
  e.preventDefault()
}


function handleDragStart(e: Event) {
  if (!(e instanceof DragEvent)) return;
  console.log('dragging', e.target)
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
        .take(10)
        .map(elt => elt.querySelector('.image-wrapper') as HTMLElement)
        .toArray()

      drawIconAsync(heads).
        then(img => {
          e.dataTransfer!.setDragImage(img, 180, 180);
        });

      e.dataTransfer!.setData('application/json', JSON.stringify(allSelected.values().map(elt => elt.dataset.path)))
    } else {
      e.dataTransfer!.setData('application/json', path)
    }
  }
}


async function drawIconAsync(wrappers: HTMLElement[]): Promise<HTMLImageElement> {
  const canvas = document.createElement('canvas');
  // document.body.prepend(canvas)

  canvas.width = 240;
  canvas.height = 240;
  const ctx = canvas.getContext('2d')!;

  const dx = 8;
  const dr = 4 * Math.PI / 180;

  // Gather all <img> elements
  const imgs: HTMLImageElement[] = [];
  wrappers.forEach(wrapper => {
    const ele = wrapper.children[0];
    if (ele.tagName === 'IMG') {
      const img = ele as HTMLImageElement;
      img.removeAttribute('loading');
      imgs.push(img);
    }
  });

  // Wait for all images to load
  await Promise.all(imgs.map(img =>
    img.complete
    ? Promise.resolve()
    : new Promise<void>(res => img.addEventListener('load', () => res(), { once: true }))
  ));

  wrappers.forEach((wrapper, i) => {
    ctx.save();
    ctx.translate(canvas.width / 2 + dx * i, canvas.height / 2);
    ctx.rotate(dr * i);

    const ele = wrapper.children[0];
    if (ele.tagName === 'IMG') {
      const img = ele as HTMLImageElement;
      const scale = Math.min(180 / img.naturalWidth, 180 / img.naturalHeight);
      const w = img.naturalWidth * scale;
      const h = img.naturalHeight * scale;
      ctx.drawImage(img, -w/2, -h/2, w, h);

    } else if (ele.tagName === 'I') {
      const icon = ele as HTMLElement;
      const style = getComputedStyle(icon);
      const fontSize = parseFloat(style.fontSize);
      ctx.font = `${style.fontWeight} ${fontSize}px ${style.fontFamily}`;
      ctx.fillStyle = style.color;
      ctx.textBaseline = 'middle';
      ctx.textAlign = 'center';
      ctx.fillText(icon.textContent || '', 0, 0);
    }

    ctx.restore();
  });

  // Return as image
  const img = new Image();
  img.src = canvas.toDataURL();
  return img;
}


function handleDrop(e: Event) {
  console.log('drop')
  if (!(e instanceof DragEvent)) return;
  e.preventDefault();
  if (e.target instanceof HTMLElement) {
    const tgt = e.target.dataset.path;
    if (tgt === undefined) return;
    const json = e.dataTransfer!.getData('application/json')
    const src =  JSON.parse(json)

    console.log(src)
    if (typeof src === 'string') {
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
  dragging = null;
}
