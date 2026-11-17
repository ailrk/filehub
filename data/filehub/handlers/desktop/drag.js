/*
 * Desktop drag-and-drop for moving files into folders.
 * Registered once on document — no re-registration after htmx swaps.
 */
export function register() {
    document.addEventListener('dragstart', onDragStart);
    document.addEventListener('drop', onDrop);
    document.addEventListener('dragover', onDragOver);
}
function onDragOver(e) {
    if (e.target.closest('.dir'))
        e.preventDefault();
}
function onDragStart(e) {
    const item = e.target.closest('.table-item');
    if (!item)
        return;
    const path = item.dataset.path;
    if (!path)
        return;
    e.dataTransfer.clearData();
    e.dataTransfer.dropEffect = 'move';
    if (item.classList.contains('selected')) {
        const allSelected = Array.from(document.querySelectorAll('.selected'));
        const heads = allSelected.slice(0, 6)
            .map(el => el.querySelector('.image-wrapper'))
            .filter((el) => el !== null);
        drawIconAsync(heads).then(img => e.dataTransfer.setDragImage(img, 180, 180));
        const paths = allSelected.map(el => el.dataset.path).filter(Boolean);
        e.dataTransfer.setData('application/json', JSON.stringify(paths));
    }
    else {
        e.dataTransfer.setData('application/json', JSON.stringify(path));
    }
}
function onDrop(e) {
    const dir = e.target.closest('.dir');
    if (!dir)
        return;
    e.preventDefault();
    const tgt = dir.dataset.path;
    if (!tgt)
        return;
    let src;
    try {
        src = JSON.parse(e.dataTransfer.getData('application/json'));
    }
    catch {
        return;
    }
    if (typeof src === 'string') {
        if (src === tgt)
            return;
    }
    htmx.ajax('POST', '/files/move', {
        values: { src, tgt },
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        target: 'body',
        swap: 'none',
    });
}
async function drawIconAsync(wrappers) {
    const canvas = document.createElement('canvas');
    const theme = getComputedStyle(document.documentElement);
    const ctx = canvas.getContext('2d');
    const imgs = [];
    const shift = Math.min(wrappers.length * 10, 50);
    wrappers.forEach(wrapper => {
        const el = wrapper.children[0];
        if (el?.tagName === 'IMG') {
            const img = el;
            img.removeAttribute('loading');
            imgs.push(img);
        }
    });
    canvas.width = canvas.height = 500;
    await Promise.all(imgs.map(img => img.complete ? Promise.resolve()
        : new Promise(res => img.addEventListener('load', () => res(), { once: true }))));
    await document.fonts.ready;
    fillRoundedRect(ctx, 0, 0, 500, 500, 16, theme.getPropertyValue('--background3'));
    wrappers
        .sort((a, b) => {
        const ta = a.children[0]?.tagName ?? '';
        const tb = b.children[0]?.tagName ?? '';
        if (ta === 'IMG' && tb === 'I')
            return -1;
        if (ta === 'I' && tb === 'IMG')
            return 1;
        return 0;
    })
        .forEach((wrapper, i) => {
        ctx.save();
        ctx.translate(250 + 8 * i, 250);
        ctx.rotate((4 * i * Math.PI) / 180);
        const el = wrapper.children[0];
        if (el?.tagName === 'IMG') {
            const img = el;
            const scale = Math.min(300 / img.naturalWidth, 300 / img.naturalHeight);
            const w = img.naturalWidth * scale, h = img.naturalHeight * scale;
            ctx.drawImage(img, (-w / 2) - shift, -h / 2, w, h);
        }
        else if (el?.tagName === 'I') {
            const style = getComputedStyle(el, '::before');
            const fontSize = 2 * parseFloat(style.fontSize);
            ctx.font = `${style.fontWeight} ${fontSize}px ${style.fontFamily}`;
            ctx.fillStyle = style.color;
            ctx.textBaseline = 'middle';
            ctx.textAlign = 'center';
            ctx.fillText(style.content.replace(/^"(.*)"$/, '$1') || '□', 0, 0);
        }
        ctx.restore();
    });
    const img = new Image();
    img.src = canvas.toDataURL();
    img.style.borderRadius = '6px';
    return img;
}
function fillRoundedRect(ctx, x, y, w, h, r, fill) {
    ctx.beginPath();
    ctx.moveTo(x + r, y);
    ctx.lineTo(x + w - r, y);
    ctx.arcTo(x + w, y, x + w, y + r, r);
    ctx.lineTo(x + w, y + h - r);
    ctx.arcTo(x + w, y + h, x + w - r, y + h, r);
    ctx.lineTo(x + r, y + h);
    ctx.arcTo(x, y + h, x, y + h - r, r);
    ctx.lineTo(x, y + r);
    ctx.arcTo(x, y, x + r, y, r);
    ctx.closePath();
    ctx.fillStyle = fill;
    ctx.fill();
}
