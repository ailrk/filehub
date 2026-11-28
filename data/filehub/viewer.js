const NAMESPACE = 'viewer';
const TEMPLATE = (`<div class="${NAMESPACE}-container" tabindex="-1" touch-action="none">`
    + `<div class="${NAMESPACE}-canvas"></div>`
    + `<div class="${NAMESPACE}-footer">`
    + `<div class="${NAMESPACE}-title"></div>`
    + `<div class="${NAMESPACE}-info"></div>`
    + `<div class="${NAMESPACE}-toolbar"></div>`
    + '</div>'
    + `<div class="${NAMESPACE}-tooltip" role="alert" aria-hidden="true"></div>`
    + `<div class="${NAMESPACE}-button" role="button"></div>`
    + `<div class="${NAMESPACE}-player"></div>`
    + '</div>');
const getUniqueID = ((id) => (() => {
    id += 1;
    return id;
}))(-1);
function buildToolbar(toolbar) {
    const list = document.createElement('ul');
    function add(c, content, isButton = true) {
        const t = document.createElement('li');
        if (isButton)
            t.setAttribute('role', 'button');
        t.innerHTML = content;
        t.classList.add(c);
        list.appendChild(t);
    }
    add(`${NAMESPACE}-rotate`, `<i class='bx bx-rotate-right'></i>`);
    add(`${NAMESPACE}-prev`, `<i class='bx bxs-left-arrow'></i>`);
    add(`${NAMESPACE}-play`, `<i class='bx bx-play'></i>`);
    add(`${NAMESPACE}-speed`, `3s`, false);
    add(`${NAMESPACE}-next`, `<i class='bx bxs-right-arrow'></i>`);
    add(`${NAMESPACE}-fullscreen`, `<i class='bx bx-fullscreen'></i>`);
    toolbar.appendChild(list);
}
function isMedia(mime) {
    return mime.startsWith('image') || mime.startsWith('video') || mime.startsWith('audio');
}
function mediaItems() {
    return Array.from(document.querySelectorAll('#table .table-item'))
        .filter(el => el.dataset.ftype !== 'dir' && isMedia(el.dataset.mime ?? ''));
}
class Viewer {
    id;
    currentPath;
    currentContent;
    state = "init";
    rotation = 0;
    playing = false;
    playInterval = 3000;
    playTimer;
    footerHideTimer;
    onViewerMouseMove = () => {
        this.viewer.classList.remove('footer-hidden');
        this.scheduleFooterHide();
    };
    viewer;
    title;
    info;
    navbar;
    toolbar;
    button;
    canvas;
    footer;
    serveUrlOverride;
    mimeOverride;
    constructor(initialPath, serveUrlOverride, mimeOverride) {
        this.currentPath = initialPath;
        this.serveUrlOverride = serveUrlOverride;
        this.mimeOverride = mimeOverride;
        this.id = getUniqueID();
        const template = document.createElement('div');
        template.innerHTML = TEMPLATE;
        this.viewer = template.querySelector(`.${NAMESPACE}-container`);
        this.title = this.viewer.querySelector(`.${NAMESPACE}-title`);
        this.info = this.viewer.querySelector(`.${NAMESPACE}-info`);
        this.toolbar = this.viewer.querySelector(`.${NAMESPACE}-toolbar`);
        this.navbar = this.viewer.querySelector(`.${NAMESPACE}-navbar`);
        this.button = this.viewer.querySelector(`.${NAMESPACE}-button`);
        this.canvas = this.viewer.querySelector(`.${NAMESPACE}-canvas`);
        this.footer = this.viewer.querySelector(`.${NAMESPACE}-footer`);
        this.viewer.id = `${NAMESPACE}${this.id}`;
        this.title.id = `${NAMESPACE}-title-${this.id}`;
        this.button.classList.add(`${NAMESPACE}-close`);
        this.button.innerHTML = `<i class='bx bx-x'></i>`;
        buildToolbar(this.toolbar);
        this.init();
    }
    load() {
        const items = mediaItems();
        const idx = items.findIndex(el => el.dataset.path === this.currentPath);
        const el = items[idx];
        const mime = this.mimeOverride ?? el?.dataset.mime ?? '';
        const url = this.serveUrlOverride ?? `/serve?file=${el?.dataset.path ?? ''}`;
        if (!el && !this.serveUrlOverride)
            return;
        let content = document.createElement('div');
        content.innerHTML = 'No content';
        this.info.textContent = '';
        if (mime.startsWith('image')) {
            const img = document.createElement('img');
            img.src = url;
            img.style.transform = `rotate(${this.rotation}deg)`;
            img.addEventListener('load', () => {
                this.info.textContent = `${img.naturalWidth} × ${img.naturalHeight}`;
            });
            content = img;
        }
        else if (mime.startsWith('video')) {
            const video = document.createElement('video');
            const source = document.createElement('source');
            video.setAttribute('controls', '');
            video.setAttribute('autoplay', '');
            video.setAttribute('loop', '');
            video.setAttribute('muted', '');
            video.setAttribute('playsinline', '');
            video.setAttribute('webkit-playsinline', '');
            source.src = url;
            source.type = mime;
            video.appendChild(source);
            content = video;
        }
        else if (mime.startsWith('audio')) {
            const audio = document.createElement('audio');
            const source = document.createElement('source');
            audio.setAttribute('controls', '');
            source.src = url;
            source.type = mime;
            audio.appendChild(source);
            content = audio;
        }
        this.canvas.innerHTML = '';
        this.canvas.appendChild(content);
        this.title.textContent = decodeURIComponent(el?.dataset.path ?? this.currentPath).split('/').pop() ?? '';
        content.classList.add('fade');
        this.currentContent = content;
        if (el) {
            const start = Math.max(0, idx - 20);
            const end = Math.min(items.length, idx + 21);
            for (let j = start; j < end; j++) {
                if (j === idx)
                    continue;
                const item = items[j];
                if ((item.dataset.mime ?? '').startsWith('image')) {
                    const preImg = new Image();
                    preImg.src = `/serve?file=${item.dataset.path ?? ''}`;
                }
            }
        }
    }
    rotate() {
        this.rotation = (this.rotation + 90) % 360;
        if (this.currentContent instanceof HTMLImageElement) {
            this.currentContent.style.transform = `rotate(${this.rotation}deg)`;
        }
        const btn = this.toolbar.querySelector(`.${NAMESPACE}-rotate`);
        btn?.classList.toggle('active', this.rotation !== 0);
    }
    toggleFullscreen() {
        if (!document.fullscreenElement) {
            this.viewer.requestFullscreen().catch(() => { });
        }
        else {
            document.exitFullscreen();
        }
    }
    play() {
        this.playing = true;
        this.playTimer = window.setInterval(() => this.advance(), this.playInterval);
        const icon = this.toolbar.querySelector(`.${NAMESPACE}-play i`);
        if (icon)
            icon.className = 'bx bx-pause';
        this.toolbar.querySelector(`.${NAMESPACE}-play`)?.classList.add('active');
    }
    pause() {
        this.playing = false;
        clearInterval(this.playTimer);
        this.playTimer = undefined;
        const icon = this.toolbar.querySelector(`.${NAMESPACE}-play i`);
        if (icon)
            icon.className = 'bx bx-play';
        this.toolbar.querySelector(`.${NAMESPACE}-play`)?.classList.remove('active');
    }
    togglePlay() {
        if (this.playing)
            this.pause();
        else
            this.play();
    }
    setPlayInterval(ms) {
        this.playInterval = Math.max(500, Math.min(10000, ms));
        const speedEl = this.toolbar.querySelector(`.${NAMESPACE}-speed`);
        if (speedEl)
            speedEl.textContent = formatInterval(this.playInterval);
        if (this.playing) {
            clearInterval(this.playTimer);
            this.playTimer = window.setInterval(() => this.advance(), this.playInterval);
        }
    }
    advance() {
        const items = mediaItems();
        const idx = items.findIndex(el => el.dataset.path === this.currentPath);
        const next = items[(idx + 1) % items.length];
        if (next) {
            this.currentPath = next.dataset.path ?? this.currentPath;
            this.show();
        }
    }
    init() {
        // Suppress custom contextmenu handler inside the viewer so the browser's
        // native right-click menu works on images (save, copy image, etc.).
        // Attached to canvas so it persists across prev/next navigation.
        this.canvas.addEventListener('contextmenu', e => {
            e.stopImmediatePropagation();
        }, { capture: true });
        this.canvas.onclick = e => {
            const target = e.target;
            if (!this.canvas.contains(target) || this.canvas === target) {
                this.hide();
            }
        };
        this.button.onclick = _ => this.hide();
        document.addEventListener('keydown', e => {
            if (this.state === 'shown') {
                switch (e.key) {
                    case 'ArrowLeft':
                        this.prev();
                        break;
                    case 'ArrowRight':
                        this.next();
                        break;
                    case 'Escape':
                        this.hide();
                        break;
                    case 'r':
                    case 'R':
                        this.rotate();
                        break;
                    case 'f':
                    case 'F':
                        this.toggleFullscreen();
                        break;
                    case ' ':
                        e.preventDefault();
                        this.togglePlay();
                        break;
                    default: break;
                }
            }
        });
        document.addEventListener('fullscreenchange', () => {
            const icon = this.toolbar.querySelector(`.${NAMESPACE}-fullscreen i`);
            if (!icon)
                return;
            if (document.fullscreenElement === this.viewer) {
                icon.className = 'bx bx-exit-fullscreen';
                this.startFooterAutoHide();
            }
            else {
                icon.className = 'bx bx-fullscreen';
                this.stopFooterAutoHide();
            }
        });
        this.toolbar.querySelector(`.${NAMESPACE}-prev`).onclick = () => this.prev();
        this.toolbar.querySelector(`.${NAMESPACE}-next`).onclick = () => this.next();
        this.toolbar.querySelector(`.${NAMESPACE}-rotate`).onclick = () => this.rotate();
        this.toolbar.querySelector(`.${NAMESPACE}-fullscreen`).onclick = () => this.toggleFullscreen();
        this.toolbar.querySelector(`.${NAMESPACE}-play`).onclick = () => this.togglePlay();
        // Drag on the speed indicator to adjust play interval.
        // Drag right = slower, drag left = faster. 80ms per pixel.
        const speedEl = this.toolbar.querySelector(`.${NAMESPACE}-speed`);
        speedEl.addEventListener('mousedown', (e) => {
            e.preventDefault();
            const startX = e.clientX;
            const startInterval = this.playInterval;
            const onMove = (me) => {
                this.setPlayInterval(startInterval + (me.clientX - startX) * 80);
            };
            const onUp = () => {
                document.removeEventListener('mousemove', onMove);
                document.removeEventListener('mouseup', onUp);
                speedEl.classList.remove('dragging');
            };
            speedEl.classList.add('dragging');
            document.addEventListener('mousemove', onMove);
            document.addEventListener('mouseup', onUp);
        });
        this.state = 'hidden';
    }
    next() {
        const items = mediaItems();
        const idx = items.findIndex(el => el.dataset.path === this.currentPath);
        if (idx < items.length - 1) {
            this.currentPath = items[idx + 1].dataset.path ?? this.currentPath;
            this.show();
        }
    }
    prev() {
        const items = mediaItems();
        const idx = items.findIndex(el => el.dataset.path === this.currentPath);
        if (idx > 0) {
            this.currentPath = items[idx - 1].dataset.path ?? this.currentPath;
            this.show();
        }
    }
    show() {
        if (this.state !== 'hidden' && this.state !== 'shown')
            return;
        this.state = 'showing';
        this.load();
        this.render();
    }
    render() {
        if (this.state !== 'showing')
            return;
        if (!this.viewer.isConnected) {
            document.querySelector('body').appendChild(this.viewer);
        }
        this.currentContent.focus();
        this.state = 'shown';
    }
    startFooterAutoHide() {
        this.viewer.addEventListener('mousemove', this.onViewerMouseMove);
        this.scheduleFooterHide();
    }
    stopFooterAutoHide() {
        this.viewer.removeEventListener('mousemove', this.onViewerMouseMove);
        clearTimeout(this.footerHideTimer);
        this.viewer.classList.remove('footer-hidden');
    }
    scheduleFooterHide() {
        clearTimeout(this.footerHideTimer);
        this.footerHideTimer = window.setTimeout(() => {
            this.viewer.classList.add('footer-hidden');
        }, 2000);
    }
    hide() {
        if (document.fullscreenElement)
            document.exitFullscreen();
        this.pause();
        this.stopFooterAutoHide();
        this.rotation = 0;
        this.state = 'hiding';
        document.querySelector('body').removeChild(this.viewer);
        this.state = 'hidden';
    }
    close() {
        if (this.state !== 'hidden' && this.state !== 'shown')
            return;
        this.state = 'closed';
    }
    destroy() {
        throw new Error('not implemented');
    }
}
function formatInterval(ms) {
    const s = ms / 1000;
    return (Number.isInteger(s) ? s.toFixed(0) : s.toFixed(1)) + 's';
}
export default Viewer;
