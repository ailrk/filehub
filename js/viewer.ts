import type { Resource } from './def.js';

const NAMESPACE = 'viewer';

const TEMPLATE = (
  `<div class="${NAMESPACE}-container" tabindex="-1" touch-action="none">`
    + `<div class="${NAMESPACE}-canvas"></div>`
    + `<div class="${NAMESPACE}-footer">`
      + `<div class="${NAMESPACE}-title"></div>`
      + `<div class="${NAMESPACE}-info"></div>`
      + `<div class="${NAMESPACE}-toolbar"></div>`
    + '</div>'
    + `<div class="${NAMESPACE}-tooltip" role="alert" aria-hidden="true"></div>`
    + `<div class="${NAMESPACE}-button" role="button"></div>`
    + `<div class="${NAMESPACE}-player"></div>`
  + '</div>'
)

const getUniqueID = ((id) => (() => {
  id += 1;
  return id;
}))(-1);


type ViewerState =
  | "init"
  | "hiding"
  | "hidden"
  | "showing"
  | "shown"
  | "closing"
  | "closed";


function buildToolbar(toolbar: HTMLElement) {
  const list = document.createElement('ul');

  function add(c: string, content: string, isButton = true) {
    const t = document.createElement('li');
    if (isButton) t.setAttribute('role', 'button');
    t.innerHTML = content;
    t.classList.add(c);
    list.appendChild(t);
  }

  add(`${NAMESPACE}-rotate`,     `<i class='bx bx-rotate-right'></i>`);
  add(`${NAMESPACE}-prev`,       `<i class='bx bxs-left-arrow'></i>`);
  add(`${NAMESPACE}-play`,       `<i class='bx bx-play'></i>`);
  add(`${NAMESPACE}-speed`,      `3s`, false);
  add(`${NAMESPACE}-next`,       `<i class='bx bxs-right-arrow'></i>`);
  add(`${NAMESPACE}-fullscreen`, `<i class='bx bx-fullscreen'></i>`);

  toolbar.appendChild(list);
}


class Viewer {
  id: number;
  resources: Resource[] = [];
  currentContent?: HTMLElement;
  state: ViewerState = "init";
  index: number;
  rotation: number = 0;
  playing: boolean = false;
  playInterval: number = 3000;
  private playTimer?: number;
  private footerHideTimer?: number;
  private readonly onViewerMouseMove = () => {
    this.viewer.classList.remove('footer-hidden');
    this.scheduleFooterHide();
  };
  viewer: HTMLElement;
  title: HTMLElement;
  info: HTMLElement;
  navbar: HTMLElement;
  toolbar: HTMLElement;
  button: HTMLElement;
  canvas: HTMLElement;
  footer: HTMLElement;

  constructor(resources: Resource[], options?: { index: number }) {
    this.resources = resources;

    this.index = options?.index ?? 0;
    this.id = getUniqueID();

    const template = document.createElement('div');
    template.innerHTML = TEMPLATE;
    this.viewer = template.querySelector(`.${NAMESPACE}-container`)!;
    this.title = this.viewer.querySelector(`.${NAMESPACE}-title`)!;
    this.info = this.viewer.querySelector(`.${NAMESPACE}-info`)!;
    this.toolbar = this.viewer.querySelector(`.${NAMESPACE}-toolbar`)!;
    this.navbar = this.viewer.querySelector(`.${NAMESPACE}-navbar`)!;
    this.button = this.viewer.querySelector(`.${NAMESPACE}-button`)!;
    this.canvas = this.viewer.querySelector(`.${NAMESPACE}-canvas`)!;
    this.footer = this.viewer.querySelector(`.${NAMESPACE}-footer`)!;
    this.viewer.id = `${NAMESPACE}${this.id}`;
    this.title.id = `${NAMESPACE}-title-${this.id}`;

    this.button.classList.add(`${NAMESPACE}-close`);
    this.button.innerHTML = `<i class='bx bx-x'></i>`;
    buildToolbar(this.toolbar);

    this.init();
  }

  load() {
    const resource = this.resources[this.index];
    const [url, mimetype] = resource;

    let content: HTMLElement = document.createElement('div');
    content.innerHTML = 'No content';

    this.info.textContent = '';

    if (mimetype.startsWith('image')) {
      const img = document.createElement('img');
      img.src = url.toString();
      img.style.transform = `rotate(${this.rotation}deg)`;
      img.addEventListener('load', () => {
        this.info.textContent = `${img.naturalWidth} × ${img.naturalHeight}`;
      });
      content = img;

    } else if (mimetype.startsWith('video')) {
      const video = document.createElement('video');
      const source = document.createElement('source');
      video.setAttribute('controls', '');
      video.setAttribute('autoplay', '');
      video.setAttribute('loop', '');
      video.setAttribute('muted', '');
      source.src = url.toString();
      source.type = mimetype;
      video.appendChild(source);
      content = video;

    } else if (mimetype.startsWith('audio')) {
      const audio = document.createElement('audio');
      const source = document.createElement('source');
      audio.setAttribute('controls', '');
      source.src = url.toString();
      source.type = mimetype;
      audio.appendChild(source);
      content = audio;
    }

    this.canvas.innerHTML = '';
    this.canvas.appendChild(content);

    const rawFile = (new URL(url, 'https://base.com')).searchParams.get('file') ?? '';
    this.title.textContent = decodeURIComponent(rawFile).split('/').pop() ?? rawFile;

    content.classList.add('fade');
    this.currentContent = content;

    const start = Math.max(0, this.index - 10);
    const end   = Math.min(this.resources.length, this.index + 11);
    for (let j = start; j < end; j++) {
      if (j === this.index) continue;
      const [preUrl, preMime] = this.resources[j];
      if (preMime.startsWith('image')) {
        const preImg = new Image();
        preImg.src = preUrl;
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
      this.viewer.requestFullscreen().catch(() => {});
    } else {
      document.exitFullscreen();
    }
  }

  play() {
    this.playing = true;
    this.playTimer = window.setInterval(() => this.advance(), this.playInterval);
    const icon = this.toolbar.querySelector<HTMLElement>(`.${NAMESPACE}-play i`);
    if (icon) icon.className = 'bx bx-pause';
    this.toolbar.querySelector(`.${NAMESPACE}-play`)?.classList.add('active');
  }

  pause() {
    this.playing = false;
    clearInterval(this.playTimer);
    this.playTimer = undefined;
    const icon = this.toolbar.querySelector<HTMLElement>(`.${NAMESPACE}-play i`);
    if (icon) icon.className = 'bx bx-play';
    this.toolbar.querySelector(`.${NAMESPACE}-play`)?.classList.remove('active');
  }

  togglePlay() {
    if (this.playing) this.pause();
    else this.play();
  }

  setPlayInterval(ms: number) {
    this.playInterval = Math.max(500, Math.min(10000, ms));
    const speedEl = this.toolbar.querySelector<HTMLElement>(`.${NAMESPACE}-speed`);
    if (speedEl) speedEl.textContent = formatInterval(this.playInterval);
    if (this.playing) {
      clearInterval(this.playTimer);
      this.playTimer = window.setInterval(() => this.advance(), this.playInterval);
    }
  }

  advance() {
    this.show((this.index + 1) % this.resources.length);
  }

  init() {
    // Suppress custom contextmenu handler inside the viewer so the browser's
    // native right-click menu works on images (save, copy image, etc.).
    // Attached to canvas so it persists across prev/next navigation.
    this.canvas.addEventListener('contextmenu', e => {
      e.stopImmediatePropagation();
    }, { capture: true });

    this.canvas.onclick = e => {
      const target = e.target as HTMLElement;
      if (!this.canvas.contains(target) || this.canvas === target) {
        this.hide();
      }
    };
    this.button.onclick = _ => this.hide();

    document.addEventListener('keydown', e => {
      if (this.state === 'shown') {
        switch (e.key) {
          case 'ArrowLeft':  this.prev();              break;
          case 'ArrowRight': this.next();              break;
          case 'Escape':     this.hide();              break;
          case 'r': case 'R': this.rotate();           break;
          case 'f': case 'F': this.toggleFullscreen(); break;
          case ' ': e.preventDefault(); this.togglePlay(); break;
          default: break;
        }
      }
    });

    document.addEventListener('fullscreenchange', () => {
      const icon = this.toolbar.querySelector<HTMLElement>(`.${NAMESPACE}-fullscreen i`);
      if (!icon) return;
      if (document.fullscreenElement === this.viewer) {
        icon.className = 'bx bx-exit-fullscreen';
        this.startFooterAutoHide();
      } else {
        icon.className = 'bx bx-fullscreen';
        this.stopFooterAutoHide();
      }
    });

    (this.toolbar.querySelector(`.${NAMESPACE}-prev`)       as HTMLElement).onclick = () => this.prev();
    (this.toolbar.querySelector(`.${NAMESPACE}-next`)       as HTMLElement).onclick = () => this.next();
    (this.toolbar.querySelector(`.${NAMESPACE}-rotate`)     as HTMLElement).onclick = () => this.rotate();
    (this.toolbar.querySelector(`.${NAMESPACE}-fullscreen`) as HTMLElement).onclick = () => this.toggleFullscreen();
    (this.toolbar.querySelector(`.${NAMESPACE}-play`)       as HTMLElement).onclick = () => this.togglePlay();

    // Drag on the speed indicator to adjust play interval.
    // Drag right = slower, drag left = faster. 80ms per pixel.
    const speedEl = this.toolbar.querySelector<HTMLElement>(`.${NAMESPACE}-speed`)!;
    speedEl.addEventListener('mousedown', (e: MouseEvent) => {
      e.preventDefault();
      const startX = e.clientX;
      const startInterval = this.playInterval;

      const onMove = (me: MouseEvent) => {
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
    this.index = Math.min(this.index + 1, this.resources.length - 1);
    this.show(this.index);
  }

  prev() {
    this.index = Math.max(this.index - 1, 0);
    this.show(this.index);
  }

  show(index = this.index) {
    if (this.state !== 'hidden' && this.state !== 'shown') return;
    this.state = 'showing';
    this.index = index;
    this.load();
    this.render();
  }

  render() {
    if (this.state !== 'showing') return;
    if (!this.viewer.isConnected) {
      document.querySelector('body')!.appendChild(this.viewer);
    }
    this.currentContent!.focus();
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

  private scheduleFooterHide() {
    clearTimeout(this.footerHideTimer);
    this.footerHideTimer = window.setTimeout(() => {
      this.viewer.classList.add('footer-hidden');
    }, 2000);
  }

  hide() {
    if (document.fullscreenElement) document.exitFullscreen();
    this.pause();
    this.stopFooterAutoHide();
    this.rotation = 0;
    this.state = 'hiding';
    document.querySelector('body')!.removeChild(this.viewer);
    this.state = 'hidden';
  }

  close() {
    if (this.state !== 'hidden' && this.state !== 'shown') return;
    this.state = 'closed';
  }

  destroy() {
    throw new Error('not implemented');
  }
}


function formatInterval(ms: number): string {
  const s = ms / 1000;
  return (Number.isInteger(s) ? s.toFixed(0) : s.toFixed(1)) + 's';
}


export default Viewer;
