const IS_BROWSER = typeof window !== 'undefined' && typeof window.document !== 'undefined';
const NAMESPACE = 'imgviewer';


const TEMPLATE = (
  '<div class="imgviewer-container" tabindex="-1" touch-action="none">'
    + '<div class="imgviewer-canvas"></div>'
    + '<div class="imgviewer-footer">'
      + '<div class="imgviewer-title"></div>'
      + '<div class="imgviewer-toolbar"></div>'
    + '</div>'
    + '<div class="imgviewer-tooltip" role="alert" aria-hidden="true"></div>'
    + '<div class="imgviewer-button" role="button"></div>'
    + '<div class="imgviewer-player"></div>'
  + '</div>'
)

const getUniqueID = ((id) => (() => {
  id += 1;
  return id;
}))(-1);


/* States: */
type ViewerState =
  | "init"
  | "hiding"
  | "hidden"
  | "showing"
  | "shown"
  | "closing"
  | "closed";


function buildToolbar(toolbar: HTMLElement) {
  let list = document.createElement('ul');

  let prev = document.createElement('li');
  prev.setAttribute('role', 'button');
  prev.innerHTML = `<i class='bx bxs-left-arrow'></i>`;
  prev.classList.add(`${NAMESPACE}-prev`);
  list.appendChild(prev);

  let next = document.createElement('li');
  next.setAttribute('role', 'button');
  prev.innerHTML = `<i class='bx bxs-right-arrow'></i>`;
  next.classList.add(`${NAMESPACE}-next`);
  list.appendChild(next);

  toolbar.appendChild(list);
}


class Viewer {
  id: number;
  images: URL[] = [];
  currentImage?: HTMLElement;
  state: ViewerState = "init";
  index: number;
  viewer: HTMLElement;
  title: HTMLElement;
  navbar: HTMLElement;
  toolbar: HTMLElement;
  button: HTMLElement;
  canvas: HTMLElement;
  footer: HTMLElement;

  constructor(images: URL[], options?: { index: number }) {
    console.log(`state: ${this.state}`);
    this.images = images

    this.index = options?.index ?? 0;
    this.id = getUniqueID();

    // build
    let template = document.createElement('div');
    template.innerHTML = TEMPLATE;
    this.viewer = template.querySelector(`.${NAMESPACE}-container`)!;
    this.title = this.viewer.querySelector(`.${NAMESPACE}-title`)!;
    this.toolbar = this.viewer.querySelector(`.${NAMESPACE}-toolbar`)!;
    this.navbar = this.viewer.querySelector(`.${NAMESPACE}-navbar`)!;
    this.button = this.viewer.querySelector(`.${NAMESPACE}-button`)!;
    this.canvas = this.viewer.querySelector(`.${NAMESPACE}-canvas`)!;
    this.footer = this.viewer.querySelector(`.${NAMESPACE}-footer`)!;
    this.viewer.id = `${NAMESPACE}${this.id}`;
    this.title.id = `${NAMESPACE}-title-${this.id}`;

    this.button.classList.add(`${NAMESPACE}-close`);
    buildToolbar(this.toolbar);

    this.init();
  }

  loadImg() {
    let url = this.images[this.index];
    let img: HTMLImageElement = document.createElement('img');
    img.src = url.toString();
    this.canvas.innerHTML = '';
    this.canvas.appendChild(img);
    this.currentImage = img;
  }

  init() {
    this.loadImg();
    this.canvas.onclick  = e => {
      let target = e.target as HTMLElement;
      if (!target.matches('img')) {
        this.hide();
      }
    }

    this.button.onclick = _ => {
      this.hide();
    };

    (this.toolbar.querySelector(`.${NAMESPACE}-prev`) as HTMLEmbedElement).onclick = () => {
      this.prev();
    };

    (this.toolbar.querySelector(`.${NAMESPACE}-next`) as HTMLEmbedElement).onclick = () => {
      this.next();
    };

    this.state = 'hidden';
  }

  next() {
    this.index = (this.index + 1) % this.images.length;
    this.show(this.index);
  }

  prev() {
    this.index = (this.index - 1) < 0 ? 0 : this.index - 1;
    this.show(this.index);
  }

  show(index=this.index) {
    if (this.state !== 'hidden' && this.state !== 'shown') {
      return;
    }
    this.state = 'showing';
    console.log(`state: ${this.state}`);

    this.index = index;
    this.loadImg();
    this.render();
  }

  render() {
    if (this.state !== 'showing') {
      return;
    }
    document.querySelector('body')!.appendChild(this.viewer);
    this.state = 'shown';
    console.log(`state: ${this.state}`);
  }

  update(urls: URL[]) {

    if (this.state !== 'shown' && this.state !== 'hidden') return;
    console.log('udpate');

    this.images = urls
    this.init();

    if (this.state === 'shown') {
      this.state = 'showing';
      return;
    }
  }

  hide() {
    this.state = 'hiding';
    document.querySelector('body')!.removeChild(this.viewer);
    this.state = 'hidden';
    console.log(`state: ${this.state}`);
  }

  close() {
    if (this.state !== 'hidden' && this.state !== 'shown') {
      return;
    }
    this.state = 'closed';
    console.log(`state: ${this.state}`);
  }

  destroy() {
    throw new Error('not implemented')
  }
}


export default Viewer;
