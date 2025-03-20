const IS_BROWSER = typeof window !== 'undefined' && typeof window.document !== 'undefined';
const NAMESPACE = 'viewer';
const TEMPLATE = (`<div class="${NAMESPACE}-container" tabindex="-1" touch-action="none">`
    + `<div class="${NAMESPACE}-canvas"></div>`
    + `<div class="${NAMESPACE}-footer">`
    + `<div class="${NAMESPACE}-title"></div>`
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
    let list = document.createElement('ul');
    let prev = document.createElement('li');
    prev.setAttribute('role', 'button');
    prev.innerHTML = `<i class='bx bxs-left-arrow'></i>`;
    prev.classList.add(`${NAMESPACE}-prev`);
    list.appendChild(prev);
    let next = document.createElement('li');
    next.setAttribute('role', 'button');
    next.innerHTML = `<i class='bx bxs-right-arrow'></i>`;
    next.classList.add(`${NAMESPACE}-next`);
    list.appendChild(next);
    toolbar.appendChild(list);
}
class Viewer {
    id;
    images = [];
    currentImage;
    state = "init";
    index;
    viewer;
    title;
    navbar;
    toolbar;
    button;
    canvas;
    footer;
    constructor(images, options) {
        console.log(`state: ${this.state}`);
        this.images = images;
        this.index = options?.index ?? 0;
        this.id = getUniqueID();
        // build
        let template = document.createElement('div');
        template.innerHTML = TEMPLATE;
        this.viewer = template.querySelector(`.${NAMESPACE}-container`);
        this.title = this.viewer.querySelector(`.${NAMESPACE}-title`);
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
    loadImg() {
        let url = this.images[this.index];
        let img = document.createElement('img');
        img.src = url.toString();
        this.canvas.innerHTML = '';
        this.canvas.appendChild(img);
        this.currentImage = img;
    }
    init() {
        this.loadImg();
        this.canvas.onclick = e => {
            let target = e.target;
            if (!target.matches('img')) {
                this.hide();
            }
        };
        this.button.onclick = _ => {
            this.hide();
        };
        window.addEventListener('keydown', e => {
            if (this.state === 'shown') {
                switch (e.key) {
                    case "ArrowLeft":
                        this.prev();
                        break;
                    case "ArrowRight":
                        this.next();
                        break;
                    case "Escape":
                        this.hide();
                        break;
                    default: break;
                }
            }
        });
        this.toolbar.querySelector(`.${NAMESPACE}-prev`).onclick = () => {
            this.prev();
        };
        this.toolbar.querySelector(`.${NAMESPACE}-next`).onclick = () => {
            this.next();
        };
        this.state = 'hidden';
    }
    next() {
        this.index = (this.index + 1) > this.images.length - 1 ? this.images.length - 1 : this.index + 1;
        this.show(this.index);
    }
    prev() {
        this.index = (this.index - 1) < 0 ? 0 : this.index - 1;
        this.show(this.index);
    }
    show(index = this.index) {
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
        document.querySelector('body').appendChild(this.viewer);
        this.state = 'shown';
        console.log(`state: ${this.state}`);
    }
    hide() {
        this.state = 'hiding';
        document.querySelector('body').removeChild(this.viewer);
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
        throw new Error('not implemented');
    }
}
export default Viewer;
