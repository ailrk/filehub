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
    function add(c, i) {
        let t = document.createElement('li');
        t.setAttribute('role', 'button');
        t.innerHTML = i;
        t.classList.add(c);
        list.appendChild(t);
    }
    add(`${NAMESPACE}-prev`, `<i class='bx bxs-left-arrow'></i>`);
    add(`${NAMESPACE}-next`, `<i class='bx bxs-right-arrow'></i>`);
    toolbar.appendChild(list);
}
class Viewer {
    id;
    resources = [];
    currentContent;
    state = "init";
    index;
    viewer;
    title;
    navbar;
    toolbar;
    button;
    canvas;
    footer;
    constructor(resources, options) {
        this.resources = resources;
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
    load() {
        let resource = this.resources[this.index];
        let [url, mimetype] = resource;
        let content = document.createElement('div');
        content.innerHTML = 'No content';
        if (mimetype.startsWith('image')) {
            let img = document.createElement('img');
            img.src = url.toString();
            content = img;
        }
        else if (mimetype.startsWith('video')) {
            let video = document.createElement('video');
            let source = document.createElement('source');
            video.setAttribute('controls', '');
            video.setAttribute('autoplay', '');
            video.setAttribute('loop', '');
            video.setAttribute('muted', '');
            source.src = url.toString();
            video.appendChild(source);
            content = video;
        }
        else if (mimetype.startsWith('audio')) {
            let audio = document.createElement('audio');
            let source = document.createElement('source');
            audio.setAttribute('controls', '');
            source.src = url.toString();
            audio.appendChild(source);
            content = audio;
        }
        this.canvas.innerHTML = '';
        this.canvas.appendChild(content);
        this.currentContent = content;
    }
    init() {
        this.load();
        this.canvas.onclick = e => {
            let target = e.target;
            if (!this.canvas.contains(target) || this.canvas === target) {
                this.hide();
            }
        };
        this.button.onclick = _ => {
            this.hide();
        };
        document.addEventListener('keydown', e => {
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
        this.index = (this.index + 1) > this.resources.length - 1 ? this.resources.length - 1 : this.index + 1;
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
        this.index = index;
        this.load();
        this.render();
    }
    render() {
        if (this.state !== 'showing') {
            return;
        }
        document.querySelector('body').appendChild(this.viewer);
        this.currentContent.focus();
        this.state = 'shown';
    }
    hide() {
        this.state = 'hiding';
        document.querySelector('body').removeChild(this.viewer);
        this.state = 'hidden';
    }
    close() {
        if (this.state !== 'hidden' && this.state !== 'shown') {
            return;
        }
        this.state = 'closed';
    }
    destroy() {
        throw new Error('not implemented');
    }
}
export default Viewer;
