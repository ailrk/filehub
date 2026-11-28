import * as Balloon from './balloon.js';
import * as Workspace from './hdesktop-workspace.js';
export function createPluginUI(pluginName) {
    return {
        window(opts = {}) {
            return Workspace.openWindow(pluginName, opts);
        },
        notify(msg, kind = 'info') {
            Balloon.pushBalloon({
                kind: kind === 'error' ? 'ErrorMsg' : 'InfoMsg',
                msg,
                duration: 3000,
            });
        },
        fetch(path, opts = {}) {
            return window.fetch(path, opts);
        },
        selectedFiles() {
            return Array.from(document.querySelectorAll('#table .table-item.selected[data-path]')).map(el => el.dataset.path);
        },
        refresh() {
            htmx.ajax('GET', '/', { target: '#index', swap: 'none' });
        },
    };
}
