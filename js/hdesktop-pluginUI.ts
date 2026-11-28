import * as Balloon from './balloon.js';
import * as Workspace from './hdesktop-workspace.js';
export type { WindowHandle } from './hdesktop-window.js';

declare var htmx: any;

export interface PluginUI {
  window(opts?: { title?: string; width?: number; height?: number }): import('./hdesktop-window.js').WindowHandle;
  notify(msg: string, kind?: 'info' | 'error'): void;
  fetch(path: string, opts?: RequestInit): Promise<Response>;
  selectedFiles(): string[];
  refresh(): void;
}

export function createPluginUI(pluginName: string): PluginUI {
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
      return Array.from(
        document.querySelectorAll<HTMLElement>('#table .table-item.selected[data-path]')
      ).map(el => el.dataset.path!);
    },

    refresh() {
      htmx.ajax('GET', '/', { target: '#index', swap: 'none' });
    },

  };
}
