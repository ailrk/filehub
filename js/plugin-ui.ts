import * as Balloon from './balloon.js';

export interface ModalHandle {
  readonly body: HTMLElement;
  show(): void;
  close(): void;
}

export interface PluginUI {
  modal(opts?: { title?: string }): ModalHandle;
  notify(msg: string, kind?: 'info' | 'error'): void;
}

export function createPluginUI(): PluginUI {
  return {

    modal(opts = {}) {
      const wrap = document.createElement('div');
      wrap.className = 'modal';

      const underlay = document.createElement('div');
      underlay.className = 'modal-underlay';

      const content = document.createElement('div');
      content.className = 'modal-content plugin-modal-content';

      const header = document.createElement('div');
      header.className = 'modal-title-bar';

      const title = document.createElement('span');
      title.textContent = opts.title ?? '';

      const closeBtn = document.createElement('button');
      closeBtn.className = 'btn-modal-close';
      closeBtn.setAttribute('data-close-modal', '');
      closeBtn.innerHTML = `<i class="bx bx-x"></i>`;

      header.appendChild(title);
      header.appendChild(closeBtn);
      content.appendChild(header);

      const body = document.createElement('div');
      body.className = 'plugin-modal-body';
      content.appendChild(body);

      wrap.appendChild(underlay);
      wrap.appendChild(content);

      return {
        body,
        show() {
          document.body.appendChild(wrap);
        },
        close() {
          wrap.dispatchEvent(new Event('Close', { bubbles: true }));
        },
      };
    },

    notify(msg, kind = 'info') {
      Balloon.pushBalloon({
        kind: kind === 'error' ? 'ErrorMsg' : 'InfoMsg',
        msg,
        duration: 3000,
      });
    },

  };
}
