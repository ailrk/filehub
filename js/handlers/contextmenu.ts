declare var htmx: any;


export function register () {
  window.addEventListener('contextmenu', e => e.preventDefault());
  document.querySelectorAll('#table tr').forEach(row => row.addEventListener('contextmenu', e => handle(row as HTMLElement, e as MouseEvent)))
}


export function handle(row: HTMLElement, e: MouseEvent) {
  const path = row.dataset.path!;
  let contextMenu = document.querySelector('#contextmenu')
  if (contextMenu) {
    contextMenu.remove();
  }
  fetch(`/contextmenu?file=${path}`)
    .then(res => res.text())
    .then(html => {
      const table = document.getElementById('table')
      if (table) {
        table.insertAdjacentHTML('afterend', html)
      } else {
        console.error('no table')
      }
      contextMenu = document.querySelector('#contextmenu')
      if (contextMenu) {
        init(contextMenu as HTMLElement)
        const event = new CustomEvent('Show', {
          detail: {
            pageX: e.pageX,
            pageY: e.pageY,
            path
          }
        })
        contextMenu.dispatchEvent(event)
      } else {
        console.error('contextmenu is not added')
      }
    })
}


function init(menu: HTMLElement) {
  htmx.process(menu);
  menu.addEventListener('Show', (e: any) => {
    let { pageX, pageY, path } = e.detail
    menu.style.left = `${pageX}px`;
    menu.style.top = `${pageY}px`;
    menu.style.position = 'absolute';
    menu.style.display = 'block';
    menu.dataset.path = path;
  })

  // wait for animation to end
  menu.addEventListener('Close', _ => {
    menu.classList.add('closing')
    menu.addEventListener('animationend', function h() {
      menu.removeEventListener('animationend', h) // Clean up the listener
      menu.remove()
    })
  })
}
