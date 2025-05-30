declare var htmx: any;
/* Ctrl_left-left_click to multi select
 * Hold ctrl left and click on file will select it.
 * Keep ctrl hold and click multiple files will select multiple.
 * Keep ctrl hold, click on a file and drag will select files on the pass.
 * */

const selectedIds: Set<string> = new Set();
let clearSelectedHandler: EventListener = _ => selectedIds.clear();

export function register() {
  document.body.addEventListener('TargetChanged', _ => { selectedIds.clear() })
  document.body.addEventListener('htmx:afterSettle', e => {
    unregisterAll()
    registerAll()
    clearSelectedHandler(e)
    collectFromHtml()
  })

  registerAll();
}


function registerAll() {
  const table = document.querySelector('#table'); // change selector to fit your layout
  table!.addEventListener('click', handleRecord, true);
}


function unregisterAll() {
  const table = document.querySelector('#table'); // change selector to fit your layout
  table!.removeEventListener('click', handleRecord);
}


function handleRecord(e: Event) {
  let tr = (e.target as Element).closest('tr');
  if (tr) {
    handle(tr, e)
  }
}

// Collect selected rows to the set `selectedIds`
function collectFromHtml() {
  let rows: NodeListOf<HTMLElement> = document.querySelectorAll('#table tr')
  rows.forEach(row => {
    const id = row.dataset.path!
    if (row.classList.contains('selected')) {
      selectedIds.add(id)
    }
  })
}

/* Select handler */
export function handle(row: HTMLElement, evt: Event) {
  let e = evt as MouseEvent;
  const id = row.dataset.path!
  // disable the default behavior when ctrl is pressed.
  if (e.ctrlKey || e.metaKey) {
    e.preventDefault()
    e.stopImmediatePropagation()
  } else
    return;

  function select(
    hooks: {
      prepare: () => void,
      confirm: () => void,
      recover: () => void
    }) {
    hooks.prepare()
    const values: Record<string, string[]> = { selected: Array.from(selectedIds) }
    htmx.ajax('POST',
      '/table/select',
      {
        values,
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded'
        },
        target: '#control-panel',
        swap: 'outerHTML'
      }
    ).then((_: any) => {
      hooks.confirm()
    }).catch((_: any) => {
      hooks.recover()
      console.error('failed to select')
    })
  }

  if (e.ctrlKey || e.metaKey) {
    if (selectedIds.has(id)) {
      select({
        prepare: () => { selectedIds.delete(id) },
        confirm: () => { row.classList.remove('selected') },
        recover: () => { selectedIds.add(id) }
      })
    } else {
      select({
        prepare: () => { selectedIds.add(id) },
        confirm: () => { row.classList.add('selected') },
        recover: () => { selectedIds.delete(id) }
      })
    }
  }
}
