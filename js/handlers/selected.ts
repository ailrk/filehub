let selectedIds: Set<string> = new Set();
const handlers: Map<string, EventListener> = new Map();


export function register() {
  registerRows()
  document.body.addEventListener('TargetChanged', _ => { selectedIds.clear() })
  document.body.addEventListener('htmx:afterSettle', _ => {
    collect()
    registerRows()
  })
}


function registerRows() {
  unregisterRows()
  let rows: NodeListOf<HTMLElement> = document.querySelectorAll('#table tr')
  rows.forEach(row => {
    const id = row.dataset.path!
    const h = (e: Event) => handle(row, e)
    handlers.set(id, h)
    row.addEventListener('click', h, true)
  })
}


function unregisterRows() {
  let rows: NodeListOf<HTMLElement> = document.querySelectorAll('#table tr')
  rows.forEach(row => {
    const id = row.dataset.path!
    const h = handlers.get(id)
    h && row.removeEventListener('click', h, true)
  })
}


// Collect selected rows to the set `selectedIds`
function collect() {
  let rows: NodeListOf<HTMLElement> = document.querySelectorAll('#table tr')
  rows.forEach(row => {
    const id = row.dataset.path!
    if (row.classList.contains('selected')) {
      selectedIds.add(id)
    }
  })
}


function handle(row: Element, evt: Event) {
  let e = evt as MouseEvent;
  const id = (row as HTMLElement).dataset.path!
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
    let payload = new URLSearchParams()
    selectedIds.forEach(id => payload.append("selected", id))
    fetch('/table/select',
      { method: 'POST',
        body: payload,
        headers: {
          "Content-Type": "application/x-www-form-urlencoded"
        }
      }
    ).then(res => {
      if (res.status == 200) {
        hooks.confirm()
      } else {
        hooks.recover()
        console.error('failed to select')
      }
    }).catch(_ => {
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
