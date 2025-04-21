const selectedIds: Set<string> = new Set();


export function register() {
  document.querySelectorAll('#table tr').forEach(row => {
    row.addEventListener('click', e => handle(row, e))
  }, true)
}


// disable the default behavior when ctrl is pressed.
function prevent(evt: Event) {
  let e = evt as MouseEvent;
  if (e.ctrlKey || e.metaKey) {
    e.preventDefault();
  }
}


function handle(row: Element, evt: Event) {
    let e = evt as MouseEvent;
    const id = (row as HTMLElement).dataset.path!;
    prevent(evt)
    if (e.ctrlKey || e.metaKey) {
      if (selectedIds.has(id)) {
        selectedIds.delete(id)
        row.classList.remove('selected')
      } else {
        selectedIds.add(id)
        row.classList.add('selected')
      }
    } else {
      // clear all selected rows and select only the current one.
      document.querySelectorAll('#table tr.selected').forEach (r => {
        r.classList.remove('selected')
      });
      selectedIds.clear()
      selectedIds.add(id)
      row.classList.add("selected")
    }
}
