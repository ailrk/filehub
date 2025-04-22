const selectedIds: Set<string> = new Set();


export function register() {
  document.querySelectorAll('#table tr').forEach(row => {
    selectedIds.clear()
    row.addEventListener('click', e => handle(row, e), true)
  })
}


// disable the default behavior when ctrl is pressed.
function prevent(evt: Event) {
  let e = evt as MouseEvent;
  if (e.ctrlKey || e.metaKey) {
    e.preventDefault();
    e.stopImmediatePropagation();
  }
}


function handle(row: Element, evt: Event) {
    let e = evt as MouseEvent;
    const id = (row as HTMLElement).dataset.path!;
    prevent(evt)
    if (e.ctrlKey || e.metaKey) {
      if (selectedIds.has(id)) {

        selectedIds.delete(id)
        let payload = new FormData()
        selectedIds.forEach(id => payload.append("selected", id))
        fetch('/table/select',
          { method: 'POST',
            body: payload,
          }
        ).then(_ => {
          row.classList.remove('selected')
        }).catch(_ => {
          selectedIds.add(id)
        })

      } else {
        selectedIds.add(id)
        let payload = new FormData()
        selectedIds.forEach(id => payload.append("selected", id))
        fetch('/table/select',
          { method: 'POST',
            body: payload
          }
        ).then(_ => {
          row.classList.add('selected')
        }).catch(_ => {
          selectedIds.delete(id)
        })
      }
    } else {
      const backup = structuredClone(selectedIds);
      selectedIds.clear()
      selectedIds.add(id)
      let payload = new FormData()
      selectedIds.forEach(id => payload.append("selected", id))
      fetch('/table/select',
        { method: 'POST',
          body: payload
        }
      ).then(_ => {
        // clear all selected rows and select only the current one.
        document.querySelectorAll('#table tr.selected').forEach (r => {
          r.classList.remove('selected')
        });

        row.classList.add("selected")
      }).catch(_ => {
        selectedIds.union(backup)
      })
    }
}
