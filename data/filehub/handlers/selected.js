let selectedIds = new Set();
const handlers = new Map();
export function register() {
    registerRows();
    document.body.addEventListener('TargetChanged', _ => { selectedIds.clear(); });
    document.body.addEventListener('htmx:afterSettle', _ => {
        collect();
        registerRows();
    });
}
function registerRows() {
    unregisterRows();
    let rows = document.querySelectorAll('#table tr');
    rows.forEach(row => {
        const id = row.dataset.path;
        const h = (e) => handle(row, e);
        handlers.set(id, h);
        row.addEventListener('click', h, true);
    });
}
function unregisterRows() {
    let rows = document.querySelectorAll('#table tr');
    rows.forEach(row => {
        const id = row.dataset.path;
        const h = handlers.get(id);
        h && row.removeEventListener('click', h, true);
    });
}
// Collect selected rows to the set `selectedIds`
function collect() {
    let rows = document.querySelectorAll('#table tr');
    rows.forEach(row => {
        const id = row.dataset.path;
        if (row.classList.contains('selected')) {
            selectedIds.add(id);
        }
    });
}
function handle(row, evt) {
    let e = evt;
    const id = row.dataset.path;
    // disable the default behavior when ctrl is pressed.
    if (e.ctrlKey || e.metaKey) {
        e.preventDefault();
        e.stopImmediatePropagation();
    }
    else
        return;
    function select(hooks) {
        hooks.prepare();
        const values = { selected: Array.from(selectedIds) };
        htmx.ajax('POST', '/table/select', {
            values,
            headers: {
                'Content-Type': 'application/x-www-form-urlencoded'
            },
            target: '#control-panel',
            swap: 'outerHTML'
        }).then((_) => {
            hooks.confirm();
        }).catch((_) => {
            hooks.recover();
            console.error('failed to select');
        });
    }
    if (e.ctrlKey || e.metaKey) {
        if (selectedIds.has(id)) {
            select({
                prepare: () => { selectedIds.delete(id); },
                confirm: () => { row.classList.remove('selected'); },
                recover: () => { selectedIds.add(id); }
            });
        }
        else {
            select({
                prepare: () => { selectedIds.add(id); },
                confirm: () => { row.classList.add('selected'); },
                recover: () => { selectedIds.delete(id); }
            });
        }
    }
}
