export function register() {
    document.body.addEventListener('htmx:afterSettle', _ => {
        console.log('load drag again!');
        register1();
    });
}
function register1() {
    console.log('load drag');
    let items = document.querySelectorAll('.table-item');
    let dirs = document.querySelectorAll('.dir');
    for (let i = 0; i < items.length; ++i) {
        let item = items[i];
        item.addEventListener('dragstart', e => handleDrag(e));
    }
    for (let i = 0; i < dirs.length; ++i) {
        let dir = dirs[i];
        dir.addEventListener('drop', e => handleDrop(e));
        dir.addEventListener('dragover', e => handleDragOver(e));
    }
}
function handleDragOver(e) {
    e.preventDefault();
}
function handleDrag(e) {
    console.log("dragged!");
    if (e.target instanceof HTMLElement) {
        let path = e.target.dataset.path;
        if (path === undefined) {
            return;
        }
        e.dataTransfer.clearData();
        e.dataTransfer.setData('text', path);
        e.dataTransfer.dropEffect = 'move';
    }
}
function handleDrop(e) {
    e.preventDefault();
    if (e.target instanceof HTMLElement) {
        let tgt = e.target.dataset.path;
        if (tgt === undefined) {
            return;
        }
        const src = e.dataTransfer.getData('text');
        const values = { src, tgt };
        htmx.ajax('POST', '/files/move', {
            values,
            headers: {
                'Content-Type': 'application/x-www-form-urlencoded'
            },
            target: '#index',
            swap: 'outerHTML'
        });
        console.log('dropped! - ', tgt, src);
    }
}
