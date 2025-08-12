export function register() {
  console.log('load drag')
  let items = document.querySelectorAll('.table-item')
  let dirs = document.querySelectorAll('.dir')
  for (let i = 0; i < items.length; ++i) {
    let item = items[i];
    item.addEventListener('dragstart', e => handleDrag(e as DragEvent));
  }

  for (let i = 0; i < dirs.length; ++i) {
    let dir = dirs[i];
    dir.addEventListener('dragover', e => {
      console.log('dragged over')
      e.preventDefault()
    })
    dir.addEventListener('drop', e => handleDrop(e as DragEvent))
  }
}


function handleDrag(e: DragEvent) {
  e.dataTransfer!.clearData()
  e.dataTransfer!.setData('text', 'hello')
  e.dataTransfer!.dropEffect = 'move';
  console.log("dragged!")
}

function handleDrop(e: DragEvent) {
  console.log('dropping')
  e.preventDefault();
  const data = e.dataTransfer!.getData('text')
  console.log('dropped!', data)
}
