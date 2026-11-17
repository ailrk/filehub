export function register(): void {
  document.addEventListener('click', onSortClick);
}

function onSortClick(e: MouseEvent): void {
  const target = (e.target as Element).closest<HTMLElement>('[data-sort-field]');
  if (!target) return;
  const field = target.dataset.sortField!;
  const table = document.getElementById('table');
  if (!table) return;

  const currentSort = table.dataset.sortBy ?? 'nameUp';
  const nextSort = nextSortKey(field, currentSort);

  sortTable(table, field, nextSort.endsWith('Down'));
  table.dataset.sortBy = nextSort;
  updateSortIcons(nextSort);

  fetch(`/table/sort?by=${nextSort}`);
}

function nextSortKey(field: string, current: string): string {
  if (field === 'name')  return current === 'nameUp'     ? 'nameDown'     : 'nameUp';
  if (field === 'mtime') return current === 'modifiedUp' ? 'modifiedDown' : 'modifiedUp';
  if (field === 'size')  return current === 'sizeUp'     ? 'sizeDown'     : 'sizeUp';
  return 'nameUp';
}

function sortTable(table: HTMLElement, field: string, desc: boolean): void {
  const tbody = table.querySelector('tbody');
  if (tbody) {
    const rows = Array.from(tbody.querySelectorAll<HTMLElement>('tr.table-item'));
    rows.sort((a, b) => compare(a, b, field, desc));
    rows.forEach(r => tbody.appendChild(r));
    return;
  }
  const items = Array.from(table.querySelectorAll<HTMLElement>('div.table-item'));
  items.sort((a, b) => compare(a, b, field, desc));
  items.forEach(item => table.appendChild(item));
}

function compare(a: HTMLElement, b: HTMLElement, field: string, desc: boolean): number {
  const aDir = a.classList.contains('dir');
  const bDir = b.classList.contains('dir');
  if (aDir !== bDir) return aDir ? -1 : 1;

  let va: string | number, vb: string | number;
  if (field === 'name') {
    va = (a.dataset.name ?? '').toLowerCase();
    vb = (b.dataset.name ?? '').toLowerCase();
  } else if (field === 'mtime') {
    va = a.dataset.mtime ?? '';
    vb = b.dataset.mtime ?? '';
  } else if (field === 'size') {
    va = parseInt(a.dataset.size ?? '0', 10);
    vb = parseInt(b.dataset.size ?? '0', 10);
  } else {
    return 0;
  }

  const cmp = va < vb ? -1 : va > vb ? 1 : 0;
  return desc ? -cmp : cmp;
}

function updateSortIcons(sort: string): void {
  document.querySelectorAll<HTMLElement>('[data-sort-field]').forEach(el => {
    const field = el.dataset.sortField!;
    const icon = el.querySelector('i');
    if (!icon) return;

    const isActive =
      (field === 'name'  && (sort === 'nameUp'     || sort === 'nameDown'))     ||
      (field === 'mtime' && (sort === 'modifiedUp' || sort === 'modifiedDown')) ||
      (field === 'size'  && (sort === 'sizeUp'     || sort === 'sizeDown'));

    if (!isActive) {
      icon.className = 'bx bx-sort';
    } else {
      icon.className = sort.endsWith('Down') ? 'bx bxs-down-arrow' : 'bx bxs-up-arrow';
    }
  });
}
