/* Observable containers */

export type ObservableSet<T> = {
  add(value: T): Set<T>;
  delete(value: T): boolean;
  clear(): void;
  has(value: T): boolean;
  forEach(callback: (value: T, value2: T, set: Set<T>) => void): void;
  size(): number;
  values(): IterableIterator<T>;
  raw: Set<T>;
} & { __brand: 'ObservableSet' };


export function createObservableSet<T>(onChange: (self: Set<T>) => void): ObservableSet<T> {
  const set = new Set<T>();
  return {
    add(value: T) {
      const result = set.add(value);
      onChange(set);
      return result;
    },
    delete(value: T) {
      const result = set.delete(value);
      onChange(set);
      return result;
    },
    clear() {
      set.clear();
      onChange(set);
    },
    has: set.has.bind(set),
    forEach: set.forEach.bind(set),
    size: () => set.size,
    values: set.values.bind(set),
    raw: set,
    __brand: 'ObservableSet',
  };
}


export type ObservableCell<T> = {
  get(): T;
  set(value: T): void;
} & { __brand: 'ObservableCell' };


export function createObservableCell<T>(init: T, onChange: (self: T) => void): ObservableCell<T> {
  let cell = init;
  return {
    get()  {
      onChange(cell)
      return cell
    },
    set(value: T) {
      cell = value
      onChange(cell)
    },
    __brand: 'ObservableCell'
  };
}
