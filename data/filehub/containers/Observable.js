/* Observable containers */
export function createObservableSet(onChange) {
    const set = new Set();
    return {
        add(value) {
            const result = set.add(value);
            onChange(set);
            return result;
        },
        delete(value) {
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
export function createObservableCell(init, onChange) {
    let cell = init;
    return {
        get() {
            onChange(cell);
            return cell;
        },
        set(value) {
            cell = value;
            onChange(cell);
        },
        __brand: 'ObservableCell'
    };
}
