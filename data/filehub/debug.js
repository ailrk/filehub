/* Simple debug utility to output log as html */
export function init() {
    // Reference to an output container, use 'pre' styling for JSON output
    let output = document.createElement('pre');
    output.style = 'pointer-events: none;';
    output.classList.add('debug');
    document.body.appendChild(output);
    // Reference to native method(s)
    let oldLog = console.log;
    console.log = function (...items) {
        // Call native method first
        oldLog.apply(this, items);
        // Use JSON to transform objects, all others display normally
        items.forEach((item, i) => {
            items[i] = (typeof item === 'object' ? JSON.stringify(item, null, 4) : item);
        });
        output.innerHTML = items.join(' ') + '<br />' + output.innerHTML;
    };
}
