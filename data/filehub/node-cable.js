/*
 * Node cables — SVG bezier connections between sockets.
 * Uses callback injection from node-graph.ts to avoid circular imports
 * with node-socket.ts.
 */
import * as Runtime from './node-runtime.js';
let _getCenter = () => null;
let _getInputs = () => [];
export function setSockets(gc, gi) {
    _getCenter = gc;
    _getInputs = gi;
}
// ---------------------------------------------------------------------------
// SVG layer
// ---------------------------------------------------------------------------
let svg;
const cables = new Map();
// Drag state
let drag = null;
// ---------------------------------------------------------------------------
// Init
// ---------------------------------------------------------------------------
export function init() {
    svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    svg.id = 'node-cables-svg';
    Object.assign(svg.style, {
        position: 'fixed', inset: '0', width: '100%', height: '100%',
        overflow: 'visible', zIndex: '750',
    });
    // Use SVG presentation attribute (not CSS) so child paths can override
    svg.setAttribute('pointer-events', 'none');
    document.body.appendChild(svg);
    document.addEventListener('mousemove', onMove);
    document.addEventListener('mouseup', onUp);
}
// ---------------------------------------------------------------------------
// Cable CRUD
// ---------------------------------------------------------------------------
export function add(connId, fromWindowId, toWindowId, toSocketName) {
    const path = makePath(false);
    path.addEventListener('click', (e) => {
        e.stopPropagation();
        Runtime.disconnect(connId);
        remove(connId);
    });
    svg.appendChild(path);
    cables.set(connId, { id: connId, fromWindowId, toWindowId, toSocketName, path });
    redrawCable(connId);
}
export function remove(connId) {
    cables.get(connId)?.path.remove();
    cables.delete(connId);
}
export function setWindowVisible(windowId, visible) {
    for (const c of cables.values()) {
        if (c.fromWindowId === windowId || c.toWindowId === windowId) {
            c.path.style.visibility = visible ? '' : 'hidden';
        }
    }
}
export function redrawForWindow(windowId) {
    for (const [id, c] of cables) {
        if (c.fromWindowId === windowId || c.toWindowId === windowId)
            redrawCable(id);
    }
}
function redrawCable(connId) {
    const c = cables.get(connId);
    if (!c)
        return;
    const from = _getCenter(c.fromWindowId, 'output', '');
    const to = _getCenter(c.toWindowId, 'input', c.toSocketName);
    if (!from || !to)
        return;
    c.path.setAttribute('d', bezier(from.x, from.y, to.x, to.y));
}
// ---------------------------------------------------------------------------
// Drag-to-connect
// ---------------------------------------------------------------------------
export function beginDrag(fromWindowId) {
    const ghost = makePath(true);
    svg.appendChild(ghost);
    drag = { fromWindowId, ghost, snap: null };
}
function onMove(e) {
    if (!drag)
        return;
    const from = _getCenter(drag.fromWindowId, 'output', '');
    if (!from)
        return;
    let tx = e.clientX, ty = e.clientY;
    let snap = null;
    for (const inp of _getInputs()) {
        if (inp.windowId === drag.fromWindowId)
            continue;
        const c = _getCenter(inp.windowId, 'input', inp.socketName);
        if (!c)
            continue;
        if (Math.hypot(e.clientX - c.x, e.clientY - c.y) < 28) {
            tx = c.x;
            ty = c.y;
            snap = { windowId: inp.windowId, socketName: inp.socketName };
            inp.el.classList.add('node-socket-snap');
        }
        else {
            inp.el.classList.remove('node-socket-snap');
        }
    }
    if (!snap)
        _getInputs().forEach(i => i.el.classList.remove('node-socket-snap'));
    drag.snap = snap;
    drag.ghost.setAttribute('d', bezier(from.x, from.y, tx, ty));
}
function onUp(_e) {
    if (!drag)
        return;
    drag.ghost.remove();
    if (drag.snap) {
        const connId = Runtime.connect(drag.fromWindowId, drag.snap.windowId, drag.snap.socketName);
        add(connId, drag.fromWindowId, drag.snap.windowId, drag.snap.socketName);
    }
    _getInputs().forEach(i => i.el.classList.remove('node-socket-snap'));
    drag = null;
}
// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------
function makePath(isDrag) {
    const p = document.createElementNS('http://www.w3.org/2000/svg', 'path');
    p.setAttribute('class', isDrag ? 'node-cable node-cable-drag' : 'node-cable');
    p.setAttribute('pointer-events', isDrag ? 'none' : 'stroke');
    p.style.cursor = isDrag ? 'default' : 'pointer';
    return p;
}
function bezier(x1, y1, x2, y2) {
    const dx = Math.max(Math.abs(x2 - x1) * 0.5, 60);
    return `M ${x1} ${y1} C ${x1 + dx} ${y1}, ${x2 - dx} ${y2}, ${x2} ${y2}`;
}
