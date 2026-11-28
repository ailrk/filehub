/*
 * Node graph — types, runtime, sockets, cables, and orchestrator.
 */
const nodes = new Map();
const cables = new Map();
let cableCounter = 0;
function registerNode(windowId, hasOutput) {
    nodes.set(windowId, { inputHandlers: new Map(), hasOutput });
}
function unregisterNode(windowId) {
    nodes.delete(windowId);
    for (const [id, c] of cables) {
        if (c.fromWindowId === windowId || c.toWindowId === windowId) {
            cables.delete(id);
        }
    }
}
function registerInputHandler(windowId, socketName, handler) {
    nodes.get(windowId)?.inputHandlers.set(socketName, handler);
}
function connect(fromWindowId, toWindowId, toSocketName) {
    const id = 'cable-' + (++cableCounter);
    cables.set(id, { id, fromWindowId, toWindowId, toSocketName });
    return id;
}
function disconnect(cableId) {
    cables.delete(cableId);
}
function getCablesFrom(windowId) {
    return [...cables.values()].filter(c => c.fromWindowId === windowId);
}
function getCablesTo(windowId) {
    return [...cables.values()].filter(c => c.toWindowId === windowId);
}
function runtimeEmit(fromWindowId, data) {
    for (const c of getCablesFrom(fromWindowId))
        deliver(c, data, new Set([fromWindowId]));
}
function deliver(cable, data, visited) {
    const handler = nodes.get(cable.toWindowId)?.inputHandlers.get(cable.toSocketName);
    if (!handler)
        return;
    Promise.resolve(handler(data)).then(result => {
        if (result === null || visited.has(cable.toWindowId))
            return;
        const nextVisited = new Set(visited);
        nextVisited.add(cable.toWindowId);
        for (const next of getCablesFrom(cable.toWindowId))
            deliver(next, result, nextVisited);
    });
}
// ---------------------------------------------------------------------------
// Sockets
// ---------------------------------------------------------------------------
const TITLEBAR_H = 37;
const SOCKET_D = 14;
const INPUT_GAP = 28;
const socketRegistry = new Map();
function socketRegisterWindow(windowId, winEl, inputs, hasOutput, onMoved) {
    const records = [];
    if (hasOutput) {
        const el = makeSocketEl('output', windowId, '');
        document.body.appendChild(el);
        records.push({ el, windowId, kind: 'output', socketName: '', placed: false });
    }
    inputs.forEach(def => {
        const el = makeSocketEl('input', windowId, def.name);
        document.body.appendChild(el);
        records.push({ el, windowId, kind: 'input', socketName: def.name, placed: false });
    });
    socketRegistry.set(windowId, records);
    placeAll(windowId, winEl);
    const ro = new ResizeObserver(() => { placeAll(windowId, winEl); onMoved(); });
    ro.observe(winEl);
    winEl.addEventListener('filehub:windowmoved', () => { placeAll(windowId, winEl); onMoved(); });
}
function socketUnregisterWindow(windowId) {
    socketRegistry.get(windowId)?.forEach(r => r.el.remove());
    socketRegistry.delete(windowId);
}
function placeAll(windowId, winEl) {
    const recs = socketRegistry.get(windowId);
    if (!recs)
        return;
    if (winEl.getAnimations().some(a => a.playState === 'running'))
        return;
    const rect = winEl.getBoundingClientRect();
    if (rect.width === 0 && rect.height === 0)
        return;
    const zIndex = String((parseInt(winEl.style.zIndex) || 800) - 1);
    let inputIdx = 0;
    for (const r of recs) {
        r.el.style.zIndex = zIndex;
        r.placed = true;
        r.el.style.visibility = '';
        if (r.kind === 'output') {
            const cx = rect.right;
            const cy = rect.top + TITLEBAR_H / 2;
            r.el.style.left = (cx - SOCKET_D / 2) + 'px';
            r.el.style.top = (cy - SOCKET_D / 2) + 'px';
        }
        else {
            const cx = rect.left;
            const cy = rect.top + TITLEBAR_H + inputIdx * INPUT_GAP + INPUT_GAP / 2;
            r.el.style.left = (cx - SOCKET_D / 2) + 'px';
            r.el.style.top = (cy - SOCKET_D / 2) + 'px';
            inputIdx++;
        }
    }
}
function getCenter(windowId, kind, socketName) {
    const recs = socketRegistry.get(windowId);
    if (!recs)
        return null;
    const r = recs.find(r => r.kind === kind && (kind === 'output' ? true : r.socketName === socketName));
    if (!r)
        return null;
    const rect = r.el.getBoundingClientRect();
    return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 };
}
function getAllInputs() {
    const out = [];
    for (const recs of socketRegistry.values()) {
        for (const r of recs) {
            if (r.kind === 'input')
                out.push({ windowId: r.windowId, socketName: r.socketName, el: r.el });
        }
    }
    return out;
}
function socketSetWindowVisible(windowId, visible) {
    socketRegistry.get(windowId)?.forEach(r => {
        r.el.style.visibility = (visible && r.placed) ? '' : 'hidden';
    });
}
function getOutputEl(windowId) {
    return socketRegistry.get(windowId)?.find(r => r.kind === 'output')?.el ?? null;
}
function makeSocketEl(kind, windowId, name) {
    const el = document.createElement('div');
    el.className = `node-socket node-socket-${kind}`;
    el.style.visibility = 'hidden';
    el.dataset.windowId = windowId;
    el.dataset.socketName = name;
    el.dataset.socketKind = kind;
    if (name) {
        const lbl = document.createElement('span');
        lbl.className = 'node-socket-label';
        lbl.textContent = name;
        el.appendChild(lbl);
    }
    return el;
}
let cableSvg;
const cablePaths = new Map();
let drag = null;
function cableInit() {
    cableSvg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    cableSvg.id = 'node-cables-svg';
    Object.assign(cableSvg.style, {
        position: 'fixed', inset: '0', width: '100%', height: '100%',
        overflow: 'visible', zIndex: '750',
    });
    cableSvg.setAttribute('pointer-events', 'none');
    document.body.appendChild(cableSvg);
    document.addEventListener('mousemove', onCableMove);
    document.addEventListener('mouseup', onCableUp);
}
function addCable(cableId, fromWindowId, toWindowId, toSocketName) {
    const path = makeCablePath(false);
    path.addEventListener('click', (e) => {
        e.stopPropagation();
        disconnect(cableId);
        removeCable(cableId);
    });
    cableSvg.appendChild(path);
    cablePaths.set(cableId, { id: cableId, fromWindowId, toWindowId, toSocketName, path });
    redrawCable(cableId);
}
function removeCable(cableId) {
    cablePaths.get(cableId)?.path.remove();
    cablePaths.delete(cableId);
}
function cableSetWindowVisible(windowId, visible) {
    for (const c of cablePaths.values()) {
        if (c.fromWindowId === windowId || c.toWindowId === windowId) {
            c.path.style.visibility = visible ? '' : 'hidden';
        }
    }
}
function redrawForWindow(windowId) {
    for (const [id, c] of cablePaths) {
        if (c.fromWindowId === windowId || c.toWindowId === windowId)
            redrawCable(id);
    }
}
function redrawCable(cableId) {
    const c = cablePaths.get(cableId);
    if (!c)
        return;
    const from = getCenter(c.fromWindowId, 'output', '');
    const to = getCenter(c.toWindowId, 'input', c.toSocketName);
    if (!from || !to)
        return;
    c.path.setAttribute('d', bezier(from.x, from.y, to.x, to.y));
}
function beginDrag(fromWindowId) {
    const ghost = makeCablePath(true);
    cableSvg.appendChild(ghost);
    drag = { fromWindowId, ghost, snap: null };
}
function onCableMove(e) {
    if (!drag)
        return;
    const from = getCenter(drag.fromWindowId, 'output', '');
    if (!from)
        return;
    let tx = e.clientX, ty = e.clientY;
    let snap = null;
    for (const inp of getAllInputs()) {
        if (inp.windowId === drag.fromWindowId)
            continue;
        const c = getCenter(inp.windowId, 'input', inp.socketName);
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
        getAllInputs().forEach(i => i.el.classList.remove('node-socket-snap'));
    drag.snap = snap;
    drag.ghost.setAttribute('d', bezier(from.x, from.y, tx, ty));
}
function onCableUp(_e) {
    if (!drag)
        return;
    drag.ghost.remove();
    if (drag.snap) {
        const cableId = connect(drag.fromWindowId, drag.snap.windowId, drag.snap.socketName);
        addCable(cableId, drag.fromWindowId, drag.snap.windowId, drag.snap.socketName);
    }
    getAllInputs().forEach(i => i.el.classList.remove('node-socket-snap'));
    drag = null;
}
function makeCablePath(isDrag) {
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
// ---------------------------------------------------------------------------
// Plugin def registry
// ---------------------------------------------------------------------------
const defs = new Map();
export function registerDef(pluginName, def) {
    defs.set(pluginName, def);
}
export function getDef(pluginName) {
    return defs.get(pluginName);
}
export function allDefs() {
    return defs;
}
// ---------------------------------------------------------------------------
// Init
// ---------------------------------------------------------------------------
export function init() {
    cableInit();
}
const pending = new Map();
export function preparePorts(pluginName, def) {
    const p = {
        def,
        pendingHandlers: [],
        emitter: { fn: () => { } },
        windowIdRef: { current: null },
    };
    pending.set(pluginName, p);
    const inputs = {
        on(socketName, handler) {
            if (p.windowIdRef.current !== null) {
                registerInputHandler(p.windowIdRef.current, socketName, handler);
            }
            else {
                p.pendingHandlers.push({ name: socketName, handler });
            }
        },
    };
    const output = {
        emit(data) { p.emitter.fn(data); },
    };
    return { inputs, output };
}
export function hasPending(pluginName) {
    return pending.has(pluginName);
}
export function onWindowCreated(windowId, root, pluginName) {
    const p = pending.get(pluginName);
    if (!p)
        return;
    pending.delete(pluginName);
    const { def, pendingHandlers, emitter, windowIdRef } = p;
    const hasOutput = !!def.output;
    registerNode(windowId, hasOutput);
    windowIdRef.current = windowId;
    for (const { name, handler } of pendingHandlers) {
        registerInputHandler(windowId, name, handler);
    }
    socketRegisterWindow(windowId, root, def.inputs ?? [], hasOutput, () => redrawForWindow(windowId));
    if (hasOutput) {
        getOutputEl(windowId)?.addEventListener('mousedown', (e) => {
            e.stopPropagation();
            beginDrag(windowId);
        });
    }
    emitter.fn = (data) => runtimeEmit(windowId, data);
}
export function onWindowVisible(windowId, visible) {
    socketSetWindowVisible(windowId, visible);
    cableSetWindowVisible(windowId, visible);
}
export function onWindowClosed(windowId) {
    for (const c of [...getCablesFrom(windowId), ...getCablesTo(windowId)]) {
        removeCable(c.id);
    }
    unregisterNode(windowId);
    socketUnregisterWindow(windowId);
}
export function getCablesState() {
    return [...cables.values()].map(c => ({
        fromWindowId: c.fromWindowId,
        toWindowId: c.toWindowId,
        toSocketName: c.toSocketName,
    }));
}
export function restoreCable(fromWindowId, toWindowId, toSocketName) {
    const cableId = connect(fromWindowId, toWindowId, toSocketName);
    addCable(cableId, fromWindowId, toWindowId, toSocketName);
}
export function isNodeWindow(windowId) {
    return getCablesFrom(windowId).length > 0 ||
        getCablesTo(windowId).length > 0 ||
        getOutputEl(windowId) !== null;
}
