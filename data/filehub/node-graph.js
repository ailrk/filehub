/*
 * Node graph orchestrator — public API consumed by workspace, contextmenu,
 * and main. Wires together node-socket, node-cable, node-runtime.
 */
import * as Cable from './node-cable.js';
import * as Socket from './node-socket.js';
import * as Runtime from './node-runtime.js';
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
    Cable.init();
    Cable.setSockets(Socket.getCenter, Socket.getAllInputs);
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
                // inputs.on() called after ui.window() — register directly
                Runtime.registerInputHandler(p.windowIdRef.current, socketName, handler);
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
// Called by workspace.ts when a window is created for a node plugin
export function onWindowCreated(windowId, root, pluginName) {
    const p = pending.get(pluginName);
    if (!p)
        return;
    pending.delete(pluginName);
    const { def, pendingHandlers, emitter, windowIdRef } = p;
    const hasOutput = !!def.output;
    Runtime.registerNode(windowId, hasOutput);
    // Set windowIdRef first so any inputs.on() calls that come after ui.window()
    // (i.e. the rest of run()) register directly instead of buffering.
    windowIdRef.current = windowId;
    for (const { name, handler } of pendingHandlers) {
        Runtime.registerInputHandler(windowId, name, handler);
    }
    Socket.registerWindow(windowId, root, def.inputs ?? [], hasOutput, () => Cable.redrawForWindow(windowId));
    // Wire output socket mouse-down → begin drag
    if (hasOutput) {
        const outEl = Socket.getOutputEl(windowId);
        outEl?.addEventListener('mousedown', (e) => {
            e.stopPropagation();
            Cable.beginDrag(windowId);
        });
    }
    // Now that we have a windowId, swap in the real emitter
    emitter.fn = (data) => Runtime.emit(windowId, data);
}
// Called by workspace.ts when a workspace switch hides or reveals a window
export function onWindowVisible(windowId, visible) {
    Socket.setWindowVisible(windowId, visible);
    Cable.setWindowVisible(windowId, visible);
}
// Called by workspace.ts when the window closes
export function onWindowClosed(windowId) {
    for (const c of [
        ...Runtime.getConnectionsFrom(windowId),
        ...Runtime.getConnectionsTo(windowId),
    ]) {
        Cable.remove(c.id);
    }
    Runtime.unregisterNode(windowId);
    Socket.unregisterWindow(windowId);
}
export function isNodeWindow(windowId) {
    // A window is a node window if it was registered with the runtime
    return Runtime.getConnectionsFrom(windowId).length > 0 ||
        Runtime.getConnectionsTo(windowId).length > 0 ||
        Socket.getOutputEl(windowId) !== null;
}
