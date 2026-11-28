/*
 * Node runtime — connection registry and async data-flow engine.
 * Completely decoupled from DOM; tested in isolation.
 */
const nodes = new Map();
const connections = new Map(); // connId → Connection
let connCounter = 0;
// ---------------------------------------------------------------------------
// Node registration
// ---------------------------------------------------------------------------
export function registerNode(windowId, hasOutput) {
    nodes.set(windowId, { inputHandlers: new Map(), hasOutput });
}
export function unregisterNode(windowId) {
    nodes.delete(windowId);
    for (const [id, c] of connections) {
        if (c.fromWindowId === windowId || c.toWindowId === windowId) {
            connections.delete(id);
        }
    }
}
export function registerInputHandler(windowId, socketName, handler) {
    nodes.get(windowId)?.inputHandlers.set(socketName, handler);
}
// ---------------------------------------------------------------------------
// Connection management
// ---------------------------------------------------------------------------
export function connect(fromWindowId, toWindowId, toSocketName) {
    const id = 'conn-' + (++connCounter);
    connections.set(id, { id, fromWindowId, toWindowId, toSocketName });
    return id;
}
export function disconnect(connId) {
    connections.delete(connId);
}
export function getConnectionsFrom(windowId) {
    return [...connections.values()].filter(c => c.fromWindowId === windowId);
}
export function getConnectionsTo(windowId) {
    return [...connections.values()].filter(c => c.toWindowId === windowId);
}
export function getAllConnections() {
    return [...connections.values()];
}
// ---------------------------------------------------------------------------
// Data flow
// ---------------------------------------------------------------------------
export function emit(fromWindowId, data) {
    const outConns = getConnectionsFrom(fromWindowId);
    for (const c of outConns)
        deliver(c, data, new Set([fromWindowId]));
}
function deliver(conn, data, visited) {
    const node = nodes.get(conn.toWindowId);
    const handler = node?.inputHandlers.get(conn.toSocketName);
    if (!handler)
        return;
    Promise.resolve(handler(data)).then(result => {
        if (result === null)
            return;
        if (visited.has(conn.toWindowId))
            return; // break feedback cycle
        const nextVisited = new Set(visited);
        nextVisited.add(conn.toWindowId);
        for (const next of getConnectionsFrom(conn.toWindowId)) {
            deliver(next, result, nextVisited);
        }
    });
}
