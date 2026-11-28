/*
 * Node runtime — connection registry and async data-flow engine.
 * Completely decoupled from DOM; tested in isolation.
 */

import type { NodeData, InputHandler } from './node-data.js'

interface NodeEntry {
  inputHandlers: Map<string, InputHandler>
  hasOutput: boolean
}

export interface Connection {
  id: string
  fromWindowId: string
  toWindowId: string
  toSocketName: string
}

const nodes       = new Map<string, NodeEntry>()
const connections = new Map<string, Connection>()   // connId → Connection
let   connCounter = 0

// ---------------------------------------------------------------------------
// Node registration
// ---------------------------------------------------------------------------

export function registerNode(windowId: string, hasOutput: boolean): void {
  nodes.set(windowId, { inputHandlers: new Map(), hasOutput })
}

export function unregisterNode(windowId: string): void {
  nodes.delete(windowId)
  for (const [id, c] of connections) {
    if (c.fromWindowId === windowId || c.toWindowId === windowId) {
      connections.delete(id)
    }
  }
}

export function registerInputHandler(
  windowId: string,
  socketName: string,
  handler: InputHandler,
): void {
  nodes.get(windowId)?.inputHandlers.set(socketName, handler)
}

// ---------------------------------------------------------------------------
// Connection management
// ---------------------------------------------------------------------------

export function connect(
  fromWindowId: string,
  toWindowId: string,
  toSocketName: string,
): string {
  const id = 'conn-' + (++connCounter)
  connections.set(id, { id, fromWindowId, toWindowId, toSocketName })
  return id
}

export function disconnect(connId: string): void {
  connections.delete(connId)
}

export function getConnectionsFrom(windowId: string): Connection[] {
  return [...connections.values()].filter(c => c.fromWindowId === windowId)
}

export function getConnectionsTo(windowId: string): Connection[] {
  return [...connections.values()].filter(c => c.toWindowId === windowId)
}

export function getAllConnections(): Connection[] {
  return [...connections.values()]
}

// ---------------------------------------------------------------------------
// Data flow
// ---------------------------------------------------------------------------

export function emit(fromWindowId: string, data: NodeData): void {
  const outConns = getConnectionsFrom(fromWindowId)
  for (const c of outConns) deliver(c, data, new Set([fromWindowId]))
}

function deliver(conn: Connection, data: NodeData, visited: Set<string>): void {
  const node    = nodes.get(conn.toWindowId)
  const handler = node?.inputHandlers.get(conn.toSocketName)
  if (!handler) return

  Promise.resolve(handler(data)).then(result => {
    if (result === null) return
    if (visited.has(conn.toWindowId)) return   // break feedback cycle

    const nextVisited = new Set(visited)
    nextVisited.add(conn.toWindowId)

    for (const next of getConnectionsFrom(conn.toWindowId)) {
      deliver(next, result, nextVisited)
    }
  })
}
