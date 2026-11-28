/*
 * Node graph — types, runtime, sockets, cables, and orchestrator.
 */

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

export interface NodeFileData   { kind: 'file';   value: { path: string; name: string; mime: string } }
export interface NodeTextData   { kind: 'text';   value: string }
export interface NodeBufferData { kind: 'buffer'; value: ArrayBuffer }
export interface NodeStreamData { kind: 'stream'; value: ReadableStream<Uint8Array> }
export interface NodeJsonData   { kind: 'json';   value: unknown }
export interface NodeSignalData { kind: 'signal'; value?: unknown }

export type NodeData =
  | NodeFileData
  | NodeTextData
  | NodeBufferData
  | NodeStreamData
  | NodeJsonData
  | NodeSignalData

export type NodeDataKind = NodeData['kind']

export type InputHandler = (data: NodeData) => Promise<NodeData | null>

export interface NodeInputPort {
  on(socketName: string, handler: InputHandler): void
}

export interface NodeOutputPort {
  emit(data: NodeData): void
}

export interface NodeSocketDef {
  name: string
  accepts?: NodeDataKind[]
}

export interface NodePluginDef {
  inputs?: NodeSocketDef[]
  output?: { produces?: NodeDataKind }
  run(inputs: NodeInputPort, output: NodeOutputPort, ui: any): void
}

// ---------------------------------------------------------------------------
// Runtime
// ---------------------------------------------------------------------------

interface NodeEntry {
  inputHandlers: Map<string, InputHandler>
  hasOutput: boolean
}

interface Connection {
  id: string
  fromWindowId: string
  toWindowId: string
  toSocketName: string
}

const nodes       = new Map<string, NodeEntry>()
const connections = new Map<string, Connection>()
let   connCounter = 0

function registerNode(windowId: string, hasOutput: boolean): void {
  nodes.set(windowId, { inputHandlers: new Map(), hasOutput })
}

function unregisterNode(windowId: string): void {
  nodes.delete(windowId)
  for (const [id, c] of connections) {
    if (c.fromWindowId === windowId || c.toWindowId === windowId) {
      connections.delete(id)
    }
  }
}

function registerInputHandler(windowId: string, socketName: string, handler: InputHandler): void {
  nodes.get(windowId)?.inputHandlers.set(socketName, handler)
}

function connect(fromWindowId: string, toWindowId: string, toSocketName: string): string {
  const id = 'conn-' + (++connCounter)
  connections.set(id, { id, fromWindowId, toWindowId, toSocketName })
  return id
}

function disconnect(connId: string): void {
  connections.delete(connId)
}

function getConnectionsFrom(windowId: string): Connection[] {
  return [...connections.values()].filter(c => c.fromWindowId === windowId)
}

function getConnectionsTo(windowId: string): Connection[] {
  return [...connections.values()].filter(c => c.toWindowId === windowId)
}

function runtimeEmit(fromWindowId: string, data: NodeData): void {
  for (const c of getConnectionsFrom(fromWindowId)) deliver(c, data, new Set([fromWindowId]))
}

function deliver(conn: Connection, data: NodeData, visited: Set<string>): void {
  const handler = nodes.get(conn.toWindowId)?.inputHandlers.get(conn.toSocketName)
  if (!handler) return

  Promise.resolve(handler(data)).then(result => {
    if (result === null || visited.has(conn.toWindowId)) return
    const nextVisited = new Set(visited)
    nextVisited.add(conn.toWindowId)
    for (const next of getConnectionsFrom(conn.toWindowId)) deliver(next, result, nextVisited)
  })
}

// ---------------------------------------------------------------------------
// Sockets
// ---------------------------------------------------------------------------

const TITLEBAR_H = 37
const SOCKET_D   = 14
const INPUT_GAP  = 28

interface SocketRecord {
  el:         HTMLElement
  windowId:   string
  kind:       'input' | 'output'
  socketName: string
  placed:     boolean
}

const socketRegistry = new Map<string, SocketRecord[]>()

function socketRegisterWindow(
  windowId:  string,
  winEl:     HTMLElement,
  inputs:    { name: string }[],
  hasOutput: boolean,
  onMoved:   () => void,
): void {
  const records: SocketRecord[] = []

  if (hasOutput) {
    const el = makeSocketEl('output', windowId, '')
    document.body.appendChild(el)
    records.push({ el, windowId, kind: 'output', socketName: '', placed: false })
  }

  inputs.forEach(def => {
    const el = makeSocketEl('input', windowId, def.name)
    document.body.appendChild(el)
    records.push({ el, windowId, kind: 'input', socketName: def.name, placed: false })
  })

  socketRegistry.set(windowId, records)
  placeAll(windowId, winEl)

  const ro = new ResizeObserver(() => { placeAll(windowId, winEl); onMoved() })
  ro.observe(winEl)
  winEl.addEventListener('filehub:windowmoved', () => { placeAll(windowId, winEl); onMoved() })
}

function socketUnregisterWindow(windowId: string): void {
  socketRegistry.get(windowId)?.forEach(r => r.el.remove())
  socketRegistry.delete(windowId)
}

function placeAll(windowId: string, winEl: HTMLElement): void {
  const recs = socketRegistry.get(windowId)
  if (!recs) return
  if (winEl.getAnimations().some(a => a.playState === 'running')) return
  const rect = winEl.getBoundingClientRect()
  if (rect.width === 0 && rect.height === 0) return
  const zIndex = String((parseInt(winEl.style.zIndex) || 800) - 1)
  let inputIdx = 0
  for (const r of recs) {
    r.el.style.zIndex = zIndex
    r.placed = true
    r.el.style.visibility = ''
    if (r.kind === 'output') {
      const cx = rect.right
      const cy = rect.top + TITLEBAR_H / 2
      r.el.style.left = (cx - SOCKET_D / 2) + 'px'
      r.el.style.top  = (cy - SOCKET_D / 2) + 'px'
    } else {
      const cx = rect.left
      const cy = rect.top + TITLEBAR_H + inputIdx * INPUT_GAP + INPUT_GAP / 2
      r.el.style.left = (cx - SOCKET_D / 2) + 'px'
      r.el.style.top  = (cy - SOCKET_D / 2) + 'px'
      inputIdx++
    }
  }
}

function getCenter(windowId: string, kind: 'input' | 'output', socketName: string): { x: number; y: number } | null {
  const recs = socketRegistry.get(windowId)
  if (!recs) return null
  const r = recs.find(r => r.kind === kind && (kind === 'output' ? true : r.socketName === socketName))
  if (!r) return null
  const rect = r.el.getBoundingClientRect()
  return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 }
}

function getAllInputs(): { windowId: string; socketName: string; el: HTMLElement }[] {
  const out: { windowId: string; socketName: string; el: HTMLElement }[] = []
  for (const recs of socketRegistry.values()) {
    for (const r of recs) {
      if (r.kind === 'input') out.push({ windowId: r.windowId, socketName: r.socketName, el: r.el })
    }
  }
  return out
}

function socketSetWindowVisible(windowId: string, visible: boolean): void {
  socketRegistry.get(windowId)?.forEach(r => {
    r.el.style.visibility = (visible && r.placed) ? '' : 'hidden'
  })
}

function getOutputEl(windowId: string): HTMLElement | null {
  return socketRegistry.get(windowId)?.find(r => r.kind === 'output')?.el ?? null
}

function makeSocketEl(kind: 'input' | 'output', windowId: string, name: string): HTMLElement {
  const el = document.createElement('div')
  el.className        = `node-socket node-socket-${kind}`
  el.style.visibility = 'hidden'
  el.dataset.windowId   = windowId
  el.dataset.socketName = name
  el.dataset.socketKind = kind
  if (name) {
    const lbl = document.createElement('span')
    lbl.className   = 'node-socket-label'
    lbl.textContent = name
    el.appendChild(lbl)
  }
  return el
}

// ---------------------------------------------------------------------------
// Cables
// ---------------------------------------------------------------------------

interface CableRecord {
  id:           string
  fromWindowId: string
  toWindowId:   string
  toSocketName: string
  path:         SVGPathElement
}

let cableSvg: SVGSVGElement
const cables = new Map<string, CableRecord>()

let drag: {
  fromWindowId: string
  ghost:        SVGPathElement
  snap:         { windowId: string; socketName: string } | null
} | null = null

function cableInit(): void {
  cableSvg = document.createElementNS('http://www.w3.org/2000/svg', 'svg')
  cableSvg.id = 'node-cables-svg'
  Object.assign(cableSvg.style, {
    position: 'fixed', inset: '0', width: '100%', height: '100%',
    overflow: 'visible', zIndex: '750',
  })
  cableSvg.setAttribute('pointer-events', 'none')
  document.body.appendChild(cableSvg)

  document.addEventListener('mousemove', onCableMove)
  document.addEventListener('mouseup',   onCableUp)
}

function addCable(connId: string, fromWindowId: string, toWindowId: string, toSocketName: string): void {
  const path = makeCablePath(false)
  path.addEventListener('click', (e) => {
    e.stopPropagation()
    disconnect(connId)
    removeCable(connId)
  })
  cableSvg.appendChild(path)
  cables.set(connId, { id: connId, fromWindowId, toWindowId, toSocketName, path })
  redrawCable(connId)
}

function removeCable(connId: string): void {
  cables.get(connId)?.path.remove()
  cables.delete(connId)
}

function cableSetWindowVisible(windowId: string, visible: boolean): void {
  for (const c of cables.values()) {
    if (c.fromWindowId === windowId || c.toWindowId === windowId) {
      c.path.style.visibility = visible ? '' : 'hidden'
    }
  }
}

function redrawForWindow(windowId: string): void {
  for (const [id, c] of cables) {
    if (c.fromWindowId === windowId || c.toWindowId === windowId) redrawCable(id)
  }
}

function redrawCable(connId: string): void {
  const c = cables.get(connId)
  if (!c) return
  const from = getCenter(c.fromWindowId, 'output', '')
  const to   = getCenter(c.toWindowId,   'input',  c.toSocketName)
  if (!from || !to) return
  c.path.setAttribute('d', bezier(from.x, from.y, to.x, to.y))
}

function beginDrag(fromWindowId: string): void {
  const ghost = makeCablePath(true)
  cableSvg.appendChild(ghost)
  drag = { fromWindowId, ghost, snap: null }
}

function onCableMove(e: MouseEvent): void {
  if (!drag) return
  const from = getCenter(drag.fromWindowId, 'output', '')
  if (!from) return

  let tx = e.clientX, ty = e.clientY
  let snap: typeof drag.snap = null

  for (const inp of getAllInputs()) {
    if (inp.windowId === drag.fromWindowId) continue
    const c = getCenter(inp.windowId, 'input', inp.socketName)
    if (!c) continue
    if (Math.hypot(e.clientX - c.x, e.clientY - c.y) < 28) {
      tx = c.x; ty = c.y
      snap = { windowId: inp.windowId, socketName: inp.socketName }
      inp.el.classList.add('node-socket-snap')
    } else {
      inp.el.classList.remove('node-socket-snap')
    }
  }

  if (!snap) getAllInputs().forEach(i => i.el.classList.remove('node-socket-snap'))
  drag.snap = snap
  drag.ghost.setAttribute('d', bezier(from.x, from.y, tx, ty))
}

function onCableUp(_e: MouseEvent): void {
  if (!drag) return
  drag.ghost.remove()

  if (drag.snap) {
    const connId = connect(drag.fromWindowId, drag.snap.windowId, drag.snap.socketName)
    addCable(connId, drag.fromWindowId, drag.snap.windowId, drag.snap.socketName)
  }

  getAllInputs().forEach(i => i.el.classList.remove('node-socket-snap'))
  drag = null
}

function makeCablePath(isDrag: boolean): SVGPathElement {
  const p = document.createElementNS('http://www.w3.org/2000/svg', 'path')
  p.setAttribute('class', isDrag ? 'node-cable node-cable-drag' : 'node-cable')
  p.setAttribute('pointer-events', isDrag ? 'none' : 'stroke')
  p.style.cursor = isDrag ? 'default' : 'pointer'
  return p
}

function bezier(x1: number, y1: number, x2: number, y2: number): string {
  const dx = Math.max(Math.abs(x2 - x1) * 0.5, 60)
  return `M ${x1} ${y1} C ${x1+dx} ${y1}, ${x2-dx} ${y2}, ${x2} ${y2}`
}

// ---------------------------------------------------------------------------
// Plugin def registry
// ---------------------------------------------------------------------------

const defs = new Map<string, NodePluginDef>()

export function registerDef(pluginName: string, def: NodePluginDef): void {
  defs.set(pluginName, def)
}

export function getDef(pluginName: string): NodePluginDef | undefined {
  return defs.get(pluginName)
}

export function allDefs(): Map<string, NodePluginDef> {
  return defs
}

// ---------------------------------------------------------------------------
// Init
// ---------------------------------------------------------------------------

export function init(): void {
  cableInit()
}

// ---------------------------------------------------------------------------
// Pending — bridges the gap between preparePorts() and onWindowCreated()
// A node plugin calls preparePorts(), then immediately calls def.run() which
// internally calls ui.window(), which triggers onWindowCreated() — all sync.
// ---------------------------------------------------------------------------

interface Pending {
  def:             NodePluginDef
  pendingHandlers: { name: string; handler: InputHandler }[]
  emitter:         { fn: (d: NodeData) => void }
  windowIdRef:     { current: string | null }
}

const pending = new Map<string, Pending>()

export function preparePorts(pluginName: string, def: NodePluginDef): {
  inputs: NodeInputPort
  output: NodeOutputPort
} {
  const p: Pending = {
    def,
    pendingHandlers: [],
    emitter: { fn: () => {} },
    windowIdRef: { current: null },
  }
  pending.set(pluginName, p)

  const inputs: NodeInputPort = {
    on(socketName, handler) {
      if (p.windowIdRef.current !== null) {
        registerInputHandler(p.windowIdRef.current, socketName, handler)
      } else {
        p.pendingHandlers.push({ name: socketName, handler })
      }
    },
  }
  const output: NodeOutputPort = {
    emit(data) { p.emitter.fn(data) },
  }
  return { inputs, output }
}

export function hasPending(pluginName: string): boolean {
  return pending.has(pluginName)
}

export function onWindowCreated(windowId: string, root: HTMLElement, pluginName: string): void {
  const p = pending.get(pluginName)
  if (!p) return
  pending.delete(pluginName)

  const { def, pendingHandlers, emitter, windowIdRef } = p
  const hasOutput = !!def.output

  registerNode(windowId, hasOutput)
  windowIdRef.current = windowId

  for (const { name, handler } of pendingHandlers) {
    registerInputHandler(windowId, name, handler)
  }

  socketRegisterWindow(
    windowId,
    root,
    def.inputs ?? [],
    hasOutput,
    () => redrawForWindow(windowId),
  )

  if (hasOutput) {
    getOutputEl(windowId)?.addEventListener('mousedown', (e) => {
      e.stopPropagation()
      beginDrag(windowId)
    })
  }

  emitter.fn = (data) => runtimeEmit(windowId, data)
}

export function onWindowVisible(windowId: string, visible: boolean): void {
  socketSetWindowVisible(windowId, visible)
  cableSetWindowVisible(windowId, visible)
}

export function onWindowClosed(windowId: string): void {
  for (const c of [...getConnectionsFrom(windowId), ...getConnectionsTo(windowId)]) {
    removeCable(c.id)
  }
  unregisterNode(windowId)
  socketUnregisterWindow(windowId)
}

export function isNodeWindow(windowId: string): boolean {
  return getConnectionsFrom(windowId).length > 0 ||
         getConnectionsTo(windowId).length  > 0 ||
         getOutputEl(windowId) !== null
}
