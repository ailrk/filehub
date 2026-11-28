/*
 * Node graph — types, runtime, ports, cables, and orchestrator.
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
  on(portName: string, handler: InputHandler): void
}

export interface NodeOutputPort {
  emit(data: NodeData): void
}

export interface NodePortDef {
  name: string
  accepts?: NodeDataKind[]
}

export interface NodePluginDef {
  inputs?: NodePortDef[]
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

interface Cable {
  id: string
  fromWindowId: string
  toWindowId: string
  toPortName: string
}

const nodes  = new Map<string, NodeEntry>()
const cables = new Map<string, Cable>()
let   cableCounter = 0

function registerNode(windowId: string, hasOutput: boolean): void {
  nodes.set(windowId, { inputHandlers: new Map(), hasOutput })
}

function unregisterNode(windowId: string): void {
  nodes.delete(windowId)
  for (const [id, c] of cables) {
    if (c.fromWindowId === windowId || c.toWindowId === windowId) {
      cables.delete(id)
    }
  }
}

function registerInputHandler(windowId: string, portName: string, handler: InputHandler): void {
  nodes.get(windowId)?.inputHandlers.set(portName, handler)
}

function connect(fromWindowId: string, toWindowId: string, toPortName: string): string {
  const id = 'cable-' + (++cableCounter)
  cables.set(id, { id, fromWindowId, toWindowId, toPortName })
  return id
}

function disconnect(cableId: string): void {
  cables.delete(cableId)
}

function getCablesFrom(windowId: string): Cable[] {
  return [...cables.values()].filter(c => c.fromWindowId === windowId)
}

function getCablesTo(windowId: string): Cable[] {
  return [...cables.values()].filter(c => c.toWindowId === windowId)
}

function runtimeEmit(fromWindowId: string, data: NodeData): void {
  for (const c of getCablesFrom(fromWindowId)) deliver(c, data, new Set([fromWindowId]))
}

function deliver(cable: Cable, data: NodeData, visited: Set<string>): void {
  const handler = nodes.get(cable.toWindowId)?.inputHandlers.get(cable.toPortName)
  if (!handler) return

  Promise.resolve(handler(data)).then(result => {
    if (result === null || visited.has(cable.toWindowId)) return
    const nextVisited = new Set(visited)
    nextVisited.add(cable.toWindowId)
    for (const next of getCablesFrom(cable.toWindowId)) deliver(next, result, nextVisited)
  })
}

// ---------------------------------------------------------------------------
// Ports
// ---------------------------------------------------------------------------

const TITLEBAR_H = 37
const SOCKET_D   = 14
const INPUT_GAP  = 28

interface PortRecord {
  el:         HTMLElement
  windowId:   string
  kind:       'input' | 'output'
  portName: string
  placed:     boolean
}

const portRegistry = new Map<string, PortRecord[]>()

function portRegisterWindow(
  windowId:  string,
  winEl:     HTMLElement,
  inputs:    { name: string }[],
  hasOutput: boolean,
  onMoved:   () => void,
): void {
  const records: PortRecord[] = []

  if (hasOutput) {
    const el = makePortEl('output', windowId, '')
    document.body.appendChild(el)
    records.push({ el, windowId, kind: 'output', portName: '', placed: false })
  }

  inputs.forEach(def => {
    const el = makePortEl('input', windowId, def.name)
    document.body.appendChild(el)
    records.push({ el, windowId, kind: 'input', portName: def.name, placed: false })
  })

  portRegistry.set(windowId, records)
  placeAll(windowId, winEl)

  const ro = new ResizeObserver(() => { placeAll(windowId, winEl); onMoved() })
  ro.observe(winEl)
  winEl.addEventListener('filehub:windowmoved', () => { placeAll(windowId, winEl); onMoved() })
}

function portUnregisterWindow(windowId: string): void {
  portRegistry.get(windowId)?.forEach(r => r.el.remove())
  portRegistry.delete(windowId)
}

function placeAll(windowId: string, winEl: HTMLElement): void {
  const recs = portRegistry.get(windowId)
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

function getCenter(windowId: string, kind: 'input' | 'output', portName: string): { x: number; y: number } | null {
  const recs = portRegistry.get(windowId)
  if (!recs) return null
  const r = recs.find(r => r.kind === kind && (kind === 'output' ? true : r.portName === portName))
  if (!r) return null
  const rect = r.el.getBoundingClientRect()
  return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 }
}

function getAllInputPorts(): { windowId: string; portName: string; el: HTMLElement }[] {
  const out: { windowId: string; portName: string; el: HTMLElement }[] = []
  for (const recs of portRegistry.values()) {
    for (const r of recs) {
      if (r.kind === 'input') out.push({ windowId: r.windowId, portName: r.portName, el: r.el })
    }
  }
  return out
}

function portSetWindowVisible(windowId: string, visible: boolean): void {
  portRegistry.get(windowId)?.forEach(r => {
    r.el.style.visibility = (visible && r.placed) ? '' : 'hidden'
  })
}

function getOutputEl(windowId: string): HTMLElement | null {
  return portRegistry.get(windowId)?.find(r => r.kind === 'output')?.el ?? null
}

function makePortEl(kind: 'input' | 'output', windowId: string, name: string): HTMLElement {
  const el = document.createElement('div')
  el.className        = `node-port node-port-${kind}`
  el.style.visibility = 'hidden'
  el.dataset.windowId   = windowId
  el.dataset.portName = name
  el.dataset.portKind = kind
  if (name) {
    const lbl = document.createElement('span')
    lbl.className   = 'node-port-label'
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
  toPortName: string
  path:         SVGPathElement
}

let cableSvg: SVGSVGElement
const cablePaths = new Map<string, CableRecord>()

let drag: {
  fromWindowId: string
  ghost:        SVGPathElement
  snap:         { windowId: string; portName: string } | null
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

function addCable(cableId: string, fromWindowId: string, toWindowId: string, toPortName: string): void {
  const path = makeCablePath(false)
  path.addEventListener('click', (e) => {
    e.stopPropagation()
    disconnect(cableId)
    removeCable(cableId)
  })
  cableSvg.appendChild(path)
  cablePaths.set(cableId, { id: cableId, fromWindowId, toWindowId, toPortName, path })
  redrawCable(cableId)
}

function removeCable(cableId: string): void {
  cablePaths.get(cableId)?.path.remove()
  cablePaths.delete(cableId)
}

function cableSetWindowVisible(windowId: string, visible: boolean): void {
  for (const c of cablePaths.values()) {
    if (c.fromWindowId === windowId || c.toWindowId === windowId) {
      c.path.style.visibility = visible ? '' : 'hidden'
    }
  }
}

function redrawForWindow(windowId: string): void {
  for (const [id, c] of cablePaths) {
    if (c.fromWindowId === windowId || c.toWindowId === windowId) redrawCable(id)
  }
}

function redrawCable(cableId: string): void {
  const c = cablePaths.get(cableId)
  if (!c) return
  const from = getCenter(c.fromWindowId, 'output', '')
  const to   = getCenter(c.toWindowId,   'input',  c.toPortName)
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

  for (const inp of getAllInputPorts()) {
    if (inp.windowId === drag.fromWindowId) continue
    const c = getCenter(inp.windowId, 'input', inp.portName)
    if (!c) continue
    if (Math.hypot(e.clientX - c.x, e.clientY - c.y) < 28) {
      tx = c.x; ty = c.y
      snap = { windowId: inp.windowId, portName: inp.portName }
      inp.el.classList.add('node-port-snap')
    } else {
      inp.el.classList.remove('node-port-snap')
    }
  }

  if (!snap) getAllInputPorts().forEach(i => i.el.classList.remove('node-socket-snap'))
  drag.snap = snap
  drag.ghost.setAttribute('d', bezier(from.x, from.y, tx, ty))
}

function onCableUp(_e: MouseEvent): void {
  if (!drag) return
  drag.ghost.remove()

  if (drag.snap) {
    const cableId = connect(drag.fromWindowId, drag.snap.windowId, drag.snap.portName)
    addCable(cableId, drag.fromWindowId, drag.snap.windowId, drag.snap.portName)
  }

  getAllInputPorts().forEach(i => i.el.classList.remove('node-port-snap'))
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
    on(portName, handler) {
      if (p.windowIdRef.current !== null) {
        registerInputHandler(p.windowIdRef.current, portName, handler)
      } else {
        p.pendingHandlers.push({ name: portName, handler })
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

  portRegisterWindow(
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
  portSetWindowVisible(windowId, visible)
  cableSetWindowVisible(windowId, visible)
}

export function onWindowClosed(windowId: string): void {
  for (const c of [...getCablesFrom(windowId), ...getCablesTo(windowId)]) {
    removeCable(c.id)
  }
  unregisterNode(windowId)
  portUnregisterWindow(windowId)
}

export function getCablesState(): Array<{ fromWindowId: string; toWindowId: string; toPortName: string }> {
  return [...cables.values()].map(c => ({
    fromWindowId: c.fromWindowId,
    toWindowId:   c.toWindowId,
    toPortName:   c.toPortName,
  }));
}

export function restoreCable(fromWindowId: string, toWindowId: string, toPortName: string): void {
  const cableId = connect(fromWindowId, toWindowId, toPortName);
  addCable(cableId, fromWindowId, toWindowId, toPortName);
}

export function isNodeWindow(windowId: string): boolean {
  return getCablesFrom(windowId).length > 0 ||
         getCablesTo(windowId).length   > 0 ||
         getOutputEl(windowId) !== null
}
