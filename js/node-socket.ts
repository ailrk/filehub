/*
 * Node sockets — the small dots on window edges.
 * Each socket is a fixed-position element on document.body, kept in sync
 * with its window via ResizeObserver + the 'filehub:windowmoved' event.
 * No dependency on node-cable or node-graph to avoid circular imports.
 */

export const TITLEBAR_H  = 37  // px — must match .filehub-window-titlebar height
export const SOCKET_D    = 14  // diameter (matches CSS)
export const INPUT_GAP   = 28  // vertical spacing between input sockets

interface SocketRecord {
  el:         HTMLElement
  windowId:   string
  kind:       'input' | 'output'
  socketName: string
  placed:     boolean   // true once placeAll has run with a valid rect
}

const registry = new Map<string, SocketRecord[]>()   // windowId → sockets

// ---------------------------------------------------------------------------
// Registration
// ---------------------------------------------------------------------------

export function registerWindow(
  windowId: string,
  winEl:    HTMLElement,
  inputs:   { name: string }[],
  hasOutput: boolean,
  onMoved:  () => void,
): void {
  const records: SocketRecord[] = []

  if (hasOutput) {
    const el = makeEl('output', windowId, '')
    document.body.appendChild(el)
    records.push({ el, windowId, kind: 'output', socketName: '', placed: false })
  }

  inputs.forEach((def, i) => {
    const el = makeEl('input', windowId, def.name)
    document.body.appendChild(el)
    records.push({ el, windowId, kind: 'input', socketName: def.name, placed: false })
  })

  registry.set(windowId, records)
  placeAll(windowId, winEl)

  const ro = new ResizeObserver(() => { placeAll(windowId, winEl); onMoved() })
  ro.observe(winEl)

  winEl.addEventListener('filehub:windowmoved', () => { placeAll(windowId, winEl); onMoved() })
}

export function unregisterWindow(windowId: string): void {
  registry.get(windowId)?.forEach(r => r.el.remove())
  registry.delete(windowId)
}

// ---------------------------------------------------------------------------
// Positioning
// ---------------------------------------------------------------------------

function placeAll(windowId: string, winEl: HTMLElement): void {
  const recs = registry.get(windowId)
  if (!recs) return
  // Skip during CSS animations — transform: scale() makes getBoundingClientRect()
  // return the wrong (scaled) rect; animationend will re-trigger us.
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

// ---------------------------------------------------------------------------
// Queries
// ---------------------------------------------------------------------------

export function getCenter(
  windowId:   string,
  kind:       'input' | 'output',
  socketName: string,
): { x: number; y: number } | null {
  const recs = registry.get(windowId)
  if (!recs) return null
  const r = recs.find(r => r.kind === kind &&
    (kind === 'output' ? true : r.socketName === socketName))
  if (!r) return null
  const rect = r.el.getBoundingClientRect()
  return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 }
}

export function getAllInputs(): { windowId: string; socketName: string; el: HTMLElement }[] {
  const out: { windowId: string; socketName: string; el: HTMLElement }[] = []
  for (const recs of registry.values()) {
    for (const r of recs) {
      if (r.kind === 'input') out.push({ windowId: r.windowId, socketName: r.socketName, el: r.el })
    }
  }
  return out
}

export function setWindowVisible(windowId: string, visible: boolean): void {
  registry.get(windowId)?.forEach(r => {
    // Only reveal if placeAll has already run with a valid rect
    r.el.style.visibility = (visible && r.placed) ? '' : 'hidden'
  })
}

export function getOutputEl(windowId: string): HTMLElement | null {
  return registry.get(windowId)?.find(r => r.kind === 'output')?.el ?? null
}

// ---------------------------------------------------------------------------
// DOM helpers
// ---------------------------------------------------------------------------

function makeEl(kind: 'input' | 'output', windowId: string, name: string): HTMLElement {
  const el = document.createElement('div')
  el.className       = `node-socket node-socket-${kind}`
  el.style.visibility = 'hidden'   // revealed by placeAll once window has layout
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
