/*
 * NodeData — the tagged-union type that flows through node graph cables.
 * All variants are structured-clone serialisable so a worker migration
 * later is mechanical: replace fn(data) with postMessage/onmessage.
 */

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
