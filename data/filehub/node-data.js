/*
 * NodeData — the tagged-union type that flows through node graph cables.
 * All variants are structured-clone serialisable so a worker migration
 * later is mechanical: replace fn(data) with postMessage/onmessage.
 */
export {};
