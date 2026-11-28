interface JsPluginEntry {
  mime: string;
  label?: string;
  script?: ((params: { filePath: string; fileName: string; mimeType: string }, ui: any) => void) | string;
  url?: string;
}

interface JsPluginNodeSocketDef {
  name: string;
  accepts?: string[];
}

interface JsPluginNodeDef {
  inputs?: JsPluginNodeSocketDef[];
  output?: { produces?: string };
  run(inputs: any, output: any, ui: any): void;
}

interface JsPluginDef {
  name: string;
  contextMenu?: JsPluginEntry[];
  viewer?: JsPluginEntry[];
  toolbar?: JsPluginEntry[];
  node?: JsPluginNodeDef;
}

interface Window {
  filehub?: {
    _jsPlugins: JsPluginDef[];
    registerPlugin(def: JsPluginDef): void;
  };
}
