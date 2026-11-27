interface JsPluginEntry {
  mime: string;
  label?: string;
  script?: ((params: { filePath: string; fileName: string; mimeType: string }, ui: any) => void) | string;
  url?: string;
}

interface JsPluginDef {
  name: string;
  contextMenu?: JsPluginEntry[];
  viewer?: JsPluginEntry[];
  toolbar?: JsPluginEntry[];
}

interface Window {
  filehub?: {
    _jsPlugins: JsPluginDef[];
    registerPlugin(def: JsPluginDef): void;
  };
}
