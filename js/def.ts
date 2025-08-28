export type Resource = [url: string, mimetype: string]

export interface ViewerInited {
  resources: Resource[],
  index: number,
}


export interface Opened {
  path: string,
  tgt: OpenedTarget
}


export type OpenedTarget
  = "OpenDOMSelf"
  | "OpenDOMBlank"
  | "OpenDOMParent"
  | "OpenDOMTop"
  | "OpenDOMUnfencedTop"
  | "OpenViewer"



export type Display = 'Desktop' | 'Mobile' | 'NoDisplay'


export type UIComponent = 'UIComponentView' | 'UIComponentSideBar' | 'UIComponentContronPanel' | 'UIComponentIndex'
