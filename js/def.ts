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



export type Rational = { numerator: number, denominator: number }


export type TaskCompleted = {
  taskId: number
  htmxResponse: string | null
}

export type DeleteProgressed = {
  taskId: number
  progress: Rational,
  htmxResponse: string | null
}

export type PasteProgressed = {
  taskId: number
  progress: Rational,
  htmxResponse: string | null
}

export type MoveProgressed = {
  taskId: number
  progress: Rational,
  htmxResponse: string | null
}

export type UploadProgressed = {
  taskId: number
  progress: Rational,
  htmxResponse: string | null
}
