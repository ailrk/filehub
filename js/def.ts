export interface Resource {
  url: string,
  mimetype: string
}


export interface InitViewer {
  resources: Resource[],
  index: number,
}
