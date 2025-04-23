export interface Resource {
  url: string,
  mimetype: string
}


export interface ViewerInited {
  resources: Resource[],
  index: number,
}
