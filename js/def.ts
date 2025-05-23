export type Resource = [url: string, mimetype: string]

export interface ViewerInited {
  resources: Resource[],
  index: number,
}


export type Display = 'Desktop' | 'Mobile' | 'NoDisplay'
