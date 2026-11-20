

export type Display = 'Desktop' | 'Mobile' | 'NoDisplay'


export type Rational = { numerator: number, denominator: number }


export type TaskCompleted = {
  taskId: number
  htmxResponse: string | null
}

export type TaskFailed = {
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
