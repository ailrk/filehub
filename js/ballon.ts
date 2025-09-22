export type MessageKind
  = "ErrorMsg"
  | "ProgressMsg"


export interface ErrorMessage {
  kind: "ErrorMsg";
  msg: string;
  duration: number;
}


export interface ProgressMessage {
  kind: "ProgressMsg";
  taskId: number;
  msg: string;
  progress: [number, number];
}


export type Message = ErrorMessage | ProgressMessage


const longLivedBallons: Map<number, HTMLElement> = new Map();


export function pushBalloon(message: Message) {
  switch (message.kind) {
    case "ErrorMsg":
      pushErrorMsgBalloon(message.msg, message.duration);
      break;
    case "ProgressMsg":
      let { msg, taskId, progress } = message;
      pushProgressBarBalloon(msg, taskId, progress)
      break;
  }
 }


function pushErrorMsgBalloon(message: string, duration = 3000) {
  const container = document.getElementById('balloon-container')!
  const balloon = document.createElement('div')
  balloon.classList.add('balloon')
  balloon.classList.add('balloon-err-msg')
  balloon.textContent = message
  container.appendChild(balloon)
  setTimeout(() => {
    balloon.style.opacity = '0'
    balloon.style.transition = 'opacity 0.5s'
    setTimeout(() => balloon.remove(), 500)
  }, duration)
}


function pushProgressBarBalloon(message: string, taskId: number, progress: [number, number]) {
  const [numerator, denominator] = progress;
  const percent = Math.min(100, Math.max(0, (numerator / denominator) * 100));

  if (longLivedBallons.get(taskId)) { // update exsting ballon
    let balloon = document.getElementById(`progress-balloon-${taskId}`)
    if (balloon === null) {
      deleteLongLivedBalloon(taskId)
      return
    }
    balloon.innerHTML = `
      <span>${message}</span>
      <div class="progress-bar-container">
        <div class="progress-bar" width="${percent}%">
      </div>
    `;
    if (numerator / denominator === 1) {
      balloon.style.opacity = '0'
      balloon.style.transition = 'opacity 0.5s'
      setTimeout(() => deleteLongLivedBalloon(taskId), 500)
    }
  } else { // create new balloon
    if (numerator / denominator === 1) return;
    const container = document.getElementById('balloon-container')!
    const balloon = document.createElement('div')
    balloon.classList.add('balloon')
    balloon.classList.add('balloon-progress-msg')
    balloon.id = `progress-balloon-${taskId}`
    balloon.innerHTML = `
      <span>${message}</span>
      <div class="progress-bar-container">
        <div class="progress-bar" width="${percent}%">
      </div>
    `;
    container.appendChild(balloon)
    longLivedBallons.set(taskId, balloon)
  }
}


function deleteLongLivedBalloon(id: number) {
  let balloon = longLivedBallons.get(id)
  if (balloon) {
    balloon.remove()
    longLivedBallons.delete(id)
  }
}
