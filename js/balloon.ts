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
  msg: string;
  progress: [number, number];
}


export type Message = ErrorMessage | ProgressMessage


const balloons: Map<number, HTMLElement> = new Map();

let nextId: number = 0;

function getNextId(): number {
  return nextId++;
}


export function pushBalloon(message: Message) {
  switch (message.kind) {
    case "ErrorMsg":
      pushErrorMsgBalloon(message.msg, message.duration);
      break;
    case "ProgressMsg":
      break;
  }
 }


function deleteBalloon(id: number) {
  let balloon = balloons.get(id)
  if (balloon) {
    balloon.remove()
    balloons.delete(id)
  }
}

function pushErrorMsgBalloon(message: string, duration = 3000) {
  const container = document.getElementById('balloon-container')!
  const balloon = document.createElement('div')
  balloon.classList.add('balloon')
  balloon.classList.add('balloon-err-msg')
  balloon.textContent = message
  container.appendChild(balloon)
  let id = getNextId()
  balloons.set(id, balloon)
  setTimeout(() => {
    balloon.style.opacity = '0'
    balloon.style.transition = 'opacity 0.5s'
    setTimeout(() => deleteBalloon(id), 500)
  }, duration)
}


function pushProgressBarBalloon() {
}
