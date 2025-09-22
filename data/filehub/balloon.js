const balloons = new Map();
let nextId = 0;
function getNextId() {
    return nextId++;
}
export function pushBalloon(message) {
    switch (message.kind) {
        case "ErrorMsg":
            pushErrorMsgBalloon(message.msg, message.duration);
            break;
        case "ProgressMsg":
            break;
    }
}
function deleteBalloon(id) {
    let balloon = balloons.get(id);
    if (balloon) {
        balloon.remove();
        balloons.delete(id);
    }
}
function pushErrorMsgBalloon(message, duration = 3000) {
    const container = document.getElementById('balloon-container');
    const balloon = document.createElement('div');
    balloon.classList.add('balloon');
    balloon.classList.add('balloon-err-msg');
    balloon.textContent = message;
    container.appendChild(balloon);
    let id = getNextId();
    balloons.set(id, balloon);
    setTimeout(() => {
        balloon.style.opacity = '0';
        balloon.style.transition = 'opacity 0.5s';
        setTimeout(() => deleteBalloon(id), 500);
    }, duration);
}
function pushProgressBarBalloon() {
}
