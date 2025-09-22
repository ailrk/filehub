const longLivedBallons = new Map();
export function pushBalloon(message) {
    switch (message.kind) {
        case "ErrorMsg":
            pushErrorMsgBalloon(message.msg, message.duration);
            break;
        case "ProgressMsg":
            break;
    }
}
function pushErrorMsgBalloon(message, duration = 3000) {
    const container = document.getElementById('balloon-container');
    const balloon = document.createElement('div');
    balloon.classList.add('balloon');
    balloon.classList.add('balloon-err-msg');
    balloon.textContent = message;
    container.appendChild(balloon);
    setTimeout(() => {
        balloon.style.opacity = '0';
        balloon.style.transition = 'opacity 0.5s';
        setTimeout(() => balloon.remove(), 500);
    }, duration);
}
function pushProgressBarBalloon(message, taskId, progress) {
    const [numerator, denominator] = progress;
    const percent = Math.min(100, Math.max(0, (numerator / denominator) * 100));
    if (longLivedBallons.get(taskId)) { // update exsting ballon
        let balloon = document.getElementById(`progress-balloon-${taskId}`);
        if (balloon === null) {
            deleteLongLivedBalloon(taskId);
            return;
        }
        balloon.innerHTML = `
      <span>${message}</span>
      <div class="progress-bar-container">
        <div class="progress-bar" width="${percent}%">
      </div>
    `;
        if (numerator / denominator === 1) {
            balloon.style.opacity = '0';
            balloon.style.transition = 'opacity 0.5s';
            setTimeout(() => deleteLongLivedBalloon(taskId), 500);
        }
    }
    else { // create new balloon
        if (numerator / denominator === 1)
            return;
        const container = document.getElementById('balloon-container');
        const balloon = document.createElement('div');
        balloon.classList.add('balloon');
        balloon.classList.add('balloon-progress-msg');
        balloon.id = `progress-balloon-${taskId}`;
        balloon.innerHTML = `
      <span>${message}</span>
      <div class="progress-bar-container">
        <div class="progress-bar" width="${percent}%">
      </div>
    `;
        container.appendChild(balloon);
        longLivedBallons.set(taskId, balloon);
    }
}
function deleteLongLivedBalloon(id) {
    let balloon = longLivedBallons.get(id);
    if (balloon) {
        balloon.remove();
        longLivedBallons.delete(id);
    }
}
