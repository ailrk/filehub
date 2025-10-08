const longLivedBallons = new Map();
export function pushBalloon(message) {
    switch (message.kind) {
        case "ErrorMsg":
            pushErrorMsgBalloon(message.msg, message.duration);
            break;
        case "ProgressedMsg":
            {
                let { msg, taskId, progress } = message;
                pushProgressBarBalloon(msg, taskId, progress);
                break;
            }
        case "ProgressingMsg":
            {
                let { taskId } = message;
                break;
            }
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
      <div class="progress-container">
        <div class="progress-bar">
      </div>
    `;
        balloon.querySelector('.progress-bar').style.width = `${percent}%`;
        if (numerator / denominator === 1) {
            setTimeout(() => {
                balloon.style.opacity = '0';
                balloon.style.transition = 'opacity 1s';
                setTimeout(() => deleteLongLivedBalloon(taskId), 1100);
            }, 500);
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
      <div class="progress-container">
        <div class="progress-bar">
      </div>
    `;
        balloon.querySelector('.progress-bar').style.width = `${percent}%`;
        container.appendChild(balloon);
        longLivedBallons.set(taskId, balloon);
    }
}
export function deleteLongLivedBalloon(id) {
    let balloon = longLivedBallons.get(id);
    if (balloon) {
        balloon.remove();
        longLivedBallons.delete(id);
    }
    return longLivedBallons.size;
}
