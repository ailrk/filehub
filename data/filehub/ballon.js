export function showBalloon(message, duration = 3000) {
    const body = document.getElementsByTagName('body')[0];
    const balloon = document.createElement('div');
    balloon.className = 'balloon';
    balloon.textContent = message;
    body.appendChild(balloon);
    setTimeout(() => {
        balloon.style.opacity = '0';
        balloon.style.transition = 'opacity 0.5s';
        setTimeout(() => balloon.remove(), 500);
    }, duration);
}
