
let holdTimer: number;

export function register() {
  window.addEventListener('mousedown', () => {
    holdTimer = setTimeout(() => {
      // select
    }, 500);
  });

  window.addEventListener('mouseup', () => {
    clearTimeout(holdTimer);
  });
}
