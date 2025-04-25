export function register() {
    window.addEventListener('FileExists', () => showBalloon("Error: file exists"));
    window.addEventListener('InvalidPath', () => showBalloon("Error: invalid path"));
    window.addEventListener('InvalidDir', () => showBalloon("Error: invalid directory"));
    window.addEventListener('InvalidSession', () => showBalloon("Error: invalid session"));
    window.addEventListener('InternalError', () => showBalloon("Error: internal error"));
    window.addEventListener('TargetError', () => showBalloon("Error: target error"));
    window.addEventListener('S3Error', () => showBalloon("Error: s3 error"));

    document.body.addEventListener('htmx:responseError', function (e: any) {
      const xhr = e.detail.xhr;
      const status = xhr.status;

      if (status === 400) {
        showBalloon("Oops! Something went wrong (401)", 5000);
      } else if (status === 401) {
        showBalloon("Oops! Something went wrong (402)", 5000);
      } else if (status === 402) {
        showBalloon("Oops! Something went wrong (403)", 5000);
      } else if (status === 404) {
        showBalloon("Oops! Something went wrong (404)", 5000);
      } else {
        showBalloon(`Oops! Something went wrong ${status}`, 5000);
      }
    });
}


function showBalloon(message: string, duration = 3000) {
  const body = document.getElementsByTagName('body')[0]
  const balloon = document.createElement('div')
  balloon.className = 'balloon'
  balloon.textContent = message

  body.appendChild(balloon)

  setTimeout(() => {
    balloon.style.opacity = '0'
    balloon.style.transition = 'opacity 0.5s'
    setTimeout(() => balloon.remove(), 500)
  }, duration)
}
