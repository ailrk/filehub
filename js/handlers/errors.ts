import { showBalloon } from '../ballon.js'

const wait = 2000

export function register() {
    document.body.addEventListener('htmx:responseError', function (e: any) {
      const xhr = e.detail.xhr;
      const status = xhr.status;
      let message = xhr.responseText;
      if (message === undefined || message === ""){
        if (status === 400) {
          message = "Bad Request"
        } else if (status === 401) {
          message = "Unauthorized"
        } else if (status === 402) {
          message = "Payment required"
        } else if (status === 403) {
          message = "Forbidden resource"
        } else if (status === 404) {
          message = "Resource not found"
        } else if (status === 500) {
          message = "Server internal error"
        } else {
          message = "Something went wrong"
        }
      }
      showBalloon(`${message} ${status}`, wait);
    });
}
