export function register() {
  window.addEventListener('FileExists', () => alert("Error: file exists"));
  window.addEventListener('InvalidPath', () => alert("Error: invalid path"));
  window.addEventListener('InvalidDir', () => alert("Error: invalid directory"));
  window.addEventListener('InvalidSession', () => alert("Error: invalid session"));
  window.addEventListener('InternalError', () => alert("Error: internal error"));
  window.addEventListener('TargetError', () => alert("Error: target error"));
  window.addEventListener('S3Error', () => alert("Error: s3 error"));
}
