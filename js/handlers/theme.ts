export function register() {
  document.addEventListener('visibilitychange', () => {
    console.log('vis');
    if (document.visibilityState === 'visible') refreshTheme();
  });
  window.addEventListener('focus', refreshTheme);
}


function getCurrentThemeColor() {
  const meta = document.querySelector('meta[name="theme-color"]');
  return meta?.getAttribute('content')!;
}


function setThemeColor(color: string | null) {
  let meta = document.querySelector('meta[name="theme-color"]');
  if (meta) {
    meta.remove()
  }

  if (color) {
    meta = document.createElement('meta');
    (meta as any).name = 'theme-color';
    document.head.appendChild(meta);
    meta.setAttribute('content', color);
  }
}


function refreshTheme () {
  console.log('refresh theme', getCurrentThemeColor())
  setThemeColor(getCurrentThemeColor());
};
