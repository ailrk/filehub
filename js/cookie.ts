function getCookie(name: string) {
  const value = `; ${document.cookie}`;
  const parts: string[] = value.split(`; ${name}=`);
  if (parts.length === 2)
    return parts.pop()!.split(';').shift();
}


function deleteCookie(name: string) {
  document.cookie = name + "=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;";
}


interface SetCookieOptions {
  path?: string ,
  maxAge?: string,
  expires?: Date,
  secure?: boolean,
  sameSite?: string

}

function setCookie(name: string, value: string, options: SetCookieOptions) {
  const {
    path = '/',
    maxAge,
    expires,
    secure = false,
    sameSite = 'Lax'
  } = options;

  let cookie = `${encodeURIComponent(name)}=${encodeURIComponent(value)}; path=${path}`;

  if (maxAge !== undefined) {
    cookie += `; max-age=${maxAge}`;
  }

  if (expires instanceof Date) {
    cookie += `; expires=${expires.toUTCString()}`;
  }

  if (secure) {
    cookie += `; Secure`;
  }

  if (sameSite) {
    cookie += `; SameSite=${sameSite}`;
  }

  document.cookie = cookie;
}



export {
  getCookie,
  deleteCookie,
  setCookie
}
