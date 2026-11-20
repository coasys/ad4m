//@ts-ignore
import { version } from "../package.json";

/** Whether the page runs inside a frame. */
export function isEmbedded(): boolean {
  return typeof window !== 'undefined' && window.self !== window.top;
}

function localStorageSupported(): boolean {
  try {
    localStorage.setItem("test", "");
    localStorage.removeItem("test");
  } catch (e) {
    return false;
  }
  return true;
}

/** Writes `value` under this package version's prefix (`${version}/${key}`). */
export function setLocal(key: string, value: string): void {
  if (localStorageSupported()) localStorage.setItem(`${version}/${key}`, value);
}

export function getLocal(key: string): string | null {
  if (localStorageSupported()) return localStorage.getItem(`${version}/${key}`);
  return null;
}

export function removeLocal(key: string): void {
  if (localStorageSupported()) localStorage.removeItem(`${version}/${key}`);
}

/**
 * Removes the tokens `@coasys/ad4m-connect` left in `localStorage` (`<version>/ad4m-token`, for
 * every version). This package keeps a token in memory only: a token that stays on disk lives as
 * long as the browser profile, and any script of the page can read it. Answers how many it removed.
 */
export function removeLegacyTokens(): number {
  if (!localStorageSupported()) return 0;
  const legacy: string[] = [];
  for (let i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (key && /^[^/]+\/ad4m-token$/.test(key)) legacy.push(key);
  }
  legacy.forEach((key) => localStorage.removeItem(key));
  return legacy.length;
}
