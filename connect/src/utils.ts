//@ts-ignore
import { version } from "../../package.json";

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

/** The keys that make up a stored session. They move together, from one version's prefix. */
const SESSION_KEYS = ["ad4m-token", "ad4m-url", "ad4m-port", "ad4m-last-host"];

/**
 * Carry a session stored by another version of this package over to this version's keys.
 *
 * Every key is stored under `${version}/`, so upgrading the package (an app redeploy) used to
 * leave the stored token under the old prefix, unread: the user had to log in again although the
 * executor still accepted the token. When this version holds no token, the session of the newest
 * other version that holds one is copied over. The token is not checked here; a refused one is
 * dropped by `checkAuth` as before. The other versions' sessions are removed once one is carried
 * over: left in place, a logout (or a refused token, which is discarded) only cleared this
 * version's keys, and the next load carried the old session over again. Answers whether a session
 * was carried over.
 */
export function adoptStoredSession(): boolean {
  if (!localStorageSupported() || localStorage.getItem(`${version}/ad4m-token`)) return false;
  const suffix = "/ad4m-token";
  const prefixes: string[] = [];
  let newest: string | null = null;
  for (let i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (!key?.endsWith(suffix) || !localStorage.getItem(key)) continue;
    const prefix = key.slice(0, key.length - suffix.length);
    if (prefix === version) continue;
    prefixes.push(prefix);
    if (newest === null || newest.localeCompare(prefix, undefined, { numeric: true }) < 0) newest = prefix;
  }
  if (newest === null) return false;
  for (const name of SESSION_KEYS) {
    const value = localStorage.getItem(`${newest}/${name}`);
    if (value !== null) localStorage.setItem(`${version}/${name}`, value);
  }
  for (const prefix of prefixes) {
    for (const name of SESSION_KEYS) localStorage.removeItem(`${prefix}/${name}`);
  }
  return true;
}

/** Convert a WebSocket URL to an HTTP base URL. */
export function wsUrlToHttpBase(wsUrl: string): string {
  return wsUrl.replace(/^ws(s?):\/\//, (_, s) => `http${s}://`);
}

/** Check that the AD4M executor is reachable at the given HTTP base URL. */
export async function checkConnection(baseUrl: string, timeout = 10000): Promise<void> {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeout);
  try {
    const res = await fetch(`${baseUrl}/health`, { signal: controller.signal });
    if (!res.ok) throw new Error(`Health check returned ${res.status}`);
    const body = await res.json();
    if (body?.status !== 'ok') throw new Error('Not an AD4M executor');
  } catch (e: any) {
    if (e.name === 'AbortError') throw new Error('Connection timed out');
    throw e;
  } finally {
    clearTimeout(timer);
  }
}

/** Extract up to 2 initials from a name (first letter of first two words) */
export function getInitials(name: string): string {
  const clean = name.trim();
  if (!clean) return "";
  const words = clean.split(/\s+/);
  if (words.length >= 2) return (words[0][0] + words[1][0]).toUpperCase();
  return clean.slice(0, 2).toUpperCase();
}

/** Deterministic hue (0-359) from a string — same input always yields same color */
export function getHue(str: string): number {
  let hash = 0;
  for (let i = 0; i < str.length; i++) hash = str.charCodeAt(i) + ((hash << 5) - hash);
  return ((hash % 360) + 360) % 360;
}

