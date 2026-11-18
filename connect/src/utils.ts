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
 * A guest's email and password on one host (`ad4m-guest-email-<host>`, `ad4m-guest-pass-<host>`),
 * stored by `connectAsGuest`. They move with the session: an app reads them under the token's
 * prefix to tell a guest's session from a person's, and `connectAsGuest` reuses them to log the
 * same guest in again.
 */
const GUEST_KEY = /^ad4m-guest-(email|pass)-/;

/** The names of the guest credentials stored under `prefix`. */
function guestKeys(prefix: string): string[] {
  const names: string[] = [];
  for (let i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (!key?.startsWith(`${prefix}/`)) continue;
    const name = key.slice(prefix.length + 1);
    if (GUEST_KEY.test(name)) names.push(name);
  }
  return names;
}

/**
 * Carry a session stored by another version of this package over to this version's keys.
 *
 * Every key is stored under `${version}/`, so upgrading the package (an app redeploy) used to
 * leave the stored token under the old prefix, unread: the user had to log in again although the
 * executor still accepted the token. When this version holds no token, the session of the newest
 * older version of the same minor line that holds one is copied over (`0.13.1-staging.4` →
 * `0.13.1-staging.5`, `0.13.1` → `0.13.2`). A minor or major upgrade may break the session, which
 * is why the keys carry the version, so it starts logged out; and a newer version's session is not
 * carried back to an app rolled back to an older version. The token is not checked here; a refused one is
 * dropped by `checkAuth` as before. A guest's credentials move with the session (see `GUEST_KEY`). The other versions' sessions are removed once one is carried
 * over: left in place, a logout (or a refused token, which is discarded) only cleared this
 * version's keys, and the next load carried the old session over again. Answers whether a session
 * was carried over.
 *
 * `url` is the executor URL the app passes (`options.url`), which overrides the stored one: only
 * sessions stored for that executor are candidates, since a token is minted by one executor. A
 * session stored for another URL (or for none) is not carried over, and does not hide an older
 * version's session for the right one.
 */
export function adoptStoredSession(url?: string): boolean {
  if (!localStorageSupported() || localStorage.getItem(`${version}/ad4m-token`)) return false;
  const suffix = "/ad4m-token";
  const prefixes: string[] = [];
  let newest: string | null = null;
  for (let i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (!key?.endsWith(suffix) || !localStorage.getItem(key)) continue;
    const prefix = key.slice(0, key.length - suffix.length);
    // A session from another minor line, from a newer version, or for another executor is neither
    // a candidate nor cleaned up: the app it belongs to can still read it, or carry it over.
    if (!adoptableFrom(prefix) || (url !== undefined && !storedFor(prefix, url))) continue;
    prefixes.push(prefix);
    if (newest === null || compareVersions(newest, prefix) < 0) newest = prefix;
  }
  if (newest === null) return false;
  // All the keys come from `newest`: one it lacks is removed here, not left from this version.
  for (const name of SESSION_KEYS) {
    const value = localStorage.getItem(`${newest}/${name}`);
    if (value !== null) localStorage.setItem(`${version}/${name}`, value);
    else localStorage.removeItem(`${version}/${name}`);
  }
  // The guest credentials come along, so a guest's session stays a guest's.
  for (const name of guestKeys(newest)) {
    localStorage.setItem(`${version}/${name}`, localStorage.getItem(`${newest}/${name}`)!);
  }
  for (const prefix of prefixes) {
    for (const name of [...SESSION_KEYS, ...guestKeys(prefix)]) localStorage.removeItem(`${prefix}/${name}`);
  }
  return true;
}

/** Whether this version may carry over the session under `prefix`: an older one, same major.minor. */
function adoptableFrom(prefix: string): boolean {
  const line = minorLine(version);
  return line !== null && minorLine(prefix) === line && compareVersions(prefix, version) < 0;
}

/** `major.minor` of a version (`0.13` for `0.13.1-staging.5`), or null for a key prefix that is no version. */
function minorLine(v: string): string | null {
  return /^(\d+\.\d+)\.\d+(?:-|$)/.exec(v)?.[1] ?? null;
}

/** Whether the session under `prefix` was stored for the executor at `url`. */
function storedFor(prefix: string, url: string): boolean {
  const stored = localStorage.getItem(`${prefix}/ad4m-url`);
  return stored !== null && wsUrlToHttpBase(stored) === wsUrlToHttpBase(url);
}

/** Package versions by semver precedence: a release ranks above its own prereleases. */
export function compareVersions(a: string, b: string): number {
  const split = (v: string) => {
    const i = v.indexOf("-");
    return i < 0 ? [v, ""] : [v.slice(0, i), v.slice(i + 1)];
  };
  const [aCore, aPre] = split(a);
  const [bCore, bPre] = split(b);
  const byCore = aCore.localeCompare(bCore, undefined, { numeric: true });
  if (byCore !== 0 || aPre === bPre) return byCore;
  if (!aPre) return 1;
  if (!bPre) return -1;
  return aPre.localeCompare(bPre, undefined, { numeric: true });
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

