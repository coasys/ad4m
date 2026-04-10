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

/** Convert a WebSocket URL to an HTTP base URL. */
export function wsUrlToHttpBase(wsUrl: string): string {
  let url = wsUrl.replace(/^ws(s?):\/\//, (_, s) => `http${s}://`);
    url = url.replace(/\/graphql\/?$/, '').replace(/^ws(s?):\/\//, 'http$1://');
  return url;
}

/** Check that the AD4M executor is reachable at the given HTTP base URL. */
export async function checkConnection(baseUrl: string, timeout = 10000): Promise<void> {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), timeout);
  try {
    const res = await fetch(`${baseUrl}/health`, { signal: controller.signal });
    if (!res.ok) throw new Error(`Health check returned ${res.status}`);
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

// Re-export getAd4mClient for backward compatibility with hooks packages
export { getAd4mClient } from "./index";

