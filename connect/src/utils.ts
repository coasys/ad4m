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

export async function connectWebSocket(url, timeout = 10000) {
  return Promise.race([
    new Promise((resolve, reject) => {
      let websocket;
      try {
        // Use the same subprotocol that graphql-ws uses
        websocket = new WebSocket(url, "graphql-transport-ws");

        websocket.onopen = () => {
          // Connection successful - close it immediately since we're just checking
          websocket.close();
          resolve(websocket);
        };

        websocket.onerror = (error) => {
          reject(error);
        };

        websocket.onclose = (event) => {
          // If we get a close event before onopen fired, the connection failed
          // onopen will have already resolved if connection was successful
          if (event.code !== 1000) {
            reject(new Error(`WebSocket closed with code ${event.code}: ${event.reason}`));
          }
        };
      } catch (e) {
        if (websocket) {
          websocket.close();
        }
        reject(e);
      }
    }),
    new Promise((resolve, reject) => {
      setTimeout(() => {
        reject(new Error("WebSocket connection timed out"));
      }, timeout);
    }),
  ]);
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
