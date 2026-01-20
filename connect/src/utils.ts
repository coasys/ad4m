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
