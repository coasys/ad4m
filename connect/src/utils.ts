import { Ad4mClient } from "@coasys/ad4m";
import { AuthStates } from "./core";

// @ts-ignore
import { version } from "../package.json";

export const DEFAULT_PORT = 12000;

function Timeout() {
  const controller = new AbortController();
  setTimeout(() => controller.abort(), 20);
  return controller;
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

export async function checkPort(port: number) {
  try {
    const res = await fetch(`http://localhost:${port}/graphql/`, {
      signal: Timeout().signal,
      mode: "cors",
    });

    if (res.status === 400 || res.status === 0 || res.status === 200) {
      return port;
    } else {
      throw new Error(`Could not connect to port ${port}`);
    }
  } catch (e) {
    throw new Error(`Could not connect to port ${port}`);
  }
}

export function onAuthStateChanged(callback) {
  const el = document.querySelector("ad4m-connect");

  el?.addEventListener("authstatechange", (e: CustomEvent) => {
    callback(e.detail as AuthStates);
  });
}

export function detectOS(): string {
  let os = navigator.userAgent;
  let finalOs = "";
  if (os.search("Windows") !== -1) {
    finalOs = "Windows";
  } else if (os.search("Mac") !== -1) {
    finalOs = "MacOS";
  } else if (os.search("X11") !== -1 && !(os.search("Linux") !== -1)) {
    finalOs = "UNIX";
  } else if (os.search("Linux") !== -1 && os.search("X11") !== -1) {
    finalOs = "Linux";
  }
  return finalOs;
}

function isSupported(): boolean {
  try {
    localStorage.setItem("test", "");
    localStorage.removeItem("test");
  } catch (e) {
    return false;
  }
  return true;
}

export function setForVersion(key: string, value: string): void {
  if (isSupported()) {
    localStorage.setItem(`${version}/${key}`, value);
  }
}

export function getForVersion(key: string): string | null {
  if (isSupported()) {
    return localStorage.getItem(`${version}/${key}`);
  }
  return null;
}

export function removeForVersion(key: string): void {
  if (isSupported()) {
    localStorage.removeItem(`${version}/${key}`);
  }
}

// Re-export getAd4mClient for backwards compatibility
export { getAd4mClient } from "./index";
