import { Ad4mClient } from "@perspect3vism/ad4m";
import { ClientStates } from "./core";

function Timeout() {
  const controller = new AbortController();
  setTimeout(() => controller.abort(), 20);
  return controller;
}

export async function checkPort(port: number) {
  try {
    const res = await fetch(`http://localhost:${port}/graphql/`, {
      signal: Timeout().signal,
      mode: "no-cors",
    });

    if (res.status === 400 || res.status === 0) {
      return port;
    } else {
      return null;
    }
  } catch (e) {
    throw e;
  }
}

export function getAd4mClient(): Promise<Ad4mClient> {
  return new Promise((resolve, reject) => {
    const el = document.querySelector("ad4m-connect");

    // @ts-ignore
    const client = el?.getAd4mClient();

    if (client) {
      resolve(client);
    } else {
      reject("No Ad4mClient found");
    }
  });
}

export function onAuthStateChanged(callback) {
  const el = document.querySelector("ad4m-connect");

  el?.addEventListener("authStateChange", (e: CustomEvent) => {
    callback(e.detail as ClientStates);
  });
}

export function isConnected() {
  const el = document.querySelector("ad4m-connect");
  //@ts-ignore
  return el.connected();
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
