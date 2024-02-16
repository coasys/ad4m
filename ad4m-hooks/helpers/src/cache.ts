import { PerspectiveProxy } from "@coasys/ad4m";

const cache: Map<string, any> = new Map();
const subscribers: Map<string, Function[]> = new Map();

export function getCache<T>(key: string) {
  const match: T | undefined = cache.get(key);
  return match;
}

export function setCache<T>(key: string, value: T) {
  cache.set(key, value);
  getSubscribers(key).forEach((cb) => cb());
}

export function subscribe(key: string, callback: Function) {
  getSubscribers(key).push(callback);
}

export function unsubscribe(key: string, callback: Function) {
  const subs = getSubscribers(key);
  const index = subs.indexOf(callback);
  if (index >= 0) {
    subs.splice(index, 1);
  }
}

export function getSubscribers(key: string) {
  if (!subscribers.has(key)) subscribers.set(key, []);
  return subscribers.get(key)!;
}

export function subscribeToPerspective(
  perspective: PerspectiveProxy,
  added: Function,
  removed: Function
) {
  const addedKey = `perspective-${perspective.uuid}-added`;
  const removedKey = `perspective-${perspective.uuid}-removed`;

  if (!subscribers.has(addedKey)) {
    console.log("subscribing!");
    perspective.addListener("link-added", (link) => {
      subscribers.get(addedKey).forEach((cb) => cb(link));
      return null;
    });
  }

  if (!subscribers.has(removedKey)) {
    perspective.addListener("link-removed", (link) => {
      subscribers.get(removedKey).forEach((cb) => cb(link));
      return null;
    });
  }

  subscribe(addedKey, added);
  subscribe(removedKey, removed);
}

export function unsubscribeToPerspective(
  perspective: PerspectiveProxy,
  added: Function,
  removed: Function
) {
  const addedKey = `perspective-${perspective.uuid}-added`;
  const removedKey = `perspective-${perspective.uuid}-removed`;

  unsubscribe(addedKey, added);
  unsubscribe(removedKey, removed);
}
