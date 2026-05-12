/**
 * PersistentCache — two-tier persistent key-value cache abstraction.
 *
 * Provides an IndexedDB-backed implementation for browsers and a no-op
 * NullCache fallback for Node.js / environments without IndexedDB.
 *
 * Design goals:
 * - Lazy open: IDB connection is deferred until first access
 * - Fire-and-forget writes: put() resolves immediately (best-effort persistence)
 * - Graceful degradation: all IDB errors resolve (never reject), so callers
 *   never need try/catch around cache operations
 * - Tree-shakeable: import only what you need
 */

// ── Interface ────────────────────────────────────────────────────────

export interface PersistentCache<T = unknown> {
  get(key: string): Promise<T | undefined>;
  put(key: string, value: T): Promise<void>;
  delete(key: string): Promise<void>;
  clear(): Promise<void>;
}

// ── IndexedDB Implementation ─────────────────────────────────────────

export class IDBCache<T = unknown> implements PersistentCache<T> {
  #dbName: string;
  #storeName: string;
  #dbPromise: Promise<IDBDatabase> | null = null;
  #version = 1;

  constructor(dbName: string, storeName: string) {
    this.#dbName = dbName;
    this.#storeName = storeName;
  }

  #open(): Promise<IDBDatabase> {
    if (!this.#dbPromise) {
      this.#dbPromise = new Promise<IDBDatabase>((resolve, reject) => {
        const storeName = this.#storeName;
        const req = indexedDB.open(this.#dbName, this.#version);
        req.onupgradeneeded = () => {
          const db = req.result;
          if (!db.objectStoreNames.contains(storeName)) {
            db.createObjectStore(storeName);
          }
        };
        req.onsuccess = () => resolve(req.result);
        req.onerror = () => reject(req.error);
      });
    }
    return this.#dbPromise;
  }

  async get(key: string): Promise<T | undefined> {
    try {
      const db = await this.#open();
      return new Promise<T | undefined>((resolve) => {
        const tx = db.transaction(this.#storeName, 'readonly');
        const store = tx.objectStore(this.#storeName);
        const req = store.get(key);
        req.onsuccess = () => resolve(req.result ?? undefined);
        req.onerror = () => resolve(undefined);
      });
    } catch {
      return undefined;
    }
  }

  async put(key: string, value: T): Promise<void> {
    try {
      const db = await this.#open();
      return new Promise<void>((resolve) => {
        const tx = db.transaction(this.#storeName, 'readwrite');
        const store = tx.objectStore(this.#storeName);
        store.put(value, key);
        tx.oncomplete = () => resolve();
        tx.onerror = () => resolve(); // fire-and-forget
      });
    } catch {
      // degrade gracefully
    }
  }

  async delete(key: string): Promise<void> {
    try {
      const db = await this.#open();
      return new Promise<void>((resolve) => {
        const tx = db.transaction(this.#storeName, 'readwrite');
        const store = tx.objectStore(this.#storeName);
        store.delete(key);
        tx.oncomplete = () => resolve();
        tx.onerror = () => resolve();
      });
    } catch {
      // degrade gracefully
    }
  }

  async clear(): Promise<void> {
    try {
      const db = await this.#open();
      return new Promise<void>((resolve) => {
        const tx = db.transaction(this.#storeName, 'readwrite');
        const store = tx.objectStore(this.#storeName);
        store.clear();
        tx.oncomplete = () => resolve();
        tx.onerror = () => resolve();
      });
    } catch {
      // degrade gracefully
    }
  }
}

// ── NullCache (no-op fallback for Node.js) ───────────────────────────

export class NullCache<T = unknown> implements PersistentCache<T> {
  async get(_key: string): Promise<T | undefined> { return undefined; }
  async put(_key: string, _value: T): Promise<void> {}
  async delete(_key: string): Promise<void> {}
  async clear(): Promise<void> {}
}

// ── Factory ──────────────────────────────────────────────────────────

/**
 * Create a PersistentCache backed by IndexedDB (browser) or NullCache (Node.js).
 *
 * @param dbName    - IndexedDB database name
 * @param storeName - IndexedDB object store name
 */
export function createPersistentCache<T = unknown>(
  dbName: string,
  storeName: string,
): PersistentCache<T> {
  try {
    if (typeof indexedDB !== 'undefined') {
      return new IDBCache<T>(dbName, storeName);
    }
  } catch {
    // indexedDB access threw (e.g. in a worker with restricted globals)
  }
  return new NullCache<T>();
}
