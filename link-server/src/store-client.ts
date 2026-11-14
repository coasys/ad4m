import { KelChain, type KelHandle, type SignedHead } from "./identity-core.js";

/**
 * The link server's reader of the identity store: which person a device key speaks for, from
 * the key's log as the store serves it (`GET /v1/keys/{multikey}`, `/v1/logs/{scid}`), trusted
 * only up to a head one of `headKeys` signed. The server folds each log itself (the identity
 * core's WASM) and keeps it, fetching only the events past what it holds.
 *
 * Fails closed: no store, a store that does not answer, a head no trusted key signed, or a log
 * that does not continue the one held (a gap or a fork) is `StoreUnavailable`. Nothing is
 * admitted on it.
 */

/** The store could not tell whom a key speaks for. */
export class StoreUnavailable extends Error {
  constructor(message: string) {
    super(message);
    this.name = "StoreUnavailable";
  }
}

/** Whom a key speaks for: the person of its log, and whether it is one of their bots. */
export interface KeyAuthor {
  person: string;
  byBot: boolean;
}

/**
 * What the store says of a key. `unlisted`: no log lists it (a device-only key). `listed`: the
 * log that lists it, trusted up to `trusted`, and `author`, whom it speaks for now (null when
 * the key is revoked, expired or otherwise no standing device key at `trusted`).
 */
export type KeyResolution =
  | { kind: "unlisted" }
  | { kind: "listed"; scid: string; handle: KelHandle; trusted: number; author: KeyAuthor | null };

interface KeyIndexEntry {
  did: string;
  head: SignedHead;
}

interface LogPage {
  events: unknown[];
}

export interface StoreClientOptions {
  /** The store's base URL; without one every resolution is `StoreUnavailable`. */
  url?: string;
  /** Multikeys of the store keys whose signed heads are trusted. */
  headKeys: string[];
  /** How long a resolution serves `resolve(key)` without `fresh` (ms). Default 60 000. */
  cacheMs?: number;
  /** Per-request timeout (ms). Default 10 000. */
  timeoutMs?: number;
}

/** The most events the store returns per log page. */
const LOG_PAGE = 256;

function multikeyOf(didKey: string): string {
  if (!didKey.startsWith("did:key:")) throw new StoreUnavailable(`not a did:key: ${didKey}`);
  return didKey.slice("did:key:".length);
}

export class StoreClient {
  private readonly url?: string;
  private readonly headKeys: string[];
  private readonly cacheMs: number;
  private readonly timeoutMs: number;
  private readonly logs = new Map<string, KelHandle>();
  private readonly cache = new Map<string, { at: number; resolution: KeyResolution }>();

  constructor(opts: StoreClientOptions) {
    this.url = opts.url?.replace(/\/+$/, "");
    this.headKeys = opts.headKeys;
    this.cacheMs = opts.cacheMs ?? 60_000;
    this.timeoutMs = opts.timeoutMs ?? 10_000;
  }

  /** Whether a store is configured at all. */
  get configured(): boolean {
    return !!this.url && this.headKeys.length > 0;
  }

  /**
   * Whom `didKey` speaks for. `fresh`: ask the store now (auth, refresh); otherwise an answer
   * up to `cacheMs` old serves (judging links).
   */
  async resolve(didKey: string, opts: { fresh?: boolean; now?: number } = {}): Promise<KeyResolution> {
    const now = opts.now ?? Date.now();
    const cached = this.cache.get(didKey);
    if (!opts.fresh && cached && now - cached.at < this.cacheMs) return cached.resolution;
    const resolution = await this.fetchResolution(didKey, now);
    this.cache.set(didKey, { at: now, resolution });
    return resolution;
  }

  private async fetchResolution(didKey: string, now: number): Promise<KeyResolution> {
    if (!this.url) throw new StoreUnavailable("no identity store is configured (STORE_URL)");
    if (this.headKeys.length === 0) throw new StoreUnavailable("no store head keys are configured (STORE_HEAD_KEYS)");
    const multikey = multikeyOf(didKey);
    const entry = await this.get<KeyIndexEntry>(`/v1/keys/${encodeURIComponent(multikey)}`);
    if (entry === null) return { kind: "unlisted" };
    if (!entry.head || typeof entry.head.scid !== "string" || typeof entry.head.seq !== "number") {
      throw new StoreUnavailable("the store's key entry carries no head");
    }
    const handle = await this.logUpTo(entry.head);
    const trusted = handle.trustedSeq(entry.head, this.headKeys);
    if (trusted === undefined) throw new StoreUnavailable(`no trusted store key signed the head of ${entry.head.scid}`);
    const author = handle.authorOf(multikey, trusted, now) as KeyAuthor | null;
    return { kind: "listed", scid: entry.head.scid, handle, trusted, author };
  }

  /** The log `head` names, held at least up to `head.seq`. */
  private async logUpTo(head: SignedHead): Promise<KelHandle> {
    let handle = this.logs.get(head.scid);
    while (!handle || (handle.head() as { seq: number }).seq < head.seq) {
      const from = handle ? (handle.head() as { seq: number }).seq + 1 : 0;
      const page = await this.get<LogPage>(`/v1/logs/${encodeURIComponent(head.scid)}?from=${from}`);
      if (page === null || !Array.isArray(page.events) || page.events.length === 0) {
        throw new StoreUnavailable(`the store has no events of ${head.scid} from ${from}`);
      }
      const events = page.events.slice(0, LOG_PAGE);
      if (!handle) {
        try {
          handle = KelChain.fold(events);
        } catch (e) {
          throw new StoreUnavailable(`the log of ${head.scid} does not fold: ${JSON.stringify(e)}`);
        }
        if (handle.id() !== head.scid) throw new StoreUnavailable(`the store served another log for ${head.scid}`);
      } else {
        let outcome: { kind: string };
        try {
          outcome = KelChain.import(handle, events) as { kind: string };
        } catch (e) {
          throw new StoreUnavailable(`the log of ${head.scid} does not import: ${JSON.stringify(e)}`);
        }
        if (outcome.kind !== "appended") {
          throw new StoreUnavailable(`the log of ${head.scid} does not continue the one held: ${outcome.kind}`);
        }
      }
      this.logs.set(head.scid, handle);
    }
    return handle;
  }

  /** GET `path` as JSON; null on 404. */
  private async get<T>(path: string): Promise<T | null> {
    let res: Response;
    try {
      res = await fetch(`${this.url}${path}`, { signal: AbortSignal.timeout(this.timeoutMs) });
    } catch (e) {
      throw new StoreUnavailable(`the identity store did not answer: ${(e as Error).message}`);
    }
    if (res.status === 404) {
      await res.body?.cancel();
      return null;
    }
    if (!res.ok) {
      await res.body?.cancel();
      throw new StoreUnavailable(`the identity store answered ${res.status}`);
    }
    try {
      return (await res.json()) as T;
    } catch {
      throw new StoreUnavailable("the identity store answered with malformed JSON");
    }
  }

  close(): void {
    for (const handle of this.logs.values()) handle.free();
    this.logs.clear();
    this.cache.clear();
  }
}
