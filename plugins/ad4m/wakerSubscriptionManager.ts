/**
 * WakerSubscriptionManager — manages live SurrealDB subscriptions for the waker.
 *
 * Extracted from index.ts so it can be tested independently in integration tests.
 * The plugin creates an instance and wires it to the Ad4mClient + wake callback.
 *
 * NOTE: keep in sync with core/src/perspectives/WakerSubscriptionManager.ts
 * until @coasys/ad4m is published with this code and we can import from there.
 */

export interface WakerSubscription {
  id: string;
  type: "mention" | "channel-messages";
  perspective: string;
  channel: string;
  query: string;
  neighbourhood?: string;
}

/** Per-message parent resolution result for mention subscriptions. */
export interface MentionMessage {
  /** The message's expression address (source of the body link). */
  address: string;
  /** All parent addresses this message belongs to (channels, conversations, etc.). */
  parents: string[];
}

export interface WakerLogger {
  info(msg: string): void;
  warn(msg: string): void;
  error(msg: string): void;
  debug(msg: string): void;
}

export interface WakerSubscriptionManagerOptions {
  /** PerspectiveClient from Ad4mClient (provides perspectiveSubscribeSurrealQuery etc.) */
  perspectiveClient: any;
  /** Logger instance */
  logger: WakerLogger;
  /** Debounce interval in ms before firing the wake callback (default 2000) */
  debounceMs?: number;
  /** Called when a subscription fires (after debounce). Return value is ignored.
   *  For mention subs, `mentions` contains per-message parent info. */
  onWake: (sub: WakerSubscription, result: any, mentions?: MentionMessage[]) => void;
  /** Called when the active subscription list changes (for persistence). Includes last result hashes to avoid duplicate wakes on restart. */
  onPersist?: (subscriptions: WakerSubscription[], resultHashes: Record<string, string>) => void;
  /** Previously persisted result hashes (subscription id → JSON hash). Seeds lastResultHash on resubscribe to avoid duplicate wakes. */
  previousResultHashes?: Record<string, string>;
  /** Optional: provide QuerySubscriptionProxy class directly (avoids require("@coasys/ad4m") at runtime). */
  QuerySubscriptionProxy?: any;
}

export class WakerSubscriptionManager {
  private perspectiveClient: any;
  private logger: WakerLogger;
  private debounceMs: number;
  private onWake: (sub: WakerSubscription, result: any, mentions?: MentionMessage[]) => void;
  private onPersist?: (subscriptions: WakerSubscription[], resultHashes: Record<string, string>) => void;
  private QuerySubscriptionProxyCtor: any;
  private previousResultHashes: Record<string, string>;

  private proxies = new Map<string, any>();
  private activeSubscriptions = new Map<string, WakerSubscription>();
  private debounceTimers = new Map<string, ReturnType<typeof setTimeout>>();
  private resultHashes = new Map<string, string>();

  constructor(options: WakerSubscriptionManagerOptions) {
    this.perspectiveClient = options.perspectiveClient;
    this.logger = options.logger;
    this.debounceMs = options.debounceMs ?? 2000;
    this.onWake = options.onWake;
    this.onPersist = options.onPersist;
    this.QuerySubscriptionProxyCtor = options.QuerySubscriptionProxy ?? null;
    this.previousResultHashes = options.previousResultHashes ?? {};
  }

  /**
   * Create a live SurrealDB subscription.
   * If a subscription with the same id already exists, it is disposed first.
   */
  async subscribe(sub: WakerSubscription): Promise<void> {
    // Dispose existing subscription with same id if any
    this.dispose(sub.id, false);

    if (!this.QuerySubscriptionProxyCtor) {
      throw new Error("WakerSubscriptionManager: QuerySubscriptionProxy must be provided via constructor options");
    }
    const QuerySubscriptionProxy = this.QuerySubscriptionProxyCtor;

    this.logger.info(
      `[waker] ${sub.id}: creating subscription (perspective=${sub.perspective}, type=${sub.type})`,
    );
    this.logger.info(`[waker] ${sub.id}: SurrealQL query:\n${sub.query}`);

    const proxy = new QuerySubscriptionProxy(
      sub.perspective,
      sub.query,
      this.perspectiveClient,
    );
    proxy.isSurrealDB = true;
    // Suppress unhandled rejection from proxy.initialized — QuerySubscriptionProxy
    // rejects this promise internally when subscribe() fails, and if nobody catches
    // it before the next microtask it crashes the process.
    if (proxy.initialized && typeof proxy.initialized.catch === "function") {
      proxy.initialized.catch(() => {});
    }
    try {
      await proxy.subscribe();
      await proxy.initialized;
    } catch (err: any) {
      const msg = err?.message ?? String(err);
      this.logger.warn(
        `[waker] ${sub.id}: subscription failed — ${msg}`,
      );
      try { proxy.dispose(); } catch {}
      // Remove from active state so it doesn't get persisted/retried
      this.activeSubscriptions.delete(sub.id);
      this.resultHashes.delete(sub.id);
      this.persist();
      return; // Don't throw — caller should not crash
    }
    this.logger.info(`[waker] ${sub.id}: subscription initialized successfully`);

    // Seed from persisted hash so we don't re-wake for already-seen results after restart
    let lastResultHash: string | null = this.previousResultHashes[sub.id] ?? null;
    if (lastResultHash) {
      this.logger.info(`[waker] ${sub.id}: seeded lastResultHash from persisted state`);
    }

    proxy.onResult(async (result: any) => {
      this.logger.info(
        `[waker] ${sub.id}: onResult fired — type=${typeof result}, isArray=${Array.isArray(result)}, value=${String(JSON.stringify(result)).substring(0, 500)}`,
      );

      // SurrealDB can deliver non-array values (e.g. false) on disconnect/reconnect — ignore them
      if (!Array.isArray(result)) {
        this.logger.warn(
          `[waker] ${sub.id}: ignoring non-array result: ${JSON.stringify(result)}`,
        );
        return;
      }
      const serialized = JSON.stringify(result);
      if (lastResultHash === serialized) {
        this.logger.info(
          `[waker] ${sub.id}: result unchanged (${result.length} items), skipping`,
        );
        return;
      }
      lastResultHash = serialized;
      this.resultHashes.set(sub.id, serialized);

      const count = result.length;
      this.logger.info(
        `[waker] ${sub.id}: query result changed (${count} items)`,
      );

      // For mention subscriptions, the query returns body links whose target
      // contains a mention. Each result has `source` = message address.
      // We resolve parents per message via a second SurrealDB query.
      let mentions: MentionMessage[] | undefined;

      if (sub.type === "mention" && result.length > 0) {
        const seenMessages = new Set<string>();
        const messageAddresses: string[] = [];
        for (const item of result) {
          if (item && item.source && !seenMessages.has(item.source)) {
            seenMessages.add(item.source);
            messageAddresses.push(item.source);
          }
        }
        this.logger.info(
          `[waker] ${sub.id}: found ${messageAddresses.length} unique message(s): ${messageAddresses.join(", ")}`,
        );

        mentions = [];
        for (const msgAddr of messageAddresses) {
          const parents: string[] = [];
          try {
            const escaped = msgAddr.replace(/'/g, "\\'");
            const parentQuery = `SELECT * FROM link WHERE predicate = 'ad4m://has_child' AND target = '${escaped}'`;
            this.logger.info(`[waker] ${sub.id}: resolving parents for ${msgAddr}`);
            const parentResult = await this.perspectiveClient.querySurrealDB(sub.perspective, parentQuery);
            if (Array.isArray(parentResult)) {
              for (const link of parentResult) {
                if (link && link.source) {
                  parents.push(link.source);
                }
              }
            }
          } catch (err: any) {
            this.logger.warn(
              `[waker] ${sub.id}: parent resolution failed for ${msgAddr} — ${err?.message ?? err}`,
            );
          }
          this.logger.info(
            `[waker] ${sub.id}: message ${msgAddr} has ${parents.length} parent(s): ${parents.join(", ")}`,
          );
          mentions.push({ address: msgAddr, parents });
        }
      }

      // Debounce the wake callback
      const existing = this.debounceTimers.get(sub.id);
      if (existing) clearTimeout(existing);

      this.debounceTimers.set(
        sub.id,
        setTimeout(() => {
          this.onWake(sub, result, mentions);
          this.debounceTimers.delete(sub.id);
          this.persist();
        }, this.debounceMs),
      );
    });

    this.proxies.set(sub.id, proxy);
    this.activeSubscriptions.set(sub.id, sub);
    this.persist();

    this.logger.info(
      `[waker] Subscription ${sub.id} active (type=${sub.type}, perspective=${sub.perspective})`,
    );
  }

  /**
   * Dispose a single subscription.
   * @param persist — if false, skip calling onPersist (used during batch cleanup).
   */
  dispose(id: string, persist = true): void {
    const proxy = this.proxies.get(id);
    if (proxy) {
      try {
        proxy.dispose();
      } catch {
        /* ignore */
      }
      this.proxies.delete(id);
    }
    const timer = this.debounceTimers.get(id);
    if (timer) {
      clearTimeout(timer);
      this.debounceTimers.delete(id);
    }
    this.activeSubscriptions.delete(id);
    this.resultHashes.delete(id);
    if (persist) this.persist();
  }

  /** Dispose all active subscriptions. */
  disposeAll(): void {
    for (const [id] of this.proxies) {
      this.dispose(id, false);
    }
  }

  /** Get all active subscriptions. */
  getActive(): WakerSubscription[] {
    return Array.from(this.activeSubscriptions.values());
  }

  /** Check if a subscription exists. */
  has(id: string): boolean {
    return this.activeSubscriptions.has(id);
  }

  private persist(): void {
    if (this.onPersist) {
      const hashes: Record<string, string> = {};
      for (const [id, hash] of this.resultHashes) {
        hashes[id] = hash;
      }
      this.onPersist(this.getActive(), hashes);
    }
  }
}
