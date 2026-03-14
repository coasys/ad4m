/**
 * WakerSubscriptionManager — manages live SurrealDB subscriptions for the waker.
 *
 * Extracted from index.ts so it can be tested independently in integration tests.
 * The plugin creates an instance and wires it to the Ad4mClient + wake callback.
 */

import type { WakerSubscription } from "./types";

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
  /** Called when a subscription fires (after debounce). Return value is ignored. */
  onWake: (sub: WakerSubscription, result: any, parentChannel?: string) => void;
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
  private onWake: (sub: WakerSubscription, result: any, parentChannel?: string) => void;
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
    await proxy.subscribe();
    await proxy.initialized;
    this.logger.info(`[waker] ${sub.id}: subscription initialized successfully`);

    // Seed from persisted hash so we don't re-wake for already-seen results after restart
    let lastResultHash: string | null = this.previousResultHashes[sub.id] ?? null;
    if (lastResultHash) {
      this.logger.info(`[waker] ${sub.id}: seeded lastResultHash from persisted state`);
    }

    proxy.onResult(async (result: any) => {
      const serialized = JSON.stringify(result);
      if (lastResultHash === serialized) return;
      lastResultHash = serialized;
      this.resultHashes.set(sub.id, serialized);

      const count = Array.isArray(result) ? result.length : "?";
      this.logger.info(
        `[waker] ${sub.id}: query result changed (${count} items)`,
      );
      this.logger.debug(
        `[waker] ${sub.id}: raw result: ${JSON.stringify(result).substring(0, 500)}`,
      );

      // Extract parent channel from has_child link results
      let parentChannel = sub.channel;
      if (
        !parentChannel &&
        sub.type === "mention" &&
        Array.isArray(result) &&
        result.length > 0
      ) {
        const first = result[0];
        if (first && first.source) {
          parentChannel = first.source;
          this.logger.info(
            `[waker] ${sub.id}: found parent ${parentChannel} from has_child link`,
          );
        } else {
          this.logger.warn(
            `[waker] ${sub.id}: could not extract parent from first result: ${JSON.stringify(first).substring(0, 300)}`,
          );
        }
      }

      // Debounce the wake callback
      const existing = this.debounceTimers.get(sub.id);
      if (existing) clearTimeout(existing);

      this.debounceTimers.set(
        sub.id,
        setTimeout(() => {
          this.onWake(sub, result, parentChannel);
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
