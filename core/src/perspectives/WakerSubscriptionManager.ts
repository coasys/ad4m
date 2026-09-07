/**
 * WakerSubscriptionManager — manages live query subscriptions for the waker.
 *
 * This is the single implementation: it is exported from `@coasys/ad4m` and
 * used by the integration tests, and the OpenClaw AD4M plugin re-exports this
 * module (`plugins/ad4m/wakerSubscriptionManager.ts`) rather than carrying its
 * own copy. Any behaviour change belongs here.
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
  /** PerspectiveClient from Ad4mClient (provides subscribeQuery etc.) */
  perspectiveClient: any;
  /** Logger instance */
  logger: WakerLogger;
  /** Debounce interval in ms before firing the wake callback (default 2000) */
  debounceMs?: number;
  /** Called when a subscription fires (after debounce). Return value is ignored.
   *  For mention subs, `mentions` contains per-message parent info. */
  onWake: (sub: WakerSubscription, result: any, mentions?: MentionMessage[]) => void;
  /** Called when the active subscription list changes (for persistence). */
  onPersist?: (subscriptions: WakerSubscription[], seenMessages: Record<string, string[]>) => void;
  /** Previously persisted seen message IDs per subscription. Seeds seenMessages on resubscribe to avoid duplicate wakes. */
  previousSeenMessages?: Record<string, string[]>;
  /** Optional: provide QuerySubscriptionProxy class directly (avoids require("@coasys/ad4m") at runtime). */
  QuerySubscriptionProxy?: any;
  /** How long to wait before re-attempting a subscription the executor rejected
   *  (default 30000). 0 disables re-attempts. */
  retryPendingMs?: number;
}

/**
 * Add a short cause hint to known subscription failures.
 * The executor needs its main key to register a live query; on a restarted
 * executor whose wallet was never unlocked this surfaces as a bare 403.
 */
export function hintFor(msg: string): string {
  if (/main key not found/i.test(msg)) {
    return " — the executor's wallet is locked (keys are held in memory only); unlock the agent and subscribe again";
  }
  return "";
}

export class WakerSubscriptionManager {
  private perspectiveClient: any;
  private logger: WakerLogger;
  private debounceMs: number;
  private onWake: (sub: WakerSubscription, result: any, mentions?: MentionMessage[]) => void;
  private onPersist?: (subscriptions: WakerSubscription[], seenMessages: Record<string, string[]>) => void;
  private QuerySubscriptionProxyCtor: any;
  private previousSeenMessages: Record<string, string[]>;
  private retryPendingMs: number;

  private proxies = new Map<string, any>();
  private activeSubscriptions = new Map<string, WakerSubscription>();
  /** Subscriptions the executor rejected, waiting to be re-attempted. */
  private pendingSubscriptions = new Map<string, WakerSubscription>();
  private retryTimer: ReturnType<typeof setTimeout> | null = null;
  /**
   * Cancellation tokens for in-flight `subscribe()` calls.
   *
   * `subscribe()` awaits `proxy.subscribe()` and `proxy.initialized`; a
   * `dispose()` / `disposeAll()` landing during either await used to leave a
   * live subscription behind, because nothing stopped the resumed call from
   * registering the proxy in `proxies` / `activeSubscriptions` afterwards.
   * `epoch` is bumped by `disposeAll()` (which cannot enumerate in-flight
   * ids), `generations` per id by `dispose()` and by each new `subscribe()`.
   */
  private epoch = 0;
  private generations = new Map<string, number>();
  private debounceTimers = new Map<string, ReturnType<typeof setTimeout>>();
  /** Per-subscription set of already-processed message addresses (for mention subs)
   *  or serialized result hash (for channel-messages subs). */
  private seenMessages = new Map<string, Set<string>>();

  constructor(options: WakerSubscriptionManagerOptions) {
    this.perspectiveClient = options.perspectiveClient;
    this.logger = options.logger;
    this.debounceMs = options.debounceMs ?? 2000;
    this.onWake = options.onWake;
    this.onPersist = options.onPersist;
    this.QuerySubscriptionProxyCtor = options.QuerySubscriptionProxy ?? null;
    this.previousSeenMessages = options.previousSeenMessages ?? {};
    this.retryPendingMs = options.retryPendingMs ?? 30000;
  }

  /** A token identifying this attempt at subscribing `id`. */
  private beginAttempt(id: string): { epoch: number; gen: number } {
    const gen = (this.generations.get(id) ?? 0) + 1;
    this.generations.set(id, gen);
    return { epoch: this.epoch, gen };
  }

  /** Whether this attempt has been superseded by a dispose or a newer one. */
  private isStale(id: string, token: { epoch: number; gen: number }): boolean {
    return token.epoch !== this.epoch || this.generations.get(id) !== token.gen;
  }

  /**
   * Create a live SPARQL subscription.
   * If a subscription with the same id already exists, it is disposed first.
   */
  async subscribe(sub: WakerSubscription): Promise<void> {
    // Dispose existing subscription with same id if any
    this.dispose(sub.id, false);
    const token = this.beginAttempt(sub.id);

    if (!this.QuerySubscriptionProxyCtor) {
      throw new Error("WakerSubscriptionManager: QuerySubscriptionProxy must be provided via constructor options");
    }
    const QuerySubscriptionProxy = this.QuerySubscriptionProxyCtor;

    this.logger.info(
      `[waker] ${sub.id}: creating subscription (perspective=${sub.perspective}, type=${sub.type})`,
    );
    this.logger.info(`[waker] ${sub.id}: SPARQL query:\n${sub.query}`);

    const proxy = new QuerySubscriptionProxy(
      sub.perspective,
      sub.query,
      this.perspectiveClient,
    );
    proxy.isSPARQL = true;
    // Suppress unhandled rejection from proxy.initialized — QuerySubscriptionProxy
    // rejects this promise internally when subscribe() fails, and if nobody catches
    // it before the next microtask it crashes the process.
    if (proxy.initialized && typeof proxy.initialized.catch === "function") {
      proxy.initialized.catch(() => {});
    }
    try {
      await proxy.subscribe();
      if (this.isStale(sub.id, token)) {
        try { proxy.dispose(); } catch {}
        return;
      }
      await proxy.initialized;
    } catch (err: any) {
      const msg = err?.message ?? String(err);
      this.logger.warn(
        `[waker] ${sub.id}: subscription failed — ${msg}`,
      );
      try { proxy.dispose(); } catch {}
      // A dispose that landed mid-attempt wins: don't re-queue something the
      // caller has since asked us to forget.
      if (this.isStale(sub.id, token)) return;
      // Remove from active state — it is not listening, so it must not be
      // reported as active or persisted as if it were.
      this.activeSubscriptions.delete(sub.id);
      this.seenMessages.delete(sub.id);
      this.persist();
      // Keep it pending and re-attempt: the usual cause (locked wallet, executor
      // mid-restart) clears on its own, and the caller asked to be enrolled.
      this.pendingSubscriptions.set(sub.id, sub);
      this.scheduleRetry();
      // Throw so callers can report the real outcome. Background callers
      // (auto-subscribe, restore-on-start) catch and log; the subscribe tools
      // turn this into an error result instead of a false "Subscribed" reply.
      throw new Error(
        `Waker subscription ${sub.id} failed: ${msg}${hintFor(msg)}` +
          (this.retryPendingMs > 0
            ? `. Not listening yet — re-attempting every ${Math.round(this.retryPendingMs / 1000)}s; check ad4m_list_waker_subscriptions.`
            : ""),
      );
    }
    if (this.isStale(sub.id, token)) {
      try { proxy.dispose(); } catch {}
      return;
    }
    this.pendingSubscriptions.delete(sub.id);
    this.logger.info(`[waker] ${sub.id}: subscription initialized successfully`);

    // Seed from persisted seen messages so we don't re-wake after restart
    const seen = new Set<string>(this.previousSeenMessages[sub.id] ?? []);
    this.seenMessages.set(sub.id, seen);
    if (seen.size > 0) {
      this.logger.info(`[waker] ${sub.id}: seeded ${seen.size} seen message(s) from persisted state`);
    }

    proxy.onResult(async (result: any) => {
      this.logger.info(
        `[waker] ${sub.id}: onResult fired — type=${typeof result}, isArray=${Array.isArray(result)}, value=${String(JSON.stringify(result)).substring(0, 500)}`,
      );

      // Query engine can deliver non-array values (e.g. false) on disconnect/reconnect — ignore them
      if (!Array.isArray(result)) {
        this.logger.warn(
          `[waker] ${sub.id}: ignoring non-array result: ${JSON.stringify(result)}`,
        );
        return;
      }

      if (sub.type === "mention") {
        // For mention subscriptions: track seen message addresses.
        // Only wake for messages we haven't seen before.
        const currentSeen = this.seenMessages.get(sub.id) ?? new Set<string>();
        const newMessages: string[] = [];

        for (const item of result) {
          if (item && item.source && !currentSeen.has(item.source)) {
            newMessages.push(item.source);
          }
        }

        if (newMessages.length === 0) {
          this.logger.info(
            `[waker] ${sub.id}: all ${result.length} items already seen, skipping`,
          );
          return;
        }

        this.logger.info(
          `[waker] ${sub.id}: ${newMessages.length} new message(s) out of ${result.length} total`,
        );

        // Resolve parents per new message
        const mentions: MentionMessage[] = [];
        for (const msgAddr of newMessages) {
          const parents: string[] = [];
          try {
            const parentQuery = `SELECT ?source WHERE { ?source <ad4m://has_child> <${msgAddr}> . }`;
            this.logger.info(`[waker] ${sub.id}: resolving parents for ${msgAddr}`);
            const parentResult = await this.perspectiveClient.querySparql(sub.perspective, parentQuery);
            const bindings = parentResult?.results?.bindings;
            if (Array.isArray(bindings)) {
              for (const binding of bindings) {
                if (binding?.source?.value) {
                  parents.push(binding.source.value);
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

        // Mark all new messages as seen
        for (const addr of newMessages) {
          currentSeen.add(addr);
        }
        this.seenMessages.set(sub.id, currentSeen);

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
      } else {
        // For channel-messages: always notify the agent on every subscription push.
        // No deduplication — the agent reads recent messages itself.
        this.logger.info(
          `[waker] ${sub.id}: channel update (${result.length} items)`,
        );

        const existing = this.debounceTimers.get(sub.id);
        if (existing) clearTimeout(existing);

        this.debounceTimers.set(
          sub.id,
          setTimeout(() => {
            this.onWake(sub, result);
            this.debounceTimers.delete(sub.id);
          }, this.debounceMs),
        );
      }
    });

    if (this.isStale(sub.id, token)) {
      try { proxy.dispose(); } catch {}
      return;
    }
    this.proxies.set(sub.id, proxy);
    this.activeSubscriptions.set(sub.id, sub);
    this.persist();

    this.logger.info(
      `[waker] Subscription ${sub.id} active (type=${sub.type}, perspective=${sub.perspective})`,
    );
  }

  /** Arm the re-attempt timer if pending subscriptions are waiting. */
  private scheduleRetry(): void {
    if (this.retryPendingMs <= 0) return;
    if (this.retryTimer || this.pendingSubscriptions.size === 0) return;
    this.retryTimer = setTimeout(() => {
      this.retryTimer = null;
      void this.retryPending();
    }, this.retryPendingMs);
    if (typeof this.retryTimer.unref === "function") this.retryTimer.unref();
  }

  /**
   * Re-attempt every subscription the executor rejected.
   * Each attempt that fails puts itself back into the pending set and re-arms
   * the timer, so an agent that asked to be subscribed ends up enrolled once
   * the executor can accept the query.
   */
  async retryPending(): Promise<void> {
    const waiting = Array.from(this.pendingSubscriptions.values());
    if (waiting.length === 0) return;
    this.logger.info(
      `[waker] re-attempting ${waiting.length} pending subscription(s)`,
    );
    const epoch = this.epoch;
    for (const sub of waiting) {
      // A disposeAll() between two awaits must stop the whole sweep, and a
      // dispose() of this id must skip just it.
      if (epoch !== this.epoch) return;
      if (!this.pendingSubscriptions.has(sub.id)) continue;
      this.pendingSubscriptions.delete(sub.id);
      try {
        await this.subscribe(sub);
        this.logger.info(`[waker] ${sub.id}: enrolled on re-attempt`);
      } catch (err: any) {
        // subscribe() already logged, re-queued and re-armed the timer.
        this.logger.debug(
          `[waker] ${sub.id}: still pending — ${err?.message ?? err}`,
        );
      }
    }
  }

  /** Subscriptions that were rejected and are waiting to be re-attempted. */
  getPending(): WakerSubscription[] {
    return Array.from(this.pendingSubscriptions.values());
  }

  /**
   * Dispose a single subscription.
   * @param persist — if false, skip calling onPersist (used during batch cleanup).
   */
  dispose(id: string, persist = true): void {
    // Invalidate any subscribe() for this id that is still awaiting.
    this.generations.set(id, (this.generations.get(id) ?? 0) + 1);
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
    this.pendingSubscriptions.delete(id);
    this.seenMessages.delete(id);
    if (persist) this.persist();
  }

  /** Dispose all active subscriptions and drop anything still pending. */
  disposeAll(): void {
    // Bump the epoch first: in-flight subscribe() calls have no proxy in the
    // map yet, so per-id invalidation alone cannot reach them.
    this.epoch++;
    for (const [id] of this.proxies) {
      this.dispose(id, false);
    }
    this.pendingSubscriptions.clear();
    if (this.retryTimer) {
      clearTimeout(this.retryTimer);
      this.retryTimer = null;
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
      const seen: Record<string, string[]> = {};
      for (const [id, set] of this.seenMessages) {
        seen[id] = Array.from(set);
      }
      this.onPersist(this.getActive(), seen);
    }
  }
}
