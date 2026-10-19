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
  /** SPARQL, or with `className` a model query (JSON) over that class. */
  query: string;
  /**
   * The class `query` reads, as a model query. A mention subscription has
   * one, from the executor's `get_mention_waker_config`: the built-in
   * `ad4m://TextContent` (#1446).
   */
  className?: string;
  /**
   * A mention subscription's terms (profile names and the DID), from
   * `get_mention_waker_config`. The query matches substrings, so a record
   * wakes only when its text holds one of these as a whole word
   * ({@link holdsAWholeWordTerm}); without them a mention subscription is
   * refused.
   */
  terms?: string[];
  neighbourhood?: string;
}

/** A letter, combining mark, digit or underscore: what a term must not touch. */
const WORD_CHAR = "[\\p{L}\\p{M}\\p{N}_]";

/**
 * Whether `text` holds one of `terms` as a whole word, case-insensitively:
 * "Data" is in "hi Data!", "@data" and "Data's turn", not in "metadata" or
 * "database". A term is matched literally (regex characters, quotes) and must
 * not touch a letter, mark, digit or underscore on either side, so a name
 * that starts or ends with punctuation still matches. Same rule as #1459's
 * SPARQL `(?:^|\W)term(?:\W|$)`.
 *
 * The mention query's `contains` matches substrings (model query has no word
 * match); this is the check that keeps "data" from waking on "metadata".
 */
export function holdsAWholeWordTerm(text: string, terms: string[]): boolean {
  return terms.some((term) => {
    if (!term) return false;
    const escaped = term.replace(/[\\^$.*+?()[\]{}|\/]/g, "\\$&");
    return new RegExp(`(?<!${WORD_CHAR})${escaped}(?!${WORD_CHAR})`, "iu").test(text);
  });
}

/** The built-in `Container` (`Container.builtinUri`; not imported, so the
 *  plugin bundle does not pull in the model layer). */
const CONTAINER = "ad4m://Container";

/** Per-message parent resolution result for mention subscriptions. */
export interface MentionMessage {
  /** The mentioned record's id. */
  address: string;
  /** Every `Container` whose `children` hold it: channels, calls, threads. */
  parents: string[];
  /**
   * Everything above the parents, nearest first: their parents, theirs, and so on up to the
   * manager's `ancestorDepth`. Each address once, however many paths reach it. Empty when the
   * parents have none, or the walk could not be made.
   */
  ancestors: string[];
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
  /**
   * How many levels above a mentioned item's parents to report as `ancestors` (default 3).
   * Each level is one batched query, so the walk is bounded by this depth and by
   * what the levels hold. 0 reports parents only.
   */
  ancestorDepth?: number;
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
  /**
   * How long the executor gets to answer the subscribe handshake before the
   * attempt is abandoned and queued for re-attempt (default 15000). 0 removes
   * the bound. Covers both `proxy.subscribe()` and `proxy.initialized`: when
   * the executor accepts the socket but never answers, neither settles, and
   * without a deadline the subscription ended up neither active nor pending,
   * so the re-attempt cycle never saw it (#1016).
   */
  subscribeTimeoutMs?: number;
}

/**
 * Add a short cause hint to known subscription failures.
 * The executor needs its main key to register a live query; on a restarted
 * executor whose wallet was never unlocked this surfaces as a bare 403.
 */
export function hintFor(msg: string): string {
  // "Executor is locked" is the current executor's message; "main key not
  // found" is what older executors (and unrelated key lookups) say for the
  // same locked state. Either way the caller cannot fix it themselves: keys
  // live in memory only, so every restart locks the node again and only its
  // operator can unlock it.
  if (/executor is locked/i.test(msg) || /main key not found/i.test(msg)) {
    return " — the executor is locked (its keys live in memory only, so every restart locks it again). Only the node's operator can unlock it: contact them and ask for unlockAgent, then subscribe again — a pending subscription re-attempts on its own once the node is unlocked";
  }
  if (/timed out after \d+ms/i.test(msg)) {
    return " — the executor accepted the waker's connection but never answered; check that the waker is pointed at the right executor host and API port (executorUrl in the plugin config) and that the node is unlocked";
  }
  return "";
}

/**
 * Reject `promise` after `ms` instead of waiting forever.
 *
 * The pending set only catches rejections. A handshake that hangs — the
 * executor accepted the socket but never answers — settles nothing, so the
 * subscription was recorded nowhere and the caller's tool invocation died at
 * the harness timeout instead (#1016). The underlying promise is left as is:
 * the caller disposes the proxy, so a late answer backs out on its own.
 */
async function withTimeout<T>(promise: Promise<T>, ms: number, message: string): Promise<T> {
  let timer: ReturnType<typeof setTimeout> | undefined;
  try {
    return await Promise.race([
      promise,
      new Promise<never>((_resolve, reject) => {
        timer = setTimeout(() => reject(new Error(message)), ms);
      }),
    ]);
  } finally {
    if (timer) clearTimeout(timer);
  }
}

export class WakerSubscriptionManager {
  private perspectiveClient: any;
  private logger: WakerLogger;
  private debounceMs: number;
  private ancestorDepth: number;
  private onWake: (sub: WakerSubscription, result: any, mentions?: MentionMessage[]) => void;
  private onPersist?: (subscriptions: WakerSubscription[], seenMessages: Record<string, string[]>) => void;
  private QuerySubscriptionProxyCtor: any;
  private previousSeenMessages: Record<string, string[]>;
  private retryPendingMs: number;
  private subscribeTimeoutMs: number;

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
    this.ancestorDepth = options.ancestorDepth ?? 3;
    this.onWake = options.onWake;
    this.onPersist = options.onPersist;
    this.QuerySubscriptionProxyCtor = options.QuerySubscriptionProxy ?? null;
    this.previousSeenMessages = options.previousSeenMessages ?? {};
    this.retryPendingMs = options.retryPendingMs ?? 30000;
    this.subscribeTimeoutMs = options.subscribeTimeoutMs ?? 15000;
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
   * Create a live subscription: SPARQL, or a model query when `sub.className` is set.
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
    if (sub.type === "mention" && sub.className && !sub.terms?.length) {
      // Without terms the whole-word check cannot run, and every substring
      // ("metadata" for "data") would wake the agent.
      throw new Error(
        `Waker subscription ${sub.id}: a mention subscription needs its terms (get_mention_waker_config returns them)`,
      );
    }

    this.logger.info(
      `[waker] ${sub.id}: creating subscription (perspective=${sub.perspective}, type=${sub.type})`,
    );
    this.logger.info(`[waker] ${sub.id}: query${sub.className ? ` over ${sub.className}` : ""}:\n${sub.query}`);

    const proxy = new QuerySubscriptionProxy(
      sub.perspective,
      sub.query,
      this.perspectiveClient,
      sub.className,
    );
    // Suppress unhandled rejection from proxy.initialized — QuerySubscriptionProxy
    // rejects this promise internally when subscribe() fails, and if nobody catches
    // it before the next microtask it crashes the process.
    if (proxy.initialized && typeof proxy.initialized.catch === "function") {
      proxy.initialized.catch(() => {});
    }
    // One deadline for the whole handshake: `proxy.subscribe()` and the
    // `initialized` promise it resolves share the budget, so a hang in either
    // surfaces as the same loud-but-recoverable failure a rejection does.
    const budgetMs = this.subscribeTimeoutMs;
    const startedAt = Date.now();
    const bounded = <T>(step: Promise<T>, what: string): Promise<T> =>
      budgetMs > 0
        ? withTimeout(
            step,
            Math.max(0, budgetMs - (Date.now() - startedAt)),
            `${what} timed out after ${budgetMs}ms`,
          )
        : step;
    try {
      await bounded(proxy.subscribe(), "subscribe handshake");
      if (this.isStale(sub.id, token)) {
        try { proxy.dispose(); } catch {}
        return;
      }
      await bounded(proxy.initialized, "subscription initialization");
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
      // mid-restart, a handshake the executor never answered) clears on its
      // own, and the caller asked to be enrolled.
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

      // A mention subscription is a model query (`{ instances }`), a
      // channel-messages one SPARQL rows. Anything else (e.g. `false` on a
      // disconnect/reconnect) is ignored.
      const items: unknown = sub.type === "mention" ? result?.instances : result;
      if (!Array.isArray(items)) {
        this.logger.warn(
          `[waker] ${sub.id}: ignoring result without ${sub.type === "mention" ? "instances" : "rows"}: ${JSON.stringify(result)}`,
        );
        return;
      }

      if (sub.type === "mention") {
        // For mention subscriptions: track seen record ids.
        // Only wake for records we haven't seen before.
        const currentSeen = this.seenMessages.get(sub.id) ?? new Set<string>();
        const newMessages: string[] = [];

        const terms = sub.terms ?? [];
        for (const item of items) {
          if (typeof item?.id !== "string" || currentSeen.has(item.id)) continue;
          // The query matched a substring; wake only on a whole word.
          const text = Array.isArray(item.text) ? item.text.join(" ") : item.text;
          if (typeof text !== "string" || !holdsAWholeWordTerm(text, terms)) {
            this.logger.debug(`[waker] ${sub.id}: ${item.id} holds no term as a whole word, skipping`);
            continue;
          }
          newMessages.push(item.id);
        }

        if (newMessages.length === 0) {
          this.logger.info(
            `[waker] ${sub.id}: no new whole-word mention among ${items.length} items, skipping`,
          );
          return;
        }

        this.logger.info(
          `[waker] ${sub.id}: ${newMessages.length} new message(s) out of ${items.length} total`,
        );

        // Resolve parents, and the chain above them, per new message
        const mentions: MentionMessage[] = [];
        for (const msgAddr of newMessages) {
          this.logger.info(`[waker] ${sub.id}: resolving parents for ${msgAddr}`);
          const parents = await this.parentsOf(sub, [msgAddr]);
          const ancestors = await this.ancestorsOf(sub, msgAddr, parents);
          this.logger.info(
            `[waker] ${sub.id}: message ${msgAddr} has ${parents.length} parent(s): ${parents.join(", ")}` +
              (ancestors.length ? `; ancestors: ${ancestors.join(", ")}` : ""),
          );
          mentions.push({ address: msgAddr, parents, ancestors });
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
          `[waker] ${sub.id}: channel update (${items.length} items)`,
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

  /**
   * One level up, in one model query: every `Container` whose `children` hold
   * any of `addresses` (#1446). A node no class describes is not a Container
   * and is not reached. Failures count as none.
   */
  private async parentsOf(sub: WakerSubscription, addresses: string[]): Promise<string[]> {
    if (!addresses.length) return [];
    const query = JSON.stringify({ where: { children: [...new Set(addresses)] }, properties: [] });
    try {
      const result = await this.perspectiveClient.modelQuery(sub.perspective, CONTAINER, query);
      const instances: unknown = result?.instances;
      if (!Array.isArray(instances)) return [];
      return instances.map((instance) => instance?.id).filter((id): id is string => typeof id === "string");
    } catch (err: any) {
      this.logger.warn(`[waker] ${sub.id}: parent resolution failed for ${JSON.stringify(addresses)} — ${err?.message ?? err}`);
      return [];
    }
  }

  /**
   * The chain above an item's parents, a level at a time, nearest first, up to `ancestorDepth`.
   *
   * Breadth-first with a visited set: containment is a graph, not a tree — an item can sit in two
   * places, and nothing stops a cycle — so each address is reported once and the walk always ends.
   * Each level uses one batched parent lookup, bounded by the depth.
   */
  private async ancestorsOf(sub: WakerSubscription, address: string, parents: string[]): Promise<string[]> {
    const seen = new Set<string>([address, ...parents]);
    const ancestors: string[] = [];
    let level = parents;
    for (let depth = 0; depth < this.ancestorDepth && level.length; depth++) {
      const next: string[] = [];
      for (const parent of await this.parentsOf(sub, level)) {
        if (seen.has(parent)) continue;
        seen.add(parent);
        next.push(parent);
      }
      ancestors.push(...next);
      level = next;
    }
    return ancestors;
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
