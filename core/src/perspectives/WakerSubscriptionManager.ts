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
   * `ad4m://TextContent`; a children subscription has the built-in
   * `ad4m://Container`, from `generate_waker_query` (#1446). Without it
   * `query` is SPARQL.
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
 * The elements a message's HTML is made of. A `<` before any other name is text:
 * We stores plain text, where "<hey Data> can you look" is words.
 */
const ELEMENT =
  "a|abbr|b|bdi|bdo|blockquote|br|caption|center|cite|code|col|colgroup|dd|del|details|dfn|div|dl|dt|em|" +
  "figcaption|figure|font|h[1-6]|hr|i|img|ins|kbd|label|li|mark|ol|p|pre|q|s|samp|section|article|small|" +
  "span|strike|strong|sub|summary|sup|table|tbody|td|tfoot|th|thead|time|tr|u|ul|var|wbr|script|style";
/** An attribute: a name, then optionally `=` and a quoted value (which may hold `>`) or an unquoted one. */
const ATTRIBUTE = `\\s+[A-Za-z_:][-\\w:.]*(?:\\s*=\\s*(?:"[^"]*"|'[^']*'|[^\\s"'=<>\`]+))?`;
/**
 * Markup: a comment, a `<script>`/`<style>` element with its contents, or a tag of
 * a known element whose rest is a valid attribute list. "a < b" and
 * "if a <b then ask Data, else c -> d" are text: the comma is no attribute.
 */
const TAG = new RegExp(
  `<!--[\\s\\S]*?-->` +
    `|<(script|style)(?=[\\s/>])(?:${ATTRIBUTE})*\\s*>[\\s\\S]*?<\\/\\1\\s*>` +
    `|<(?:${ELEMENT})(?=[\\s/>])(?:${ATTRIBUTE})*\\s*\\/?>` +
    `|<\\/(?:${ELEMENT})\\s*>`,
  "gi",
);
/** Tags that break a line: replaced by a space, so `<p>hi</p><p>Data</p>` keeps "Data" a word. */
const BREAKING_TAG = /^<\/?(?:p|div|br|hr|li|ul|ol|h[1-6]|blockquote|pre|tr|td|th|table|section|article)\b/i;
/**
 * The attributes a mention puts its target's DID in: Flux's `<a data-type="mention"
 * href="did:…">`, older Flux messages' `<span data-id="did:…">`, and We's editor chip
 * `<span class="we-mention" data-did="did:…">`.
 */
const ATTRIBUTES = new RegExp(ATTRIBUTE, "g");
const MENTION_TARGET = /^\s+(?:href|data-id|data-did)\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s"'=<>`]+))$/i;

/**
 * How much of a record's text the word check reads. Dropping markup costs time
 * quadratic in a run of unclosed `<script>`, `<style>` or `<!--` openers, so a
 * very long text could hold the event loop for seconds, and a row that holds
 * no term is checked again on every result while it is in the window. A term
 * past this many characters does not wake.
 */
export const MENTION_TEXT_LIMIT = 16_384;

const ENTITIES: Record<string, string> = { amp: "&", lt: "<", gt: ">", quot: '"', apos: "'", nbsp: " " };

function decodeEntities(s: string): string {
  return s.replace(/&(#x[0-9a-f]+|#[0-9]+|[a-z]+);/gi, (whole, ref: string) => {
    if (ref[0] !== "#") return ENTITIES[ref.toLowerCase()] ?? whole;
    const code = ref[1] === "x" || ref[1] === "X" ? parseInt(ref.slice(2), 16) : parseInt(ref.slice(1), 10);
    return code > 0 && code <= 0x10ffff ? String.fromCodePoint(code) : whole;
  });
}

/**
 * What a reader sees of `text`, and the DIDs its mentions point at. `text` may be
 * HTML (TextContent allows it): tag names and attributes are markup, not words, so
 * a bot named "Data" must not wake on `data-type="mention"` in a mention of
 * someone else. Plain text passes through unchanged, apart from entities.
 */
function readableParts(text: string): { visible: string; targets: string[] } {
  const targets: string[] = [];
  const visible = text.replace(TAG, (tag, scriptOrStyle?: string) => {
    if (scriptOrStyle) return " ";
    if (tag.startsWith("<!--")) return "";
    // Parse complete attributes first: a DID-shaped string inside `title`
    // is not a target attribute and must not wake an agent.
    for (const attribute of tag.matchAll(ATTRIBUTES)) {
      const target = MENTION_TARGET.exec(attribute[0]);
      if (target) targets.push(decodeEntities(target[1] ?? target[2] ?? target[3]));
    }
    return BREAKING_TAG.test(tag) ? " " : "";
  });
  return { visible: decodeEntities(visible), targets };
}

/**
 * Whether `text` holds one of `terms` as a whole word, case-insensitively:
 * "Data" is in "hi Data!", "@data" and "Data's turn", not in "metadata" or
 * "database". A term is matched literally (regex characters, quotes) and must
 * not touch a letter, mark, digit or underscore on either side, so a name
 * that starts or ends with punctuation still matches. Same rule as #1459's
 * SPARQL `(?:^|\W)term(?:\W|$)`.
 *
 * The rule runs on the visible text only: in HTML, tags and attributes are
 * dropped and entities decoded first. A mention's target attribute
 * (`href`, `data-id`, `data-did`) matches an exact, case-sensitive DID term, which
 * is how a mention by DID is found when the DID is not in the visible text.
 *
 * The mention query's `contains` matches substrings (model query has no word
 * match); this is the check that keeps "data" from waking on "metadata".
 * It reads the first {@link MENTION_TEXT_LIMIT} characters only.
 */
export function holdsAWholeWordTerm(text: string, terms: string[]): boolean {
  const { visible, targets } = readableParts(text.slice(0, MENTION_TEXT_LIMIT));
  return terms.some((term) => {
    if (!term) return false;
    if (term.startsWith("did:") && targets.includes(term)) return true;
    const escaped = term.replace(/[\\^$.*+?()[\]{}|\/]/g, "\\$&");
    return new RegExp(`(?<!${WORD_CHAR})${escaped}(?!${WORD_CHAR})`, "iu").test(visible);
  });
}

/** The built-in `Container` (`Container.builtinUri`; not imported, so the
 *  plugin bundle does not pull in the model layer). */
const CONTAINER = "ad4m://Container";

/** Per-message parent resolution result for mention subscriptions. */
export interface MentionMessage {
  /** The mentioned record's id. */
  address: string;
  /**
   * Every container holding it (channels, threads, calls, blocks): whatever
   * links it with `ad4m://has_child`, and every instance of a class whose
   * `children` relation links it. A record can sit in several.
   */
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
    this.logger.info(`[waker] ${sub.id}: ${sub.className ? `model query over ${sub.className}` : "SPARQL query"}:\n${sub.query}`);

    const proxy = sub.className
      ? new QuerySubscriptionProxy(sub.perspective, sub.query, this.perspectiveClient, sub.className)
      : new QuerySubscriptionProxy(sub.perspective, sub.query, this.perspectiveClient);
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

      // A model subscription delivers `{ instances }`, SPARQL an array of
      // rows. Anything else (e.g. `false` on a disconnect/reconnect) is
      // ignored.
      const rows: unknown = sub.className ? result?.instances : result;
      if (!Array.isArray(rows)) {
        this.logger.warn(
          `[waker] ${sub.id}: ignoring result without ${sub.className ? "instances" : "rows"}: ${JSON.stringify(result)}`,
        );
        return;
      }

      if (sub.type === "mention") {
        // For mention subscriptions: track seen record ids.
        // Only wake for records we haven't seen before.
        const currentSeen = this.seenMessages.get(sub.id) ?? new Set<string>();
        const newMessages: string[] = [];

        const terms = sub.terms ?? [];
        for (const item of rows as any[]) {
          // A SPARQL row names the record as `source`, a model row as `id`.
          const address = sub.className ? item?.id : item?.source;
          if (typeof address !== "string" || !address || currentSeen.has(address)) continue;
          if (terms.length) {
            // The mention query matched a substring; wake only on a whole word.
            const text = Array.isArray(item.text) ? item.text.join(" ") : item.text;
            if (typeof text !== "string" || !holdsAWholeWordTerm(text, terms)) {
              this.logger.debug(`[waker] ${sub.id}: ${address} holds no term as a whole word, skipping`);
              continue;
            }
          }
          currentSeen.add(address);
          newMessages.push(address);
        }

        if (newMessages.length === 0) {
          this.logger.info(
            `[waker] ${sub.id}: no new mention among ${rows.length} items, skipping`,
          );
          return;
        }

        this.logger.info(
          `[waker] ${sub.id}: ${newMessages.length} new message(s) out of ${rows.length} total`,
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
          `[waker] ${sub.id}: channel update (${rows.length} items)`,
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
   * Everything holding any of `addresses`, in one model query: a `Container`
   * step in (#1446). The executor follows `ad4m://has_child` on any node and
   * each class's own `children` relation on its instances, so the waker
   * looks up no predicate. Failures count as none.
   */
  private async parentsOf(sub: WakerSubscription, addresses: string[]): Promise<string[]> {
    if (!addresses.length) return [];
    return this.containers(sub, { ids: [...new Set(addresses)], direction: "in" }, addresses);
  }

  /**
   * The chain above an item's parents, nearest first, up to `ancestorDepth` levels: one
   * transitive `Container` walk in from the parents, bounded by `maxDepth`. The executor walks
   * containment as a graph, so each address comes once however many paths reach it, and a
   * cycle ends. The item and its parents are never reported as ancestors.
   */
  private async ancestorsOf(sub: WakerSubscription, address: string, parents: string[]): Promise<string[]> {
    if (!parents.length || this.ancestorDepth <= 0) return [];
    const scope = { ids: parents, direction: "in", transitive: true, maxDepth: this.ancestorDepth };
    const seen = new Set<string>([address, ...parents]);
    return (await this.containers(sub, scope, parents)).filter((id) => !seen.has(id));
  }

  /** The ids of the `Container` rows `parent` scopes to, in the executor's order. */
  private async containers(sub: WakerSubscription, parent: Record<string, unknown>, from: string[]): Promise<string[]> {
    try {
      const query = JSON.stringify({ parent, properties: [] });
      const result = await this.perspectiveClient.modelQuery(sub.perspective, CONTAINER, query);
      const instances: unknown = result?.instances;
      if (!Array.isArray(instances)) return [];
      return instances.map((row) => row?.id).filter((id): id is string => typeof id === "string");
    } catch (err: any) {
      this.logger.warn(`[waker] ${sub.id}: containment lookup failed for ${JSON.stringify(from)} — ${err?.message ?? err}`);
      return [];
    }
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
