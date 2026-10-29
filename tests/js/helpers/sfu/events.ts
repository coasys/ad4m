/**
 * Subscriber to the executor's `/api/v1/ws/events` event stream.
 *
 * The events WS is a single multiplexed connection per token; events
 * arrive as `{ type, ...payload }` JSON frames.  This subscriber opens
 * one connection per token, dispatches each frame to any registered
 * listener that matches the `type`, and gracefully closes on disconnect.
 *
 * The executor sends a socket only the event types it asked for with
 * `events.watch`.  Each change to the registered types sends a fresh
 * watch; `watchApplied()` resolves once the executor applied the latest.
 *
 * Used by SFU scenarios to listen for `sfu-call-renegotiation-offer`
 * events targeted at their per-peer DID.
 */

import WebSocket from "ws";

export type EventFrame = { type: string; [k: string]: unknown };
export type EventListener = (frame: EventFrame) => void;

export interface EventsClientConfig {
  port: number;
  host?: string;
  token: string;
}

export class EventsClient {
  private ws: WebSocket | null = null;
  private ready: Promise<void> | null = null;
  private listenersByType = new Map<string, Set<EventListener>>();
  private watchId = 0;
  private watchReplies = new Map<string, () => void>();
  private lastWatch: Promise<void> = Promise.resolve();

  constructor(public readonly config: EventsClientConfig) {}

  get wsUrl(): string {
    const host = this.config.host ?? "127.0.0.1";
    return `ws://${host}:${this.config.port}/api/v1/ws/events?token=${encodeURIComponent(
      this.config.token,
    )}`;
  }

  async connect(): Promise<void> {
    this.ready = new Promise((resolve, reject) => {
      this.ws = new WebSocket(this.wsUrl);
      this.ws.on("open", () => {
        resolve();
        this.sendWatch();
      });
      this.ws.on("error", (err) => reject(err));
      this.ws.on("message", (data) => {
        let frame: EventFrame & { id?: string; error?: { message?: string } };
        try {
          frame = JSON.parse(data.toString());
        } catch {
          return;
        }
        if (!frame) return;
        if (typeof frame.type !== "string") {
          // The reply to an `events.watch`.
          if (frame.error) console.warn("[events] events.watch failed:", frame.error.message);
          const done = frame.id !== undefined ? this.watchReplies.get(String(frame.id)) : undefined;
          if (done) {
            this.watchReplies.delete(String(frame.id));
            done();
          }
          return;
        }
        const handlers = this.listenersByType.get(frame.type);
        if (handlers) {
          for (const h of handlers) {
            try {
              h(frame);
            } catch (e) {
              console.warn(`[events] handler for '${frame.type}' threw:`, e);
            }
          }
        }
      });
      this.ws.on("close", () => {
        // Best-effort: handlers can re-subscribe on reconnect if they
        // care, the wind tunnel scenarios don't need durability.
        for (const done of this.watchReplies.values()) done();
        this.watchReplies.clear();
      });
    });
    await this.ready;
  }

  /** Ask the executor for every event type that has a listener. */
  private sendWatch(): void {
    const ws = this.ws;
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    const params: Record<string, null> = {};
    for (const type of this.listenersByType.keys()) params[type] = null;
    const id = `watch-${++this.watchId}`;
    this.lastWatch = new Promise<void>((resolve) => this.watchReplies.set(id, resolve));
    ws.send(JSON.stringify({ id, type: "events.watch", params }));
  }

  /** Resolves once the executor applied the latest `events.watch`. */
  watchApplied(): Promise<void> {
    return this.lastWatch;
  }

  on(eventType: string, handler: EventListener): () => void {
    let set = this.listenersByType.get(eventType);
    if (!set) {
      set = new Set();
      this.listenersByType.set(eventType, set);
      this.sendWatch();
    }
    set.add(handler);
    return () => {
      set!.delete(handler);
      if (set!.size === 0) {
        this.listenersByType.delete(eventType);
        this.sendWatch();
      }
    };
  }

  async disconnect(): Promise<void> {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
    this.listenersByType.clear();
  }
}
