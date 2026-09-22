/**
 * Subscriber to the executor's `/api/v1/ws/events` event stream.
 *
 * The events WS is a single multiplexed connection per token; events
 * arrive as `{ type, ...payload }` JSON frames.  This subscriber opens
 * one connection per token, dispatches each frame to any registered
 * listener that matches the `type`, and gracefully closes on disconnect.
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
      this.ws.on("open", () => resolve());
      this.ws.on("error", (err) => reject(err));
      this.ws.on("message", (data) => {
        let frame: EventFrame;
        try {
          frame = JSON.parse(data.toString()) as EventFrame;
        } catch {
          return;
        }
        if (!frame || typeof frame.type !== "string") return;
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
      });
    });
    await this.ready;
  }

  on(eventType: string, handler: EventListener): () => void {
    let set = this.listenersByType.get(eventType);
    if (!set) {
      set = new Set();
      this.listenersByType.set(eventType, set);
    }
    set.add(handler);
    return () => {
      set!.delete(handler);
      if (set!.size === 0) {
        this.listenersByType.delete(eventType);
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
