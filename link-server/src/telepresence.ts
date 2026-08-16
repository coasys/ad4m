import type { LinkServerDB } from "./db.js";
import type { OnlineAgent } from "./types.js";

/**
 * Online/offline presence tracking, decoupled from the WebSocket transport
 * by design: this module owns grace-period timers and DB persistence only.
 * The caller (ws.ts) supplies callbacks for what to broadcast, avoiding a
 * circular dependency between ws.ts and telepresence.ts.
 */
export class TelepresenceManager {
  private db: LinkServerDB;
  private graceMs: number;
  private offlineTimers = new Map<string, NodeJS.Timeout>();

  constructor(db: LinkServerDB, opts: { graceMs?: number } = {}) {
    this.db = db;
    this.graceMs = opts.graceMs ?? 5_000;
  }

  private key(roomId: string, did: string): string {
    return `${roomId}:${did}`;
  }

  /** Marks an agent online for a room and cancels any pending grace-period offline timer. */
  markOnline(roomId: string, did: string, wsId: string): void {
    this.cancelPendingOffline(roomId, did);
    this.db.upsertOnline(roomId, did, wsId);
  }

  cancelPendingOffline(roomId: string, did: string): void {
    const k = this.key(roomId, did);
    const timer = this.offlineTimers.get(k);
    if (timer) {
      clearTimeout(timer);
      this.offlineTimers.delete(k);
    }
  }

  /**
   * Called when an agent's last WebSocket in a room disconnects. If no
   * reconnect (markOnline) cancels the timer within the grace period, the
   * agent is marked offline and `onExpired` fires so the caller can
   * broadcast `peer-left`.
   */
  scheduleOffline(roomId: string, did: string, onExpired: () => void): void {
    const k = this.key(roomId, did);
    this.cancelPendingOffline(roomId, did);
    const timer = setTimeout(() => {
      this.offlineTimers.delete(k);
      this.db.setOffline(roomId, did);
      onExpired();
    }, this.graceMs);
    timer.unref();
    this.offlineTimers.set(k, timer);
  }

  setStatus(roomId: string, did: string, status: unknown): void {
    this.db.setOnlineStatus(roomId, did, status);
  }

  getOnlineAgents(roomId: string): OnlineAgent[] {
    return this.db.getOnlineAgents(roomId).map((row) => ({
      did: row.did,
      status: row.status ? safeParse(row.status) : undefined,
    }));
  }

  isOnline(roomId: string, did: string): boolean {
    return this.db.getOnlineAgent(roomId, did) !== undefined;
  }

  close(): void {
    for (const timer of this.offlineTimers.values()) clearTimeout(timer);
    this.offlineTimers.clear();
  }
}

function safeParse(json: string): unknown {
  try {
    return JSON.parse(json);
  } catch {
    return json;
  }
}
