import type { OnlineAgent } from "./types.js";

interface AgentEntry {
  wsId: string;
  status?: unknown;
}

/**
 * Online/offline presence tracking, decoupled from the WebSocket transport
 * by design: this module owns grace-period timers and the in-memory agent
 * registry. The caller (ws.ts) supplies callbacks for what to broadcast,
 * avoiding a circular dependency between ws.ts and telepresence.ts.
 *
 * Presence data lives entirely in memory — if the server restarts, all
 * WebSocket connections drop anyway, so persisting to SQLite added I/O
 * without benefit.
 */
export class TelepresenceManager {
  private graceMs: number;
  private offlineTimers = new Map<string, NodeJS.Timeout>();
  /** roomId → did → agent entry */
  private agents = new Map<string, Map<string, AgentEntry>>();

  constructor(opts: { graceMs?: number } = {}) {
    this.graceMs = opts.graceMs ?? 5_000;
  }

  private key(roomId: string, did: string): string {
    return `${roomId}:${did}`;
  }

  /** Marks an agent online for a room and cancels any pending grace-period offline timer. */
  markOnline(roomId: string, did: string, wsId: string): void {
    this.cancelPendingOffline(roomId, did);
    let room = this.agents.get(roomId);
    if (!room) {
      room = new Map();
      this.agents.set(roomId, room);
    }
    const existing = room.get(did);
    room.set(did, { wsId, status: existing?.status });
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
      this.agents.get(roomId)?.delete(did);
      onExpired();
    }, this.graceMs);
    timer.unref();
    this.offlineTimers.set(k, timer);
  }

  setStatus(roomId: string, did: string, status: unknown): void {
    const entry = this.agents.get(roomId)?.get(did);
    if (entry) entry.status = status;
  }

  getOnlineAgents(roomId: string): OnlineAgent[] {
    const room = this.agents.get(roomId);
    if (!room) return [];
    const result: OnlineAgent[] = [];
    for (const [did, entry] of room) {
      result.push({ did, status: entry.status });
    }
    return result;
  }

  isOnline(roomId: string, did: string): boolean {
    return this.agents.get(roomId)?.has(did) === true;
  }

  close(): void {
    for (const timer of this.offlineTimers.values()) clearTimeout(timer);
    this.offlineTimers.clear();
    this.agents.clear();
  }
}
