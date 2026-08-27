import { randomUUID } from "node:crypto";
import type { FastifyInstance, FastifyRequest } from "fastify";
import type { WebSocket } from "ws";
import type { AuthManager } from "./auth.js";
import { SlidingWindowLimiter } from "./rate-limit.js";
import type { TelepresenceManager } from "./telepresence.js";
import type { ClientWsMessage, RoomParams, ServerWsMessage } from "./types.js";

interface Connection {
  id: string;
  did: string;
  roomId: string;
  socket: WebSocket;
  limiter: SlidingWindowLimiter;
}

export interface WsManagerOptions {
  /** Max inbound WS messages per connection per window. Default 100. */
  wsMessageLimit?: number;
  /** Sliding window size in ms for WS message rate limiting. Default 60 000. */
  wsMessageWindowMs?: number;
  /** Seconds to wait for the auth message before closing. Default 5. */
  authTimeoutMs?: number;
}

/**
 * Owns the live WebSocket connection registry, room presence broadcast, and
 * telepresence-signal routing. Depends only on AuthManager + TelepresenceManager
 * (one-directional) — routes.ts and federation.ts call into this to push
 * server-originated events, but this module never imports them, so there's
 * no import cycle.
 *
 * Auth: first-message pattern — the WebSocket upgrade succeeds without
 * credentials; the client must send `{type:"auth",token:"<jwt>"}` as its
 * first message within `authTimeoutMs`. This keeps JWTs out of query
 * parameters (which appear in access logs, CDN caches, and browser history).
 */
export class WsManager {
  private auth: AuthManager;
  private telepresence: TelepresenceManager;
  private wsMessageLimit: number;
  private wsMessageWindowMs: number;
  private authTimeoutMs: number;
  // roomId -> did -> connections for that agent (multiple devices allowed)
  private rooms = new Map<string, Map<string, Set<Connection>>>();
  private byId = new Map<string, Connection>();

  constructor(auth: AuthManager, telepresence: TelepresenceManager, opts?: WsManagerOptions) {
    this.auth = auth;
    this.telepresence = telepresence;
    this.wsMessageLimit = opts?.wsMessageLimit ?? 100;
    this.wsMessageWindowMs = opts?.wsMessageWindowMs ?? 60_000;
    this.authTimeoutMs = opts?.authTimeoutMs ?? 5_000;
  }

  register(app: FastifyInstance): void {
    app.get(
      "/rooms/:roomId/ws",
      { websocket: true },
      (socket: WebSocket, request: FastifyRequest) => {
        const { roomId } = request.params as RoomParams;
        this.handleUnauthenticated(socket, roomId);
      }
    );
  }

  /** Accepts the raw socket and waits for an auth message before registering. */
  private handleUnauthenticated(socket: WebSocket, roomId: string): void {
    const timeout = setTimeout(() => {
      this.sendRaw(socket, { type: "auth-error", error: "auth timeout" });
      socket.close(4001, "auth timeout");
    }, this.authTimeoutMs);

    const onMessage = async (raw: Buffer | ArrayBuffer | Buffer[]) => {
      clearTimeout(timeout);
      socket.removeListener("message", onMessage);
      let msg: ClientWsMessage;
      try {
        msg = JSON.parse(raw.toString());
      } catch {
        this.sendRaw(socket, { type: "auth-error", error: "malformed JSON" });
        socket.close(4002, "malformed auth");
        return;
      }
      if (!msg || typeof msg !== "object" || msg.type !== "auth" || typeof (msg as { token?: unknown }).token !== "string") {
        this.sendRaw(socket, { type: "auth-error", error: "first message must be {type:'auth',token:'...'}" });
        socket.close(4003, "expected auth message");
        return;
      }
      const result = await this.auth.authenticate((msg as { token: string }).token, roomId);
      if (!result.ok) {
        this.sendRaw(socket, { type: "auth-error", error: result.error });
        socket.close(4004, result.error);
        return;
      }
      this.handleConnection(socket, roomId, result.did);
    };

    socket.on("message", onMessage);
    socket.on("close", () => clearTimeout(timeout));
  }

  private sendRaw(socket: WebSocket, msg: ServerWsMessage): void {
    if (socket.readyState === socket.OPEN) {
      socket.send(JSON.stringify(msg));
    }
  }

  private handleConnection(socket: WebSocket, roomId: string, did: string): void {
    const id = randomUUID();
    const limiter = new SlidingWindowLimiter(this.wsMessageLimit, this.wsMessageWindowMs);
    const conn: Connection = { id, did, roomId, socket, limiter };
    this.byId.set(id, conn);

    let room = this.rooms.get(roomId);
    if (!room) {
      room = new Map();
      this.rooms.set(roomId, room);
    }
    let didConns = room.get(did);
    const wasOffline = !didConns || didConns.size === 0;
    if (!didConns) {
      didConns = new Set();
      room.set(did, didConns);
    }
    didConns.add(conn);

    this.telepresence.markOnline(roomId, did, id);

    if (wasOffline) {
      // Exclude the connecting socket itself: it learns of its own presence
      // via the online-agents snapshot sent below, not a peer-joined about itself.
      this.broadcast(roomId, { type: "peer-joined", did }, { excludeConnId: conn.id });
    }
    this.sendTo(conn, {
      type: "online-agents",
      agents: this.telepresence.getOnlineAgents(roomId),
    });

    // Ping/pong keepalive: detect half-open TCP connections that the OS
    // hasn't noticed yet (NAT rebind, mobile network switch, etc.). The
    // `ws` library responds to ping frames with pong automatically on the
    // client side; we just need to send pings and kill the socket if no
    // pong comes back within one interval.
    let alive = true;
    socket.on("pong", () => { alive = true; });
    const pingTimer = setInterval(() => {
      if (!alive) {
        // No pong since last ping — connection is dead.
        clearInterval(pingTimer);
        socket.terminate();
        return;
      }
      alive = false;
      socket.ping();
    }, 30_000);
    pingTimer.unref();

    socket.on("message", (raw: Buffer | ArrayBuffer | Buffer[]) => {
      this.handleMessage(conn, raw);
    });
    socket.on("close", () => {
      clearInterval(pingTimer);
      this.handleClose(conn);
    });
    socket.on("error", () => {
      // 'close' fires after 'error' for ws sockets; cleanup happens there.
    });
  }

  private handleMessage(conn: Connection, raw: Buffer | ArrayBuffer | Buffer[]): void {
    if (!conn.limiter.check(conn.id).allowed) {
      // Drop the message silently — the client can retry after the window slides.
      return;
    }
    let msg: ClientWsMessage;
    try {
      msg = JSON.parse(raw.toString());
    } catch {
      return;
    }
    if (!msg || typeof msg !== "object" || typeof msg.type !== "string") return;

    switch (msg.type) {
      case "telepresence-signal": {
        if (typeof msg.toDid !== "string") return;
        this.routeSignal(conn.roomId, conn.did, msg.toDid, msg.payload);
        break;
      }
      case "telepresence-broadcast": {
        this.broadcast(
          conn.roomId,
          { type: "telepresence-broadcast", fromDid: conn.did, payload: msg.payload },
          { excludeConnId: conn.id }
        );
        break;
      }
      case "set-online-status": {
        this.telepresence.setStatus(conn.roomId, conn.did, msg.status);
        this.broadcast(conn.roomId, {
          type: "status-changed",
          did: conn.did,
          status: msg.status,
        });
        break;
      }
      default:
        break;
    }
  }

  private routeSignal(roomId: string, fromDid: string, toDid: string, payload: unknown): void {
    const targets = this.rooms.get(roomId)?.get(toDid);
    if (!targets) return;
    for (const conn of targets) {
      this.sendTo(conn, { type: "telepresence-signal", fromDid, payload });
    }
  }

  private handleClose(conn: Connection): void {
    this.byId.delete(conn.id);
    this.telepresence.markConnectionClosed(conn.roomId, conn.did, conn.id);
    const room = this.rooms.get(conn.roomId);
    const didConns = room?.get(conn.did);
    if (!didConns) return;
    didConns.delete(conn);
    if (didConns.size === 0) {
      room!.delete(conn.did);
      this.telepresence.scheduleOffline(conn.roomId, conn.did, () => {
        this.broadcast(conn.roomId, { type: "peer-left", did: conn.did });
      });
    }
  }

  sendTo(conn: Connection, msg: ServerWsMessage): void {
    if (conn.socket.readyState === conn.socket.OPEN) {
      conn.socket.send(JSON.stringify(msg));
    }
  }

  /** Broadcasts to every connection in a room, with optional exclusion of one connection or one DID's connections. */
  broadcast(
    roomId: string,
    msg: ServerWsMessage,
    opts: { excludeConnId?: string; excludeDid?: string } = {}
  ): void {
    const room = this.rooms.get(roomId);
    if (!room) return;
    for (const [did, conns] of room) {
      if (opts.excludeDid && did === opts.excludeDid) continue;
      for (const conn of conns) {
        if (opts.excludeConnId && conn.id === opts.excludeConnId) continue;
        this.sendTo(conn, msg);
      }
    }
  }

  hasConnections(roomId: string): boolean {
    const room = this.rooms.get(roomId);
    return !!room && room.size > 0;
  }

  closeAll(): void {
    for (const conn of this.byId.values()) {
      try {
        conn.socket.close();
      } catch {
        // best-effort on shutdown
      }
    }
    this.byId.clear();
    this.rooms.clear();
  }
}
