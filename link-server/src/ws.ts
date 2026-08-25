import { randomUUID } from "node:crypto";
import type { FastifyInstance, FastifyRequest } from "fastify";
import type { WebSocket } from "ws";
import type { AuthManager } from "./auth.js";
import type { TelepresenceManager } from "./telepresence.js";
import type { ClientWsMessage, RoomParams, ServerWsMessage } from "./types.js";

interface Connection {
  id: string;
  did: string;
  roomId: string;
  socket: WebSocket;
}

/**
 * Owns the live WebSocket connection registry, room presence broadcast, and
 * telepresence-signal routing. Depends only on AuthManager + TelepresenceManager
 * (one-directional) — routes.ts and federation.ts call into this to push
 * server-originated events, but this module never imports them, so there's
 * no import cycle.
 */
export class WsManager {
  private auth: AuthManager;
  private telepresence: TelepresenceManager;
  // roomId -> did -> connections for that agent (multiple devices allowed)
  private rooms = new Map<string, Map<string, Set<Connection>>>();
  private byId = new Map<string, Connection>();

  constructor(auth: AuthManager, telepresence: TelepresenceManager) {
    this.auth = auth;
    this.telepresence = telepresence;
  }

  register(app: FastifyInstance): void {
    app.get(
      "/rooms/:roomId/ws",
      {
        websocket: true,
        preValidation: async (request: FastifyRequest, reply) => {
          const { roomId } = request.params as RoomParams;
          const token = (request.query as Record<string, string | undefined>)?.token;
          const result = await this.auth.authenticate(token, roomId);
          if (!result.ok) {
            reply.code(result.status).send({ error: result.error });
            return;
          }
          request.authClaims = { did: result.did, roomId: result.roomId, token: result.token };
        },
      },
      (socket: WebSocket, request: FastifyRequest) => {
        const claims = request.authClaims!;
        this.handleConnection(socket, claims.roomId, claims.did);
      }
    );
  }

  private handleConnection(socket: WebSocket, roomId: string, did: string): void {
    const id = randomUUID();
    const conn: Connection = { id, did, roomId, socket };
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

    socket.on("message", (raw: Buffer | ArrayBuffer | Buffer[]) => {
      this.handleMessage(conn, raw);
    });
    socket.on("close", () => this.handleClose(conn));
    socket.on("error", () => {
      // 'close' fires after 'error' for ws sockets; cleanup happens there.
    });
  }

  private handleMessage(conn: Connection, raw: Buffer | ArrayBuffer | Buffer[]): void {
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
          type: "online-agents",
          agents: this.telepresence.getOnlineAgents(conn.roomId),
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
