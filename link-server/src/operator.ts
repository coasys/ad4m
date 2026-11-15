import { createHash, timingSafeEqual } from "node:crypto";
import { readFileSync, statSync } from "node:fs";
import Fastify, { type FastifyInstance, type FastifyServerOptions } from "fastify";
import type { AuthManager } from "./auth.js";
import type { LinkServerDB, RoomRow } from "./db.js";
import type { LogEntry, LogTail } from "./log-tail.js";
import { OPERATOR_PAGE_CSS, OPERATOR_PAGE_HTML, OPERATOR_PAGE_JS } from "./operator-page.js";
import type { MembershipRooms } from "./membership.js";
import { admittedKeys } from "./room-events.js";
import type { TelepresenceManager } from "./telepresence.js";
import type { WsManager } from "./ws.js";

/**
 * Operator listener: a second Fastify app, bound to loopback only, that lets
 * whoever runs this server see its rooms (members, keys, who is online) and
 * suspend or resume a room. Who belongs to a room is the room's own signed
 * membership: the operator cannot add or remove anyone. It shares the
 * database, sessions and sockets with the public app, so a suspension ends
 * access at once.
 *
 * Every `/api/*` call needs `Authorization: Bearer <operator token>`. The
 * token comes from a file (see index.ts), never argv or a URL, and the
 * request log never contains headers. In a deployment, nginx signs the
 * human in (oauth2-proxy) and injects the token; see README "Admission UI".
 */
export interface OperatorOptions {
  /** The shared secret every /api request must carry as a Bearer token. */
  token: string;
  /**
   * Origins (scheme://host[:port]) the page is served from. Every POST must
   * carry an Origin, or failing that a Referer, from this list (CSRF guard:
   * the sign-in is a cookie). Required, at least one.
   */
  allowedOrigins: string[];
  /** Instance name shown on the page ("staging", "prod"); one listener serves one instance. */
  label?: string;
  /** Logger for this listener. Default: same as the public app. */
  logger?: FastifyServerOptions["logger"];
}

export interface OperatorContext {
  db: LinkServerDB;
  auth: AuthManager;
  rooms: MembershipRooms;
  ws: WsManager;
  telepresence: TelepresenceManager;
  /** The server's recent log lines; without it, `GET /api/logs` answers 404 (logging is off). */
  logTail?: LogTail;
  /** Whether the public app admits every authenticated key (else by membership). */
  autoAdmit: boolean;
}

/** Live log streams one listener serves at once; each holds a socket open. */
export const MAX_LOG_STREAMS = 8;

/** A comment line this often keeps proxies (nginx: 60 s read timeout) from closing an idle stream. */
const LOG_STREAM_HEARTBEAT_MS = 15_000;

/** Bytes a log stream may have queued for a slow client before it is dropped. */
const MAX_LOG_STREAM_BUFFER = 1024 * 1024;

/** One Server-Sent Event carrying one log line. */
function logEvent(entry: LogEntry): string {
  return `data: ${JSON.stringify(entry)}\n\n`;
}

export const MIN_OPERATOR_TOKEN_LENGTH = 32;

const LOOPBACK_HOSTS = new Set(["127.0.0.1", "::1", "localhost"]);

/** The operator listener may only bind to these: it is reached through a local reverse proxy. */
export function isLoopbackHost(host: string): boolean {
  return LOOPBACK_HOSTS.has(host);
}

/**
 * Reads the operator token from a file that only its owner may read (mode
 * 600 or 400). One trailing newline is not part of the token. Throws with a
 * message that never contains the file's content.
 */
export function readOperatorToken(file: string): string {
  const mode = statSync(file).mode & 0o777;
  if (mode & 0o077) {
    throw new Error(`operator token file ${file} has mode ${mode.toString(8)}; it must be 600 or 400`);
  }
  const token = readFileSync(file, "utf8").replace(/\r?\n$/, "");
  if (token.length < MIN_OPERATOR_TOKEN_LENGTH) {
    throw new Error(`operator token in ${file} is shorter than ${MIN_OPERATOR_TOKEN_LENGTH} characters`);
  }
  return token;
}

function digest(value: string): Uint8Array {
  return new Uint8Array(createHash("sha256").update(value).digest());
}

/** Constant-time check of an Authorization header against the operator token. */
function tokenMatches(header: string | undefined, expected: Uint8Array): boolean {
  if (!header || !header.startsWith("Bearer ")) return false;
  return timingSafeEqual(digest(header.slice("Bearer ".length)), expected);
}

/** GitHub login syntax: what oauth2-proxy passes on as the signed-in user. */
const LOGIN_PATTERN = /^[A-Za-z0-9](?:[A-Za-z0-9-]{0,38})$/;

/** The signed-in login nginx set from the oauth2-proxy subrequest, or null. */
function operatorOf(headers: Record<string, unknown>): string | null {
  const user = headers["x-forwarded-user"];
  return typeof user === "string" && LOGIN_PATTERN.test(user) ? user : null;
}

/** scheme://host[:port] of a URL, or null if it does not parse. */
function originOf(url: string): string | null {
  try {
    return new URL(url).origin;
  } catch {
    return null;
  }
}

/**
 * CSRF guard for state changes: the Origin header, or the Referer when a
 * browser sends no Origin, must name an allowed origin. A request with
 * neither is refused.
 */
function sameOriginRequest(headers: Record<string, unknown>, allowed: Set<string>): boolean {
  const origin = headers["origin"];
  // An opaque origin ("null": sandboxed frame, cross-origin redirect) is never the admin page.
  if (typeof origin === "string") return origin !== "null" && allowed.has(origin);
  const referer = headers["referer"];
  if (typeof referer === "string") {
    const o = originOf(referer);
    return o !== null && allowed.has(o);
  }
  return false;
}

function roomSummary(ctx: OperatorContext, room: RoomRow) {
  return {
    id: room.id,
    creator: room.admin_did,
    createdAt: room.created_at,
    legacy: room.legacy === 1,
    suspended: room.suspended === 1,
    memberCount: room.legacy ? 0 : ctx.rooms.members(room.id).length,
  };
}

function roomDetail(ctx: OperatorContext, roomId: string) {
  const room = ctx.db.getRoom(roomId);
  if (!room) return undefined;
  const online = new Set(ctx.telepresence.getOnlineAgents(roomId).map((a) => a.did));
  const keys = ctx.db.getMemberKeys(roomId);
  const admitted = new Set(room.legacy ? [] : admittedKeys(ctx, roomId).map((k) => k.did));
  const members = room.legacy ? [] : ctx.rooms.members(roomId);
  return {
    ...roomSummary(ctx, room),
    members: members.map((m) => ({
      person: m.person,
      role: m.role,
      botOf: m.botOf,
      since: new Date(m.since).toISOString(),
      online: keys.some((k) => online.has(k.did) && (k.did === m.person || (k.person === m.person && k.by_bot === 0))),
    })),
    keys: keys.map((k) => ({
      did: k.did,
      person: k.person,
      byBot: k.by_bot === 1,
      revoked: k.revoked === 1,
      admitted: admitted.has(k.did),
      online: online.has(k.did),
      updatedAt: k.updated_at,
    })),
  };
}

export async function buildOperatorApp(ctx: OperatorContext, opts: OperatorOptions): Promise<FastifyInstance> {
  if (opts.token.length < MIN_OPERATOR_TOKEN_LENGTH) {
    throw new Error(`operator token must be at least ${MIN_OPERATOR_TOKEN_LENGTH} characters`);
  }
  const expected = digest(opts.token);
  const allowedOrigins = new Set(opts.allowedOrigins.map((o) => originOf(o)).filter((o): o is string => !!o));
  if (allowedOrigins.size === 0) {
    throw new Error("operator listener needs at least one allowed origin (e.g. https://admin.example.org)");
  }
  const app = Fastify({ logger: opts.logger ?? false, bodyLimit: 16 * 1024 });

  // Open log streams never finish on their own; end them so close() does not wait on them.
  const logStreams = new Set<() => void>();
  app.addHook("preClose", async () => {
    for (const end of [...logStreams]) end();
  });

  app.addHook("onSend", async (_request, reply) => {
    reply.header("X-Content-Type-Options", "nosniff");
    reply.header("Cache-Control", "no-store");
  });

  // The page, its script and its styles carry no data; everything else needs the token.
  app.get("/", async (_request, reply) => {
    return reply
      .type("text/html; charset=utf-8")
      .header("Content-Security-Policy", "default-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'")
      .send(OPERATOR_PAGE_HTML);
  });
  app.get("/admin.js", async (_request, reply) => {
    return reply.type("text/javascript; charset=utf-8").send(OPERATOR_PAGE_JS);
  });
  app.get("/admin.css", async (_request, reply) => {
    return reply.type("text/css; charset=utf-8").send(OPERATOR_PAGE_CSS);
  });

  app.register(async (api) => {
    api.addHook("onRequest", async (request, reply) => {
      if (!tokenMatches(request.headers.authorization, expected)) {
        return reply.code(401).send({ error: "operator token required" });
      }
      // Browsers mark cross-site requests; a signed-in operator's cookie must
      // not let another site drive these endpoints.
      if (request.headers["sec-fetch-site"] === "cross-site") {
        return reply.code(403).send({ error: "cross-site request refused" });
      }
      // Every change is recorded against a person. This listener is loopback
      // only and reached through nginx, which sets the header from the
      // sign-in subrequest and never passes a client's own value through.
      if (!operatorOf(request.headers)) {
        return reply.code(401).send({ error: "signed-in user required (X-Forwarded-User)" });
      }
      if (request.method === "POST") {
        if (!sameOriginRequest(request.headers, allowedOrigins)) {
          return reply.code(403).send({ error: "Origin or Referer must be this admin page" });
        }
        if (!String(request.headers["content-type"] ?? "").startsWith("application/json")) {
          return reply.code(415).send({ error: "content-type must be application/json" });
        }
      }
    });

    api.get("/whoami", async (request) => ({
      operator: operatorOf(request.headers),
      instance: opts.label ?? null,
      admission: ctx.autoAdmit ? "auto" : "membership",
    }));

    // Live logs as Server-Sent Events: the buffered lines first, then each new
    // one. A reconnect gets the whole buffer again; the page starts over.
    api.get("/logs", async (_request, reply) => {
      const tail = ctx.logTail;
      if (!tail) return reply.code(404).send({ error: "logging is off on this server" });
      if (logStreams.size >= MAX_LOG_STREAMS) {
        return reply.code(429).send({ error: `at most ${MAX_LOG_STREAMS} log streams at once` });
      }

      reply.hijack();
      const raw = reply.raw;
      raw.writeHead(200, {
        "Content-Type": "text/event-stream; charset=utf-8",
        "Cache-Control": "no-store",
        "X-Content-Type-Options": "nosniff",
        // nginx buffers proxied responses unless told not to; a log stream must arrive line by line.
        "X-Accel-Buffering": "no",
        Connection: "keep-alive",
      });
      raw.write("retry: 3000\n\n" + tail.lines().map(logEvent).join(""));

      let unsubscribe = () => {};
      let heartbeat: NodeJS.Timeout | undefined;
      const end = () => {
        clearInterval(heartbeat);
        unsubscribe();
        logStreams.delete(end);
        if (!raw.writableEnded && !raw.destroyed) raw.end();
      };
      logStreams.add(end);
      raw.on("close", end);
      unsubscribe = tail.subscribe((entry) => {
        // A client this far behind is dropped: destroyed, not ended, since
        // end() would wait for it to read the queued megabyte first.
        if (raw.writableLength > MAX_LOG_STREAM_BUFFER) {
          raw.destroy();
          end();
          return;
        }
        raw.write(logEvent(entry));
      });
      heartbeat = setInterval(() => raw.write(": ping\n\n"), LOG_STREAM_HEARTBEAT_MS);
      heartbeat.unref();
    });

    api.get("/rooms", async () => ({
      rooms: ctx.db.listRooms().map((r) => roomSummary(ctx, r)),
    }));

    api.get("/rooms/:roomId", async (request, reply) => {
      const { roomId } = request.params as { roomId: string };
      const detail = roomDetail(ctx, roomId);
      if (!detail) return reply.code(404).send({ error: "no such room" });
      return detail;
    });

    // A suspended room answers 423 to every request and its sockets close (4006); resume undoes it.
    for (const action of ["suspend", "resume"] as const) {
      api.post(`/rooms/:roomId/${action}`, async (request, reply) => {
        const { roomId } = request.params as { roomId: string };
        const room = ctx.db.getRoom(roomId);
        if (!room) return reply.code(404).send({ error: "no such room" });
        const suspend = action === "suspend";
        if ((room.suspended === 1) !== suspend) {
          ctx.db.setSuspended(roomId, suspend);
          if (suspend) {
            ctx.db.deleteSessionsForRoom(roomId);
            ctx.ws.closeRoom(roomId, 4006, "room suspended");
          }
          request.log.info(
            { operator: operatorOf(request.headers), instance: opts.label ?? null, roomId, action },
            `operator ${action}`
          );
        }
        return roomDetail(ctx, roomId);
      });
    }
  }, { prefix: "/api" });

  return app;
}
