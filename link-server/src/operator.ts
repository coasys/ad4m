import { createHash, timingSafeEqual } from "node:crypto";
import { readFileSync, statSync } from "node:fs";
import Fastify, { type FastifyInstance, type FastifyServerOptions } from "fastify";
import { didToPublicKey, type AuthManager } from "./auth.js";
import type { LinkServerDB } from "./db.js";
import type { LogEntry, LogTail } from "./log-tail.js";
import { OPERATOR_PAGE_CSS, OPERATOR_PAGE_HTML, OPERATOR_PAGE_JS } from "./operator-page.js";
import { removeMember } from "./routes.js";
import type { TelepresenceManager } from "./telepresence.js";
import type { WsManager } from "./ws.js";

/**
 * Operator admission listener: a second Fastify app, bound to loopback only,
 * that lets whoever runs this server list rooms and add or remove DIDs
 * without holding the room admin's DID key. It shares the database, sessions
 * and sockets with the public app, so a removal here ends access at once,
 * exactly like `POST /rooms/:roomId/acl`.
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
  ws: WsManager;
  telepresence: TelepresenceManager;
  /** The server's recent log lines; without it, `GET /api/logs` answers 404 (logging is off). */
  logTail?: LogTail;
}

/** Live log streams one listener serves at once; each holds a socket open. */
export const MAX_LOG_STREAMS = 8;

/** A comment line this often keeps proxies (nginx: 60 s read timeout) from closing an idle stream. */
const LOG_STREAM_HEARTBEAT_MS = 15_000;

/** Bytes a log stream may have queued for a slow client before it is dropped. */
const MAX_LOG_STREAM_BUFFER = 1024 * 1024;

/**
 * One Server-Sent Event. Its id is `<boot>-<seq>`, so a reconnect resumes
 * after it, and an id from before a restart (another boot) gets every line.
 */
function logEvent(boot: string, entry: LogEntry): string {
  return `id: ${boot}-${entry.seq}\ndata: ${JSON.stringify(entry)}\n\n`;
}

/** The sequence number to resume after: the one in `lastEventId` if it is from this boot, else 0. */
function resumeAfter(lastEventId: unknown, boot: string): number {
  const match = typeof lastEventId === "string" ? /^([0-9a-f]+)-(\d+)$/.exec(lastEventId) : null;
  return match && match[1] === boot ? Number(match[2]) : 0;
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

function isValidDid(did: unknown): did is string {
  if (typeof did !== "string") return false;
  try {
    didToPublicKey(did);
    return true;
  } catch {
    return false;
  }
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

function roomDetail(ctx: OperatorContext, roomId: string) {
  const room = ctx.db.getRoom(roomId);
  if (!room) return undefined;
  const online = new Set(ctx.telepresence.getOnlineAgents(roomId).map((a) => a.did));
  const history = ctx.db.getAclChanges(roomId);
  // Over the whole history, not just the last 100 changes shown.
  const addedBy = new Map(
    ctx.db.getLatestAdds(roomId).map((c) => [c.did, { actor: c.actor, source: c.source, at: c.at }])
  );
  return {
    id: room.id,
    admin: room.admin_did,
    e2e: !!room.e2e_enabled,
    createdAt: room.created_at,
    members: ctx.db.getAcl(roomId).map((a) => ({
      did: a.did,
      addedAt: a.added_at,
      hasX25519Key: !!a.x25519_public_key,
      online: online.has(a.did),
      addedBy: addedBy.get(a.did) ?? null,
    })),
    history: history.map((c) => ({ did: c.did, action: c.action, actor: c.actor, source: c.source, at: c.at })),
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

    api.get("/whoami", async (request) => ({ operator: operatorOf(request.headers), instance: opts.label ?? null }));

    // Live logs as Server-Sent Events: the buffered lines first, then each new
    // one. A reconnect sends Last-Event-ID and gets only what it missed.
    api.get("/logs", async (request, reply) => {
      const tail = ctx.logTail;
      if (!tail) return reply.code(404).send({ error: "logging is off on this server" });
      if (logStreams.size >= MAX_LOG_STREAMS) {
        return reply.code(429).send({ error: `at most ${MAX_LOG_STREAMS} log streams at once` });
      }
      const backlog = tail.since(resumeAfter(request.headers["last-event-id"], tail.boot));

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
      raw.write("retry: 3000\n\n" + backlog.map((entry) => logEvent(tail.boot, entry)).join(""));

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
        // end() would wait for it to read the queued megabyte first. It
        // reconnects and resumes from its last id.
        if (raw.writableLength > MAX_LOG_STREAM_BUFFER) {
          raw.destroy();
          end();
          return;
        }
        raw.write(logEvent(tail.boot, entry));
      });
      heartbeat = setInterval(() => raw.write(": ping\n\n"), LOG_STREAM_HEARTBEAT_MS);
      heartbeat.unref();
    });

    api.get("/rooms", async () => ({
      rooms: ctx.db.listRooms().map((r) => ({
        id: r.id,
        admin: r.admin_did,
        e2e: !!r.e2e_enabled,
        createdAt: r.created_at,
        memberCount: r.member_count,
      })),
    }));

    api.get("/rooms/:roomId", async (request, reply) => {
      const { roomId } = request.params as { roomId: string };
      const detail = roomDetail(ctx, roomId);
      if (!detail) return reply.code(404).send({ error: "no such room" });
      return detail;
    });

    api.post("/rooms/:roomId/acl", async (request, reply) => {
      const { roomId } = request.params as { roomId: string };
      const body = request.body as { action?: unknown; did?: unknown } | null;
      if (body?.action !== "add" && body?.action !== "remove") {
        return reply.code(400).send({ error: 'action must be "add" or "remove"' });
      }
      if (!isValidDid(body.did)) {
        return reply.code(400).send({ error: "did must be a did:key" });
      }
      // Rooms are created by their first agent's challenge-response; the
      // operator only manages rooms that exist.
      const room = ctx.db.getRoom(roomId);
      if (!room) return reply.code(404).send({ error: "no such room" });

      if (body.action === "remove" && body.did === room.admin_did) {
        return reply.code(400).send({ error: "the room admin cannot be removed" });
      }
      const changed = body.action === "remove" ? removeMember(ctx, roomId, body.did) : ctx.db.addAcl(roomId, body.did);
      // A double submit or a stale page changes nothing: no history row, no log line.
      if (changed) {
        const operator = operatorOf(request.headers)!;
        ctx.db.recordAclChange(roomId, body.did, body.action, operator, "operator");
        request.log.info(
          { operator, instance: opts.label ?? null, roomId, action: body.action, did: body.did },
          "operator acl change"
        );
      }
      return roomDetail(ctx, roomId);
    });
  }, { prefix: "/api" });

  return app;
}
