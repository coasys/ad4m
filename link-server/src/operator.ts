import { createHash, timingSafeEqual } from "node:crypto";
import { readFileSync, statSync } from "node:fs";
import Fastify, { type FastifyInstance, type FastifyServerOptions } from "fastify";
import { didToPublicKey, type AuthManager } from "./auth.js";
import type { LinkServerDB } from "./db.js";
import { OPERATOR_PAGE_HTML, OPERATOR_PAGE_JS } from "./operator-page.js";
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
  /** Logger for this listener. Default: same as the public app. */
  logger?: FastifyServerOptions["logger"];
}

export interface OperatorContext {
  db: LinkServerDB;
  auth: AuthManager;
  ws: WsManager;
  telepresence: TelepresenceManager;
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

function roomDetail(ctx: OperatorContext, roomId: string) {
  const room = ctx.db.getRoom(roomId);
  if (!room) return undefined;
  const online = new Set(ctx.telepresence.getOnlineAgents(roomId).map((a) => a.did));
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
    })),
  };
}

export async function buildOperatorApp(ctx: OperatorContext, opts: OperatorOptions): Promise<FastifyInstance> {
  if (opts.token.length < MIN_OPERATOR_TOKEN_LENGTH) {
    throw new Error(`operator token must be at least ${MIN_OPERATOR_TOKEN_LENGTH} characters`);
  }
  const expected = digest(opts.token);
  const app = Fastify({ logger: opts.logger ?? false, bodyLimit: 16 * 1024 });

  app.addHook("onSend", async (_request, reply) => {
    reply.header("X-Content-Type-Options", "nosniff");
    reply.header("Cache-Control", "no-store");
  });

  // The page and its script carry no data; everything else needs the token.
  app.get("/", async (_request, reply) => {
    return reply
      .type("text/html; charset=utf-8")
      .header("Content-Security-Policy", "default-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'")
      .send(OPERATOR_PAGE_HTML);
  });
  app.get("/admin.js", async (_request, reply) => {
    return reply.type("text/javascript; charset=utf-8").send(OPERATOR_PAGE_JS);
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
      if (request.method === "POST" && !String(request.headers["content-type"] ?? "").startsWith("application/json")) {
        return reply.code(415).send({ error: "content-type must be application/json" });
      }
    });

    /** Login of the signed-in human, as nginx passes it on (audit only, not auth). */
    const operatorOf = (headers: Record<string, unknown>): string | null => {
      const user = headers["x-forwarded-user"];
      return typeof user === "string" && user ? user : null;
    };

    api.get("/whoami", async (request) => ({ operator: operatorOf(request.headers) }));

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

      if (body.action === "remove") {
        if (body.did === room.admin_did) {
          return reply.code(400).send({ error: "the room admin cannot be removed" });
        }
        removeMember(ctx, roomId, body.did);
      } else {
        ctx.db.addAcl(roomId, body.did);
      }
      request.log.info(
        { operator: operatorOf(request.headers), roomId, action: body.action, did: body.did },
        "operator acl change"
      );
      return roomDetail(ctx, roomId);
    });
  }, { prefix: "/api" });

  return app;
}
