import path from "node:path";
import Fastify, { type FastifyInstance } from "fastify";
import websocketPlugin from "@fastify/websocket";
import { AuthManager, ChallengeStore, ensureServerIdentity } from "./auth.js";
import { LinkServerDB } from "./db.js";
import { FederationManager } from "./federation.js";
import { SlidingWindowLimiter } from "./rate-limit.js";
import { registerRoutes, type RouteContext } from "./routes.js";
import { TelepresenceManager } from "./telepresence.js";
import { WsManager } from "./ws.js";

export interface ServerOptions {
  /** Directory the SQLite database lives in, or ":memory:" for an ephemeral in-memory DB (tests). */
  dataDir: string;
  host?: string;
  port?: number;
  /** How long issued JWTs (and their backing session rows) remain valid. Default 24h. */
  jwtExpirySeconds?: number;
  /** How often the federation reconciliation sweep runs. Default 60s. */
  reconcileIntervalMs?: number;
  /** Grace period before a disconnected agent is marked offline. Default 5s. */
  telepresenceGraceMs?: number;
  /** This server's own externally-reachable base URL, advertised to federation peers. */
  selfUrl?: string;
  /** Enable Fastify's pino logger. Default false (quiet, e.g. for tests). */
  logger?: boolean;
  /** Override fetch (tests can inject a stub to simulate peer servers without real network I/O). */
  fetchImpl?: typeof fetch;
  /**
   * When true, any DID that passes the challenge-response flow gets
   * auto-admitted to any room's ACL on first contact. Designed for
   * testing (wind tunnel) and open-community deployments. Default false.
   */
  autoAdmit?: boolean;
  /**
   * Override the sliding-window rate limiter thresholds. Defaults match the
   * project brief exactly (100/min per-IP auth, 300/min per-JWT room
   * endpoints, 60/min per-JWT commits); tests use small windows here to
   * exercise 429 behavior in milliseconds instead of real minutes.
   */
  rateLimits?: {
    authIp?: { limit: number; windowMs: number };
    roomJwt?: { limit: number; windowMs: number };
    commitJwt?: { limit: number; windowMs: number };
  };
}

export interface BuiltServer {
  app: FastifyInstance;
  db: LinkServerDB;
  federation: FederationManager;
  identity: { publicKey: string; privateKey: string };
  close: () => Promise<void>;
}

/**
 * Builds (but does not start listening on) a fully-wired ADAM Simple Server:
 * SQLite storage, DID auth, ACL, OR-Set link sync, WebSocket telepresence,
 * federation and E2E encryption. Call `app.listen(...)` on the result, or
 * use `close()` to tear everything down (used heavily by tests).
 */
export async function buildServer(opts: ServerOptions): Promise<BuiltServer> {
  const dbPath = opts.dataDir === ":memory:" ? ":memory:" : path.join(opts.dataDir, "data.sqlite");
  const db = new LinkServerDB(dbPath);
  const identity = await ensureServerIdentity(db);

  const app = Fastify({ logger: opts.logger ?? false });
  await app.register(websocketPlugin, {
    options: {
      // Cap inbound WebSocket frames at 1 MiB — link diffs and telepresence
      // signals are small JSON payloads; anything larger signals a misbehaving
      // client or an attack. The `ws` library closes the socket with 1009
      // (message too big) when a frame exceeds this limit.
      maxPayload: 1 * 1024 * 1024,
    },
  });

  const auth = new AuthManager(db, { jwtExpirySeconds: opts.jwtExpirySeconds });
  const challenges = new ChallengeStore();
  const telepresence = new TelepresenceManager({ graceMs: opts.telepresenceGraceMs });
  const ws = new WsManager(auth, telepresence);
  const federation = new FederationManager({
    db,
    identity,
    ws,
    selfUrl: opts.selfUrl,
    reconcileIntervalMs: opts.reconcileIntervalMs,
    fetchImpl: opts.fetchImpl,
    logger: { warn: (msg, err) => app.log.warn({ err }, msg) },
  });

  const rateLimits = {
    authIp: new SlidingWindowLimiter(
      opts.rateLimits?.authIp?.limit ?? 100,
      opts.rateLimits?.authIp?.windowMs ?? 60_000
    ),
    roomJwt: new SlidingWindowLimiter(
      opts.rateLimits?.roomJwt?.limit ?? 300,
      opts.rateLimits?.roomJwt?.windowMs ?? 60_000
    ),
    commitJwt: new SlidingWindowLimiter(
      opts.rateLimits?.commitJwt?.limit ?? 60,
      opts.rateLimits?.commitJwt?.windowMs ?? 60_000
    ),
  };

  const ctx: RouteContext = {
    db, auth, challenges, ws, telepresence, federation, identity, rateLimits,
    autoAdmit: opts.autoAdmit ?? false,
  };
  registerRoutes(app, ctx);
  ws.register(app);

  federation.start();

  app.addHook("onClose", async () => {
    await federation.stop();
    challenges.close();
    telepresence.close();
    rateLimits.authIp.close();
    rateLimits.roomJwt.close();
    rateLimits.commitJwt.close();
    ws.closeAll();
    db.close();
  });

  return {
    app,
    db,
    federation,
    identity,
    close: async () => {
      await app.close();
    },
  };
}
