import path from "node:path";
import Fastify, { type FastifyInstance } from "fastify";
import compressPlugin from "@fastify/compress";
import websocketPlugin from "@fastify/websocket";
import { AuthManager, AuthReplayGuard, publicOrigin, type Admission } from "./auth.js";
import { LinkServerDB } from "./db.js";
import { LogTail, teeDestination } from "./log-tail.js";
import { MembershipRooms } from "./membership.js";
import { buildOperatorApp, type OperatorOptions } from "./operator.js";
import { SlidingWindowLimiter } from "./rate-limit.js";
import { registerRoutes, type RouteContext } from "./routes.js";
import { refreshKeys } from "./refresh.js";
import { StoreClient } from "./store-client.js";
import { TelepresenceManager } from "./telepresence.js";
import { WsManager, type WsManagerOptions } from "./ws.js";

export interface ServerOptions {
  /** Directory the SQLite database lives in, or ":memory:" for an ephemeral in-memory DB (tests). */
  dataDir: string;
  /** How long issued JWTs (and their backing session rows) remain valid. Default 24h. */
  jwtExpirySeconds?: number;
  /** Grace period before a disconnected agent is marked offline. Default 5s. */
  telepresenceGraceMs?: number;
  /** Enable Fastify's pino logger. Default false (quiet, e.g. for tests). */
  logger?: boolean;
  /**
   * Where the log lines go when `logger` is on. Default stdout. Each line is
   * also kept in `logTail` for the operator's live log view.
   */
  logDestination?: { write(line: string): unknown };
  /**
   * When true, every key that authenticates is admitted to the room, member or not. For tests
   * and development only: `index.ts` refuses it under `NODE_ENV=production`. Default false:
   * a key is admitted while its person (or bot) is a member of the room.
   */
  autoAdmit?: boolean;
  /** The server's name in `GET /.well-known/coasys-link-server`. Default "link-server". */
  name?: string;
  /**
   * The origins clients reach this server at (`https://links.example.org`): an auth message
   * names one of them, so a signature made for another server is refused. Normalised with
   * `publicOrigin`; a value with a path, query or fragment stops the build. Required unless
   * `hostOrigin` is on.
   */
  publicOrigins?: string[];
  /**
   * Development only: also take an auth signed for the origin each request's Host header names.
   * `index.ts` turns it on only without `PUBLIC_ORIGIN` on a server marked for development
   * (`ALLOW_AUTO_ADMIT_DEV=true`, never with `NODE_ENV=production`). Default false.
   */
  hostOrigin?: boolean;
  /** The identity store's base URL: whom each key speaks for. Without it no person is admitted. */
  storeUrl?: string;
  /** Multikeys of the store keys whose signed heads the server trusts. */
  storeHeadKeys?: string[];
  /**
   * How often the keys not revoked in some room (and keys with open sockets) are asked of the
   * store again, so a key its log revoked loses its sessions and sockets (ms). Default 600 000
   * (10 min).
   */
  refreshIntervalMs?: number;
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
  /** Maximum HTTP request body size. Default "10mb". */
  bodyLimit?: number;
  /** How often expired sessions get swept from the DB (ms). Default 900 000 (15 min). */
  sessionSweepIntervalMs?: number;
  /** WebSocket rate-limit and auth-timeout overrides. */
  wsOptions?: WsManagerOptions;
  /**
   * Builds the operator admission app (operator.ts) next to the public one.
   * The caller listens on it separately, on loopback only. Off by default.
   */
  operator?: OperatorOptions;
}

export interface BuiltServer {
  app: FastifyInstance;
  /** Present when `operator` was given; not listening until the caller says so. */
  operatorApp?: FastifyInstance;
  db: LinkServerDB;
  /** The recent log lines of both apps; present when `logger` is on. */
  logTail?: LogTail;
  /** Runs one key refresh now (the timer runs it every `refreshIntervalMs`): the keys revoked. */
  refreshKeys: () => Promise<string[]>;
  close: () => Promise<void>;
}

/**
 * Builds (but does not start listening on) a fully-wired link-server:
 * SQLite storage, DID auth, ACL, OR-Set link sync, WebSocket telepresence,
 * and E2E encryption. Call `app.listen(...)` on the result, or use
 * `close()` to tear everything down (used heavily by tests).
 */
export async function buildServer(opts: ServerOptions): Promise<BuiltServer> {
  // Checked before anything opens: a server that takes auth for no origin takes none.
  const publicOrigins = new Set((opts.publicOrigins ?? []).map(publicOrigin));
  const hostOrigin = opts.hostOrigin ?? false;
  if (publicOrigins.size === 0 && !hostOrigin) {
    throw new Error(
      "no public origin: set PUBLIC_ORIGIN (or publicOrigins) to the origin clients reach this server at, e.g. https://links.example.org"
    );
  }

  const dbPath = opts.dataDir === ":memory:" ? ":memory:" : path.join(opts.dataDir, "data.sqlite");
  const db = new LinkServerDB(dbPath);

  // One destination for both apps, so the operator's log view shows the
  // public app's lines and its own.
  const logTail = opts.logger ? new LogTail() : undefined;
  const logger = logTail
    ? { stream: teeDestination(opts.logDestination ?? process.stdout, logTail) }
    : false;

  const app = Fastify({
    logger,
    bodyLimit: opts.bodyLimit ?? 10 * 1024 * 1024, // 10 MiB
  });
  // Compresses JSON answers (above 1 kB) for a client that sends
  // Accept-Encoding: the Deno host's fetch sends it and decodes br/gzip. A
  // client that sends none gets the uncompressed body, as before. Request
  // bodies stay uncompressed: decompressing them would let a small upload
  // grow past bodyLimit.
  await app.register(compressPlugin, { globalDecompression: false });
  await app.register(websocketPlugin, {
    options: {
      // Cap inbound WebSocket frames at 1 MiB — link diffs and telepresence
      // signals are small JSON payloads; anything larger signals a misbehaving
      // client or an attack. The `ws` library closes the socket with 1009
      // (message too big) when a frame exceeds this limit.
      maxPayload: 1 * 1024 * 1024,
    },
  });

  const autoAdmit = opts.autoAdmit ?? false;
  const store = new StoreClient({ url: opts.storeUrl, headKeys: opts.storeHeadKeys ?? [] });
  const rooms = new MembershipRooms(db);
  // A person's own key while the person is a member; a bot key while the bot is one.
  const admits: Admission = (roomId, key) => {
    if (autoAdmit) return true;
    if (key.byBot) return rooms.isMember(roomId, key.did);
    return key.person !== undefined && rooms.isMember(roomId, key.person);
  };
  const auth = new AuthManager(db, admits, { jwtExpirySeconds: opts.jwtExpirySeconds });
  const replayGuard = new AuthReplayGuard();
  const telepresence = new TelepresenceManager({ graceMs: opts.telepresenceGraceMs });
  const ws = new WsManager(auth, telepresence, opts.wsOptions);

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
    db, auth, replayGuard, store, rooms, ws, telepresence, rateLimits, autoAdmit,
    name: opts.name ?? "link-server",
    publicOrigins,
    hostOrigin,
  };
  registerRoutes(app, ctx);
  ws.register(app);

  const operatorApp = opts.operator
    ? await buildOperatorApp(
        { db, auth, rooms, ws, telepresence, logTail, autoAdmit },
        { logger, ...opts.operator }
      )
    : undefined;

  const admission = autoAdmit ? "auto" : "membership";
  app.log.info({ admission }, `admission mode: ${admission}`);
  if (publicOrigins.size > 0) {
    app.log.info({ publicOrigins: [...publicOrigins] }, `auth is taken for: ${[...publicOrigins].join(", ")}`);
  }
  if (hostOrigin) {
    app.log.warn("auth is also taken for the origin each request's Host header names (development only)");
  }
  if (!store.configured) app.log.warn("no identity store configured (STORE_URL, STORE_HEAD_KEYS): no person can be admitted");

  // Keys not revoked in some room, and keys with open sockets, are asked of the store again: a
  // key its log revoked loses its sessions and sockets (refreshKeys).
  let refreshing = false;
  const refreshInterval = setInterval(() => {
    if (refreshing || !store.configured) return;
    refreshing = true;
    refreshKeys({ db, auth, ws, store, log: app.log })
      .catch((err: unknown) => app.log.error({ err }, "key refresh failed"))
      .finally(() => {
        refreshing = false;
      });
  }, opts.refreshIntervalMs ?? 10 * 60 * 1000);
  refreshInterval.unref();

  // Periodic sweep of expired JWT sessions from the DB.
  const sweepInterval = setInterval(() => {
    db.sweepExpiredSessions();
  }, opts.sessionSweepIntervalMs ?? 15 * 60 * 1000);
  sweepInterval.unref();

  app.addHook("onClose", async () => {
    clearInterval(sweepInterval);
    clearInterval(refreshInterval);
    replayGuard.close();
    telepresence.close();
    rateLimits.authIp.close();
    rateLimits.roomJwt.close();
    rateLimits.commitJwt.close();
    ws.closeAll();
    rooms.close();
    store.close();
    db.close();
  });

  return {
    app,
    operatorApp,
    db,
    logTail,
    refreshKeys: () => refreshKeys({ db, auth, ws, store, log: app.log }),
    close: async () => {
      // The operator app shares the DB the public app's onClose closes.
      await operatorApp?.close();
      await app.close();
    },
  };
}
