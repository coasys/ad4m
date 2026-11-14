import type { FastifyInstance, FastifyReply, FastifyRequest } from "fastify";
import {
  AUTH_SKEW_MS,
  authMessage,
  didToPublicKey,
  hashMessageForVerify,
  verifyHex,
  type AuthManager,
  type AuthReplayGuard,
} from "./auth.js";
import type { LinkServerDB } from "./db.js";
import type { SealedBox } from "./encryption.js";
import {
  isCoreError,
  isWireMembershipLink,
  MEMBERSHIP_PREFIX,
  verifyRoomKey,
  type WireMembershipLink,
} from "./identity-core.js";
import { hashOf, judgeLink, roomIdOf, type Judged, type LinkResult, type MembershipRooms } from "./membership.js";
import type { SlidingWindowLimiter } from "./rate-limit.js";
import { admittedKeys, announce } from "./room-events.js";
import { StoreUnavailable, type StoreClient } from "./store-client.js";
import type { TelepresenceManager } from "./telepresence.js";
import {
  isEncryptedLinkData,
  type LinkExpression,
  type RoomParams,
} from "./types.js";
import type { WsManager } from "./ws.js";

/** The protocol this server speaks (`GET /.well-known/coasys-link-server`). */
export const PROTOCOL_VERSION = 2;

export interface RouteContext {
  db: LinkServerDB;
  auth: AuthManager;
  replayGuard: AuthReplayGuard;
  store: StoreClient;
  rooms: MembershipRooms;
  ws: WsManager;
  telepresence: TelepresenceManager;
  autoAdmit: boolean;
  /** The server's name, as the well-known document gives it. */
  name: string;
  rateLimits: {
    authIp: SlidingWindowLimiter;
    roomJwt: SlidingWindowLimiter;
    commitJwt: SlidingWindowLimiter;
  };
}

function bearerToken(request: FastifyRequest): string | undefined {
  const header = request.headers.authorization;
  if (!header || !header.startsWith("Bearer ")) return undefined;
  return header.slice("Bearer ".length);
}

/**
 * preHandler: verifies the JWT and its session, and that the key is admitted to the room now
 * (its person a current member, or auto-admit). Stashes the claims on the request.
 */
function requireAuth(ctx: RouteContext) {
  return async (request: FastifyRequest, reply: FastifyReply): Promise<void> => {
    const { roomId } = request.params as RoomParams;
    const result = await ctx.auth.authenticate(bearerToken(request), roomId);
    if (!result.ok) {
      reply.code(result.status).send({ error: result.error });
      return;
    }
    request.authClaims = {
      did: result.did,
      roomId: result.roomId,
      token: result.token,
      person: result.person,
      byBot: result.byBot,
      admitted: result.admitted,
    };
  };
}

function isSealedBox(value: unknown): value is SealedBox {
  const box = value as Partial<SealedBox> | null;
  return (
    !!box &&
    typeof box === "object" &&
    typeof box.ephemeralPublicKey === "string" &&
    typeof box.nonce === "string" &&
    typeof box.ciphertext === "string"
  );
}

/** The admitted keys lacking one or more of the room's key versions. */
function membersMissingKeys(
  ctx: Pick<RouteContext, "db" | "auth">,
  roomId: string
): Array<{ did: string; missingVersions: number[] }> {
  const latestVersion = ctx.db.getLatestKeyVersion(roomId);
  if (latestVersion === 0) return [];
  const held = ctx.db.getAllMemberKeyVersions(roomId);
  const expected = Array.from({ length: latestVersion }, (_, i) => i + 1);
  const missing: Array<{ did: string; missingVersions: number[] }> = [];
  for (const row of admittedKeys(ctx, roomId)) {
    const has = new Set(held.get(row.did) ?? []);
    const missingVersions = expected.filter((v) => !has.has(v));
    if (missingVersions.length > 0) missing.push({ did: row.did, missingVersions });
  }
  return missing;
}

function rateLimitHook(limiter: SlidingWindowLimiter, keyOf: (request: FastifyRequest) => string) {
  return async (request: FastifyRequest, reply: FastifyReply): Promise<void> => {
    const check = limiter.check(keyOf(request));
    if (!check.allowed) {
      reply
        .header("Retry-After", String(check.retryAfterSec))
        .code(429)
        .send({ error: "rate limit exceeded", retryAfterSec: check.retryAfterSec });
    }
  };
}

function ipRateLimit(limiter: SlidingWindowLimiter) {
  return rateLimitHook(limiter, (request) => request.ip);
}

function jwtRateLimit(limiter: SlidingWindowLimiter) {
  return rateLimitHook(limiter, (request) => request.authClaims?.token ?? request.ip);
}

/** The most links (`/render`) or diffs (`/sync`) one page may carry. */
export const MAX_PAGE_LIMIT = 5000;

/** The page size of a `/render` or `/sync` request that names no `limit`. */
export const DEFAULT_PAGE_LIMIT = 500;

/** The page size a request asks for; null when `limit` is not an integer from 1 to MAX_PAGE_LIMIT. */
function parsePageLimit(raw: string | undefined): number | null {
  if (raw === undefined) return DEFAULT_PAGE_LIMIT;
  if (!/^\d+$/.test(raw)) return null;
  const limit = Number(raw);
  return limit >= 1 && limit <= MAX_PAGE_LIMIT ? limit : null;
}

/** Where a paged `/render` resumes: the snapshot's `upto` and the last `(sequence, link_hash)` sent. */
interface RenderCursor {
  upto: number;
  sequence: number;
  hash: string;
}

function encodeRenderCursor(cursor: RenderCursor): string {
  return Buffer.from(JSON.stringify([cursor.upto, cursor.sequence, cursor.hash])).toString("base64url");
}

function decodeRenderCursor(raw: string): RenderCursor | null {
  try {
    const parsed = JSON.parse(Buffer.from(raw, "base64url").toString("utf8")) as unknown;
    if (!Array.isArray(parsed) || parsed.length !== 3) return null;
    const [upto, sequence, hash] = parsed as unknown[];
    if (!Number.isSafeInteger(upto) || !Number.isSafeInteger(sequence) || typeof hash !== "string") return null;
    return { upto: upto as number, sequence: sequence as number, hash };
  } catch {
    return null;
  }
}

const HEX = /^[0-9a-f]+$/i;

/** The body of a protocol 2 auth request. */
interface AuthRequest {
  did: string;
  signature: string;
  timestamp: number;
  x25519PublicKey: string;
  x25519Signature: string;
}

function parseAuthRequest(body: unknown): AuthRequest | string {
  const b = body as Partial<AuthRequest> | null;
  if (!b || typeof b !== "object") return "body must be {did, signature, timestamp, x25519PublicKey, x25519Signature}";
  if (typeof b.did !== "string") return "did is required";
  try {
    didToPublicKey(b.did);
  } catch {
    return "invalid did:key";
  }
  if (typeof b.signature !== "string" || !HEX.test(b.signature)) return "signature must be hex";
  if (!Number.isSafeInteger(b.timestamp)) return "timestamp must be an integer (ms since the epoch)";
  if (typeof b.x25519PublicKey !== "string" || !/^[0-9a-f]{64}$/.test(b.x25519PublicKey)) {
    return "x25519PublicKey must be 64 lowercase hex characters";
  }
  if (typeof b.x25519Signature !== "string" || !HEX.test(b.x25519Signature)) return "x25519Signature must be hex";
  return b as AuthRequest;
}

/** The predicate of a self-join: the only link a key not yet admitted may send. */
const MEMBER_PREDICATE = `${MEMBERSHIP_PREFIX}member`;

/**
 * Judges a batch against the signers' logs. A link that is not wire-shaped is refused as
 * `malformed`; one `refuse` names is refused with that reason before any judgement. Throws
 * `StoreUnavailable` when the store cannot say whom a signer speaks for: nothing is applied then.
 */
async function judgeBatch(
  store: StoreClient,
  links: unknown[],
  refuse: (link: WireMembershipLink) => string | undefined,
  now: number
): Promise<Judged[]> {
  const batch: Judged[] = [];
  for (const raw of links) {
    if (!isWireMembershipLink(raw)) {
      batch.push({ link: raw as WireMembershipLink, hash: "", refused: "malformed" });
      continue;
    }
    const hash = hashOf(raw);
    if (hash === null) {
      batch.push({ link: raw, hash: "", refused: "malformed" });
      continue;
    }
    const refused = refuse(raw);
    if (refused) {
      batch.push({ link: raw, hash, refused });
      continue;
    }
    try {
      batch.push({ link: raw, hash, judged: await judgeLink(store, raw, now) });
    } catch (e) {
      if (e instanceof StoreUnavailable) throw e;
      if (isCoreError(e)) batch.push({ link: raw, hash, refused: e.reason });
      else throw e;
    }
  }
  return batch;
}

/** 200 with the results when any link counted; 403 with the first refusal otherwise. */
function answerResults(reply: FastifyReply, results: LinkResult[]) {
  if (results.some((r) => r.counted)) return reply.send({ results });
  const reason = results.find((r) => r.reason)?.reason ?? "notCounted";
  return reply.code(403).send({ reason, results });
}

export function registerRoutes(app: FastifyInstance, ctx: RouteContext): void {
  // ---- health check and discovery (public, unauthenticated) ----

  app.get("/health", async (_request, reply) => {
    return reply.send({ status: "ok" });
  });

  app.get("/.well-known/coasys-link-server", async (_request, reply) => {
    return reply.send({ protocol: PROTOCOL_VERSION, name: ctx.name, membership: true });
  });

  // ---- auth (protocol 2): a signed timestamp, the key's X25519 key, whom it speaks for ----

  app.post(
    "/rooms/:roomId/auth",
    { preHandler: ipRateLimit(ctx.rateLimits.authIp) },
    async (request, reply) => {
      const { roomId } = request.params as RoomParams;
      const body = parseAuthRequest(request.body);
      if (typeof body === "string") return reply.code(400).send({ error: body });

      // Auth never creates a room: the creator's genesis on /membership does.
      const room = ctx.db.getRoom(roomId);
      if (!room) return reply.code(404).send({ error: "no such room" });
      if (room.legacy) return reply.code(410).send({ error: "this room predates protocol 2 and is closed" });
      if (room.suspended) return reply.code(423).send({ error: "this room is suspended" });

      if (Math.abs(Date.now() - body.timestamp) > AUTH_SKEW_MS) {
        return reply.code(401).send({ error: "timestamp is too far from the server's clock" });
      }
      // Signed like every executor signature: over SHA-256 of the message.
      const digest = hashMessageForVerify(authMessage(roomId, body.timestamp));
      if (!(await verifyHex(didToPublicKey(body.did), digest, body.signature))) {
        return reply.code(401).send({ error: "invalid signature" });
      }
      let roomKeyValid = false;
      try {
        roomKeyValid = verifyRoomKey(body.did, body.x25519PublicKey, body.x25519Signature);
      } catch {
        roomKeyValid = false;
      }
      if (!roomKeyValid) return reply.code(401).send({ error: "x25519Signature does not sign x25519PublicKey" });
      if (!ctx.replayGuard.use(body.did, roomId, body.timestamp)) {
        return reply.code(401).send({ error: "this auth request was already used" });
      }

      // Whom the key speaks for, asked of the store now: a revoked key is refused at once.
      let person: string | undefined;
      let byBot = false;
      if (ctx.autoAdmit && !ctx.store.configured) {
        // An auto-admit server without a store (development): every key is a device-only key.
      } else {
        try {
          const resolution = await ctx.store.resolve(body.did, { fresh: true });
          if (resolution.kind === "listed") {
            if (!resolution.author) return reply.code(403).send({ error: "this key is revoked or expired in its log" });
            person = resolution.author.person;
            byBot = resolution.author.byBot;
          }
        } catch (e) {
          if (e instanceof StoreUnavailable) {
            request.log.warn({ err: e.message }, "identity store unavailable at auth");
            return reply.code(503).send({ error: "the identity store is unavailable" });
          }
          throw e;
        }
      }

      ctx.db.upsertMemberKey(roomId, body.did, person ?? null, byBot, body.x25519PublicKey, body.x25519Signature);
      const { token, expiresAt } = await ctx.auth.issueSession(body.did, roomId, person, byBot);
      const admitted = ctx.auth.admitted(roomId, { did: body.did, person, byBot });
      return reply.send({ token, expiresAt, admitted });
    }
  );

  // ---- membership (protocol 2) ----

  app.post(
    "/rooms/:roomId/membership",
    { preHandler: ipRateLimit(ctx.rateLimits.authIp) },
    async (request, reply) => {
      const { roomId } = request.params as RoomParams;
      const body = request.body as { genesis?: { creator?: unknown; salt?: unknown }; links?: unknown } | null;
      if (!body || typeof body !== "object" || !Array.isArray(body.links) || body.links.length === 0) {
        return reply.code(400).send({ error: "body must be {genesis?, links: [...]} with at least one link" });
      }
      const links = body.links as unknown[];
      const now = Date.now();

      if (!ctx.db.getRoom(roomId)) {
        // A room comes to be with its creator's genesis: no token, as no room exists to hold one.
        const genesis = body.genesis;
        if (!genesis) return reply.code(404).send({ error: "no such room" });
        if (typeof genesis.creator !== "string" || typeof genesis.salt !== "string") {
          return reply.code(400).send({ error: "genesis must be {creator, salt}" });
        }
        if (roomIdOf(genesis.creator, genesis.salt) !== roomId) {
          return reply.code(400).send({ error: "the room id is not base58(sha256(creator, 0x00, salt))" });
        }
        let batch: Judged[];
        try {
          batch = await judgeBatch(ctx.store, links, () => undefined, now);
        } catch (e) {
          if (e instanceof StoreUnavailable) return reply.code(503).send({ error: "the identity store is unavailable" });
          throw e;
        }
        const { created, results } = ctx.rooms.genesis(roomId, genesis.creator, batch);
        if (!created) {
          const reason = results.find((r) => r.reason)?.reason ?? "notCreator";
          return reply.code(403).send({ reason, results });
        }
        request.log.info({ roomId, creator: genesis.creator }, "room created");
        return reply.send({ results });
      }

      // The room exists: a genesis is ignored, and a token is needed.
      const session = await ctx.auth.authenticate(bearerToken(request), roomId, { requireAdmitted: false });
      if (!session.ok) return reply.code(session.status).send({ error: session.error });
      const limited = ctx.rateLimits.roomJwt.check(session.token);
      if (!limited.allowed) {
        return reply
          .header("Retry-After", String(limited.retryAfterSec))
          .code(429)
          .send({ error: "rate limit exceeded", retryAfterSec: limited.retryAfterSec });
      }

      // A key not yet admitted may send its own person's self-join only: nothing else of the room
      // is its own. A key that speaks for no person, or only as a bot, cannot join by itself
      // (`needsIdentity`, as the core's `admits` says): a bot joins by its owner's bot link.
      const refuse = (link: WireMembershipLink): string | undefined => {
        if (session.admitted) return undefined;
        if (!session.person || session.byBot) return "needsIdentity";
        const selfJoin =
          link.data.predicate === MEMBER_PREDICATE && link.data.source === session.person && link.author === session.did;
        return selfJoin ? undefined : "notAdmitted";
      };

      let batch: Judged[];
      try {
        batch = await judgeBatch(ctx.store, links, refuse, now);
      } catch (e) {
        if (e instanceof StoreUnavailable) return reply.code(503).send({ error: "the identity store is unavailable" });
        throw e;
      }
      // The room may have gone while the links were judged.
      if (!ctx.rooms.replay(roomId)) return reply.code(404).send({ error: "no such room" });
      const outcome = ctx.rooms.apply(roomId, batch);
      announce(ctx, roomId, outcome);
      return answerResults(reply, outcome.results);
    }
  );

  app.get(
    "/rooms/:roomId/membership",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const query = request.query as { since?: string; limit?: string };
      const since = query.since !== undefined && /^\d+$/.test(query.since) ? Number(query.since) : 0;
      const limit = parsePageLimit(query.limit);
      if (limit === null) return reply.code(400).send({ error: `limit must be an integer from 1 to ${MAX_PAGE_LIMIT}` });
      const rows = ctx.db.getMembershipSince(claims.roomId, since, limit);
      const seq = rows.length > 0 ? rows[rows.length - 1].seq : ctx.db.getMembershipSeq(claims.roomId);
      return reply.send({ links: rows.map((r) => JSON.parse(r.link) as WireMembershipLink), seq });
    }
  );

  app.get(
    "/rooms/:roomId/members",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      return reply.send({
        members: ctx.rooms.members(claims.roomId),
        // The keys a room key is sealed to, each with its signed X25519 key.
        keys: admittedKeys(ctx, claims.roomId).map((k) => ({
          did: k.did,
          person: k.person,
          byBot: k.by_bot === 1,
          x25519PublicKey: k.x25519,
          x25519Signature: k.x25519_sig,
        })),
      });
    }
  );

  // ---- commit ----

  app.post(
    "/rooms/:roomId/commit",
    {
      preHandler: [
        requireAuth(ctx),
        jwtRateLimit(ctx.rateLimits.roomJwt),
        jwtRateLimit(ctx.rateLimits.commitJwt),
      ],
    },
    async (request, reply) => {
      const claims = request.authClaims!;
      const body = request.body as { additions?: unknown; removals?: unknown } | null;
      if (!body || typeof body !== "object") {
        return reply.code(400).send({ error: "request body must be an object with additions and/or removals" });
      }
      if (body.additions !== undefined && !Array.isArray(body.additions)) {
        return reply.code(400).send({ error: "additions must be an array" });
      }
      if (body.removals !== undefined && !Array.isArray(body.removals)) {
        return reply.code(400).send({ error: "removals must be an array" });
      }
      const additions = (body.additions as LinkExpression[] | undefined) ?? [];
      const removals = (body.removals as LinkExpression[] | undefined) ?? [];

      if (additions.length === 0 && removals.length === 0) {
        return reply.code(400).send({ error: "commit must contain at least one addition or removal" });
      }

      // Every room is end-to-end encrypted from creation: refuse plaintext links.
      for (const link of [...additions, ...removals]) {
        if (!link || typeof link !== "object") {
          return reply.code(400).send({ error: "each link must be an object" });
        }
        if (!link.data || typeof link.data !== "object") {
          return reply.code(400).send({ error: "each link must have a data object" });
        }
        // Membership is public to the room and judged by the server: it has its own route.
        const predicate = (link.data as { predicate?: unknown }).predicate;
        if (typeof predicate === "string" && predicate.startsWith(MEMBERSHIP_PREFIX)) {
          return reply.code(400).send({ error: "membership links go to POST /rooms/:roomId/membership, not /commit" });
        }
        // Only ciphertext and nonce: a plaintext key beside them would be stored and served in the clear.
        if (!isEncryptedLinkData(link.data) || Object.keys(link.data).length !== 2) {
          return reply.code(400).send({
            error: "this room requires end-to-end encryption; link data must be exactly {ciphertext, nonce}",
          });
        }
        if (!link.link_hash || typeof link.link_hash !== "string") {
          return reply.code(400).send({ error: "encrypted links must include a link_hash" });
        }
        if (link.key_version !== undefined && !(Number.isSafeInteger(link.key_version) && link.key_version > 0)) {
          return reply.code(400).send({ error: "key_version must be a positive integer" });
        }
      }

      const { sequence, revision } = ctx.db.applyDiffAndAppend(
        claims.roomId,
        { additions, removals },
        claims.did
      );

      ctx.ws.broadcast(
        claims.roomId,
        { type: "diff", payload: { additions, removals }, revision, sequence },
        { excludeDid: claims.did }
      );

      return reply.send({ sequence, revision });
    }
  );

  // ---- sync / render / revision / peers ----

  // Both routes are paged; see README, "Paged snapshot, then tail".
  app.get(
    "/rooms/:roomId/sync",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const query = request.query as { since?: string; limit?: string };
      const parsedSince = query.since !== undefined ? Number.parseInt(query.since, 10) : 0;
      const since = Number.isFinite(parsedSince) ? Math.max(parsedSince, 0) : 0;
      const limit = parsePageLimit(query.limit);
      if (limit === null) return reply.code(400).send({ error: `limit must be an integer from 1 to ${MAX_PAGE_LIMIT}` });
      const revision = ctx.db.getRoomRevision(claims.roomId);
      const sequence = ctx.db.getMaxSequence(claims.roomId);
      // One row past the page tells whether another page follows.
      const rows = ctx.db.getDiffPageSince(claims.roomId, since, limit + 1);
      const page = rows.slice(0, limit);
      const next = rows.length > limit ? page[page.length - 1].sequence : null;
      // Each entry names its sequence, so a client's cursor never runs ahead of what it applied.
      const diffs = page.map((row) => ({ ...(JSON.parse(row.diff_data) as object), sequence: row.sequence }));
      return reply.send({ diffs, revision, sequence, next });
    }
  );

  app.get(
    "/rooms/:roomId/render",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const query = request.query as { limit?: string; next?: string };
      const limit = parsePageLimit(query.limit);
      if (limit === null) return reply.code(400).send({ error: `limit must be an integer from 1 to ${MAX_PAGE_LIMIT}` });
      const revision = ctx.db.getRoomRevision(claims.roomId);
      const sequence = ctx.db.getMaxSequence(claims.roomId);
      // The first page pins the snapshot at the room's current sequence; the
      // cursor carries it to every later page.
      let cursor: RenderCursor = { upto: sequence, sequence: 0, hash: "" };
      if (query.next !== undefined) {
        const decoded = decodeRenderCursor(query.next);
        if (!decoded) return reply.code(400).send({ error: "malformed next cursor" });
        cursor = decoded;
      }
      const rows = ctx.db.getActiveLinkPage(claims.roomId, cursor.upto, cursor, limit + 1);
      const page = rows.slice(0, limit);
      const last = page[page.length - 1];
      const next = rows.length > limit
        ? encodeRenderCursor({ upto: cursor.upto, sequence: last.sequence, hash: last.link_hash })
        : null;
      const links = page.map((row) => JSON.parse(row.link_data) as LinkExpression);
      return reply.send({ links, revision, sequence, upto: cursor.upto, next });
    }
  );

  app.get(
    "/rooms/:roomId/revision",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const revision = ctx.db.getRoomRevision(claims.roomId);
      const sequence = ctx.db.getMaxSequence(claims.roomId);
      return reply.send({ revision, sequence });
    }
  );

  app.get(
    "/rooms/:roomId/peers",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const agents = ctx.telepresence.getOnlineAgents(claims.roomId);
      return reply.send({ peers: agents.map((a) => a.did) });
    }
  );

  // ---- E2E room keys ----

  app.get(
    "/rooms/:roomId/keys",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      // An empty list: no member has rotated a key in yet, or no holder
      // has granted this key one.
      const rows = ctx.db.getAllRoomKeys(claims.roomId, claims.did);
      return reply.send({
        keys: rows.map((r) => ({
          encryptedKey: JSON.parse(r.encrypted_key),
          version: r.version,
          commitment: r.commitment ?? undefined,
        })),
      });
    }
  );

  // Any admitted member rotates; the new key goes to exactly the keys admitted now.
  app.post(
    "/rooms/:roomId/keys/rotate",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const body = request.body as {
        keys?: Array<{ did: string; encryptedKey: unknown }>;
        commitment?: unknown;
      } | null;

      if (!body || !Array.isArray(body.keys) || body.keys.length === 0) {
        return reply.code(400).send({
          error: "keys array is required and must not be empty — " +
                 "generate the room key client-side and seal it to each admitted key",
        });
      }
      const seen = new Set<string>();
      for (const entry of body.keys) {
        if (!entry || typeof entry !== "object") {
          return reply.code(400).send({ error: "each key entry must be a non-null object" });
        }
        if (typeof entry.did !== "string") {
          return reply.code(400).send({ error: "each key entry must be {did, encryptedKey}" });
        }
        if (seen.has(entry.did)) return reply.code(400).send({ error: `DID ${entry.did} appears twice` });
        seen.add(entry.did);
        if (!isSealedBox(entry.encryptedKey)) {
          return reply.code(400).send({ error: `malformed encryptedKey for DID ${entry.did}` });
        }
      }

      // Members check every key they open against it, whoever granted the box.
      if (typeof body.commitment !== "string" || !/^[0-9a-f]{64}$/.test(body.commitment)) {
        return reply.code(400).send({ error: "commitment is required: 64 hex characters committing to the new key" });
      }

      // Exactly the admitted keys: none left out (a member who could not read), none extra (a
      // removed or revoked key that would read on).
      const admitted = new Set(admittedKeys(ctx, claims.roomId).map((k) => k.did));
      const missing = [...admitted].filter((did) => !seen.has(did));
      const extra = [...seen].filter((did) => !admitted.has(did));
      if (missing.length > 0 || extra.length > 0) {
        return reply.code(409).send({
          error: "the envelopes must go to exactly the keys admitted now (GET /members)",
          missing,
          extra,
        });
      }

      const version = ctx.db.getLatestKeyVersion(claims.roomId) + 1;
      ctx.db.addKeyCommitment(claims.roomId, version, body.commitment);
      const recipients: string[] = [];
      for (const entry of body.keys) {
        ctx.db.addRoomKey(
          claims.roomId,
          entry.did,
          version,
          JSON.stringify(entry.encryptedKey),
        );
        recipients.push(entry.did);
      }

      return reply.send({ version, recipients, membersNeedingHistoricalKeys: membersMissingKeys(ctx, claims.roomId) });
    }
  );

  // ---- E2E missing keys (any member: which admitted key lacks which version?) ----

  app.get(
    "/rooms/:roomId/keys/missing",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      return reply.send({ membersNeedingHistoricalKeys: membersMissingKeys(ctx, claims.roomId) });
    }
  );

  // ---- E2E: a member drops its own box for a version (it did not open to the committed key) ----

  app.delete(
    "/rooms/:roomId/keys/:version",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const version = Number((request.params as { version: string }).version);
      if (!Number.isInteger(version) || version < 1) {
        return reply.code(400).send({ error: "version must be a positive integer" });
      }
      // The version is then missing for this member again, so a member holding it grants it.
      return reply.send({ deleted: ctx.db.deleteRoomKey(claims.roomId, claims.did, version) });
    }
  );

  // ---- E2E key grant (any member re-seals versions it holds for an admitted key lacking them) ----

  app.post(
    "/rooms/:roomId/keys/grant",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const body = request.body as {
        targetDid?: string;
        keys?: Array<{ version: number; encryptedKey: SealedBox }>;
      } | null;

      if (!body || typeof body.targetDid !== "string") {
        return reply.code(400).send({ error: "targetDid is required" });
      }
      if (!Array.isArray(body.keys) || body.keys.length === 0) {
        return reply.code(400).send({ error: "keys array is required and must not be empty" });
      }

      // The target must be a key admitted to the room now.
      if (!admittedKeys(ctx, claims.roomId).some((k) => k.did === body.targetDid)) {
        return reply.code(404).send({ error: "target DID is not a key admitted to this room" });
      }

      // Validate every entry before storing any: the version exists, the
      // granter holds it (a member passes on only what it was given), and the
      // box has the expected shape. The server cannot check the key inside;
      // the first box stored for a version stays.
      const latestVersion = ctx.db.getLatestKeyVersion(claims.roomId);
      const held = new Set(ctx.db.getAllRoomKeys(claims.roomId, claims.did).map((r) => r.version));
      for (const entry of body.keys) {
        if (!entry || typeof entry !== "object") {
          return reply.code(400).send({ error: "each key entry must be a non-null object" });
        }
        if (!Number.isInteger(entry.version) || entry.version < 1 || entry.version > latestVersion) {
          return reply.code(400).send({ error: `invalid version ${entry.version}; latest is ${latestVersion}` });
        }
        if (!held.has(entry.version)) {
          return reply.code(403).send({ error: `you do not hold key version ${entry.version}` });
        }
        if (!isSealedBox(entry.encryptedKey)) {
          return reply.code(400).send({ error: `malformed encryptedKey for version ${entry.version}` });
        }
      }

      const stored: number[] = [];
      for (const entry of body.keys) {
        if (ctx.db.addRoomKeyIfMissing(claims.roomId, body.targetDid, entry.version, JSON.stringify(entry.encryptedKey))) {
          stored.push(entry.version);
        }
      }
      // The grantee waits for these to read the room's history: tell it now.
      if (stored.length > 0) {
        ctx.ws.sendToDid(claims.roomId, body.targetDid, { type: "keys-granted", versions: stored });
      }
      return reply.send({ granted: stored });
    }
  );
}
