import type { FastifyInstance, FastifyReply, FastifyRequest } from "fastify";
import { didToPublicKey, hashMessageForVerify, verifyHex, type AuthManager, type ChallengeStore } from "./auth.js";
import type { LinkServerDB } from "./db.js";
import { rotateRoomKey } from "./encryption.js";
import type { FederateResult, FederationIdentity, FederationManager } from "./federation.js";
import type { SlidingWindowLimiter } from "./rate-limit.js";
import type { TelepresenceManager } from "./telepresence.js";
import {
  type FederateRequestBody,
  isEncryptedLinkData,
  type LinkExpression,
  type ReconcileRequestBody,
  type RoomParams,
} from "./types.js";
import type { WsManager } from "./ws.js";

export interface RouteContext {
  db: LinkServerDB;
  auth: AuthManager;
  challenges: ChallengeStore;
  ws: WsManager;
  telepresence: TelepresenceManager;
  federation: FederationManager;
  identity: FederationIdentity;
  autoAdmit: boolean;
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

/** preHandler: verifies the JWT + live ACL membership, stashes claims on the request. */
function requireAuth(ctx: RouteContext) {
  return async (request: FastifyRequest, reply: FastifyReply): Promise<void> => {
    const { roomId } = request.params as RoomParams;
    const result = await ctx.auth.authenticate(bearerToken(request), roomId);
    if (!result.ok) {
      reply.code(result.status).send({ error: result.error });
      return;
    }
    request.authClaims = { did: result.did, roomId: result.roomId, token: result.token };
  };
}

/** preHandler: requires the authenticated agent to be the room's admin. Must run after requireAuth. */
function requireAdmin(ctx: RouteContext) {
  return async (request: FastifyRequest, reply: FastifyReply): Promise<void> => {
    const claims = request.authClaims!;
    const room = ctx.db.getRoom(claims.roomId);
    if (!room || room.admin_did !== claims.did) {
      reply.code(403).send({ error: "admin only" });
      return;
    }
  };
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

export function registerRoutes(app: FastifyInstance, ctx: RouteContext): void {
  // ---- health check (public, unauthenticated) ----

  app.get("/health", async (_request, reply) => {
    return reply.send({ status: "ok" });
  });

  // ---- server identity (public, not room-scoped) ----

  app.get("/server/identity", async (_request, reply) => {
    return reply.send({ publicKey: ctx.identity.publicKey });
  });

  // ---- auth: DID challenge-response ----

  app.post(
    "/rooms/:roomId/auth",
    { preHandler: ipRateLimit(ctx.rateLimits.authIp) },
    async (request, reply) => {
      const { roomId } = request.params as RoomParams;
      const body = request.body as {
        did?: string;
        challenge?: string;
        signature?: string;
        x25519PublicKey?: string;
      } | null;

      if (!body || typeof body.did !== "string") {
        return reply.code(400).send({ error: "did is required" });
      }
      try {
        didToPublicKey(body.did);
      } catch {
        return reply.code(400).send({ error: "invalid did:key" });
      }

      // Step 1: no challenge/signature yet -> issue a fresh nonce.
      if (body.challenge === undefined && body.signature === undefined) {
        const challenge = ctx.challenges.create(roomId, body.did);
        return reply.send({ challenge });
      }

      if (typeof body.challenge !== "string" || typeof body.signature !== "string") {
        return reply.code(400).send({ error: "challenge and signature are required" });
      }

      // Step 2: verify the signed challenge.
      const challengeValid = ctx.challenges.consume(roomId, body.did, body.challenge);
      if (!challengeValid) {
        return reply.code(401).send({ error: "invalid or expired challenge" });
      }
      const pubkey = didToPublicKey(body.did);
      // The AD4M executor's agentSignStringHex signs SHA-256(message),
      // not the raw message bytes. Hash the challenge to match.
      const challengeHash = hashMessageForVerify(body.challenge);
      const sigValid = await verifyHex(pubkey, challengeHash, body.signature);
      if (!sigValid) {
        return reply.code(401).send({ error: "invalid signature" });
      }

      const room = ctx.db.getRoom(roomId);
      if (!room) {
        ctx.db.createRoom(roomId, body.did);
        ctx.db.addAcl(roomId, body.did);
      } else if (!ctx.db.isMember(roomId, body.did)) {
        if (ctx.autoAdmit) {
          ctx.db.addAcl(roomId, body.did);
        } else {
          return reply
            .code(403)
            .send({ error: "not authorized for this room; ask the admin to add your DID to the ACL" });
        }
      }

      // Store the agent's derived X25519 public key for E2E room-key
      // sealing. The language sends this during step 2 of the DID
      // challenge-response — the one point where DID ownership has
      // already been verified.
      if (
        typeof body.x25519PublicKey === "string" &&
        body.x25519PublicKey.length === 64 &&
        /^[0-9a-fA-F]{64}$/.test(body.x25519PublicKey)
      ) {
        ctx.db.setX25519PublicKey(roomId, body.did, body.x25519PublicKey);
      }

      const { token, expiresAt } = await ctx.auth.issueSession(body.did, roomId);
      return reply.send({ token, expiresAt });
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

      for (const link of [...additions, ...removals]) {
        if (!link || typeof link !== "object") {
          return reply.code(400).send({ error: "each link must be an object" });
        }
        if (!link.data || typeof link.data !== "object") {
          return reply.code(400).send({ error: "each link must have a data object" });
        }
        if (isEncryptedLinkData(link.data)) {
          if (!link.link_hash || typeof link.link_hash !== "string") {
            return reply.code(400).send({ error: "encrypted links must include a link_hash" });
          }
        } else {
          if (link.author !== claims.did) {
            return reply
              .code(400)
              .send({ error: "every link's author must match the authenticated DID" });
          }
          if (typeof link.timestamp !== "string") {
            return reply.code(400).send({ error: "each link must have a string timestamp" });
          }
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
      void ctx.federation.forwardDiff(claims.roomId, { additions, removals }, sequence, revision);

      return reply.send({ sequence, revision });
    }
  );

  // ---- sync / render / revision / peers ----

  app.get(
    "/rooms/:roomId/sync",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const query = request.query as { since?: string };
      const parsedSince = query.since !== undefined ? Number.parseInt(query.since, 10) : 0;
      const since = Number.isFinite(parsedSince) ? Math.max(parsedSince, 0) : 0;
      const rows = ctx.db.getDiffsSinceParsed(claims.roomId, since);
      const revision = ctx.db.getRoomRevision(claims.roomId);
      const sequence = ctx.db.getMaxSequence(claims.roomId);
      return reply.send({ diffs: rows.map((r) => r.diff), revision, sequence });
    }
  );

  app.get(
    "/rooms/:roomId/render",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const links = ctx.db.getActiveLinks(claims.roomId);
      const revision = ctx.db.getRoomRevision(claims.roomId);
      const sequence = ctx.db.getMaxSequence(claims.roomId);
      return reply.send({ links, revision, sequence });
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

  // ---- ACL ----

  app.post(
    "/rooms/:roomId/acl",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt), requireAdmin(ctx)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const body = request.body as { action?: string; did?: string } | null;
      if (body?.action !== "add" && body?.action !== "remove") {
        return reply.code(400).send({ error: 'action must be "add" or "remove"' });
      }
      if (typeof body.did !== "string") {
        return reply.code(400).send({ error: "did is required" });
      }

      if (body.action === "remove") {
        if (body.did === claims.did) {
          return reply.code(400).send({ error: "admin cannot remove themselves from the ACL" });
        }
        ctx.db.removeAcl(claims.roomId, body.did);
        ctx.auth.revokeSessionsForDid(claims.roomId, body.did);
      } else {
        ctx.db.addAcl(claims.roomId, body.did);
      }

      const acl = ctx.db.getAcl(claims.roomId);
      const room = ctx.db.getRoom(claims.roomId)!;
      return reply.send({ admin: room.admin_did, members: acl.map((a) => a.did) });
    }
  );

  app.get(
    "/rooms/:roomId/acl",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const room = ctx.db.getRoom(claims.roomId)!;
      const acl = ctx.db.getAcl(claims.roomId);
      return reply.send({ admin: room.admin_did, members: acl.map((a) => a.did) });
    }
  );

  // ---- federation peer management ----

  app.post(
    "/rooms/:roomId/federation",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt), requireAdmin(ctx)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const body = request.body as { action?: string; peerUrl?: string } | null;
      if (body?.action !== "add" && body?.action !== "remove") {
        return reply.code(400).send({ error: 'action must be "add" or "remove"' });
      }
      if (typeof body.peerUrl !== "string") {
        return reply.code(400).send({ error: "peerUrl is required" });
      }
      if (body.action === "add") {
        await ctx.federation.addPeer(claims.roomId, body.peerUrl);
      } else {
        ctx.federation.removePeer(claims.roomId, body.peerUrl);
      }
      return reply.send({ peers: ctx.federation.listPeers(claims.roomId) });
    }
  );

  app.get(
    "/rooms/:roomId/federation",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      return reply.send({ peers: ctx.federation.listPeers(claims.roomId) });
    }
  );

  // ---- federation transport (server-to-server; authenticated by server signature, not JWT) ----
  // Rate-limited by IP (same pool as auth) since these endpoints accept
  // unauthenticated requests — the signature check happens inside the handler.

  app.post("/rooms/:roomId/federate", { preHandler: ipRateLimit(ctx.rateLimits.authIp) }, async (request, reply) => {
    const { roomId } = request.params as RoomParams;
    const body = request.body as Partial<FederateRequestBody> | null;
    if (
      !body ||
      !body.diff ||
      !Array.isArray(body.diff.additions) ||
      !Array.isArray(body.diff.removals) ||
      typeof body.serverPublicKey !== "string" ||
      typeof body.serverSignature !== "string" ||
      typeof body.sequence !== "number" ||
      typeof body.revision !== "string"
    ) {
      return reply.code(400).send({ error: "malformed federate payload" });
    }
    const result: FederateResult = await ctx.federation.handleIncomingFederate(
      roomId,
      body as FederateRequestBody
    );
    if (!result.ok) return reply.code(result.status).send({ error: result.error });
    return reply.send({ applied: result.applied, revision: result.revision, sequence: result.sequence });
  });

  app.post("/rooms/:roomId/reconcile", { preHandler: ipRateLimit(ctx.rateLimits.authIp) }, async (request, reply) => {
    const { roomId } = request.params as RoomParams;
    const body = request.body as Partial<ReconcileRequestBody> | null;
    if (
      !body ||
      typeof body.sinceSequence !== "number" ||
      typeof body.revision !== "string" ||
      typeof body.serverPublicKey !== "string" ||
      typeof body.serverSignature !== "string"
    ) {
      return reply.code(400).send({ error: "malformed reconcile payload" });
    }
    const result = await ctx.federation.handleIncomingReconcile(roomId, body as ReconcileRequestBody);
    if (!result.ok) return reply.code(result.status).send({ error: result.error });
    return reply.send(result.response);
  });

  // ---- E2E room keys ----

  app.get(
    "/rooms/:roomId/keys",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const rows = ctx.db.getAllRoomKeys(claims.roomId, claims.did);
      if (rows.length === 0) {
        return reply.code(404).send({ error: "no room key available for this agent yet" });
      }
      return reply.send({
        keys: rows.map((r) => ({ encryptedKey: JSON.parse(r.encrypted_key), version: r.version })),
      });
    }
  );

  app.post(
    "/rooms/:roomId/keys/rotate",
    { preHandler: [requireAuth(ctx), jwtRateLimit(ctx.rateLimits.roomJwt), requireAdmin(ctx)] },
    async (request, reply) => {
      const claims = request.authClaims!;
      const result = rotateRoomKey(ctx.db, claims.roomId);
      return reply.send({ version: result.version, recipients: result.recipients });
    }
  );
}
