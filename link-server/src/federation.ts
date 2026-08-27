import { signHex, verifyHex } from "./auth.js";
import type { LinkServerDB } from "./db.js";
import {
  canonicalFederationPayload,
  FEDERATION_PAYLOAD_MAX_AGE_MS,
  type FederateRequestBody,
  type PerspectiveDiff,
  type ReconcileRequestBody,
  type ReconcileResponseBody,
} from "./types.js";
import type { WsManager } from "./ws.js";

/**
 * Server-to-server federation: forwarding locally-committed diffs to peer
 * servers, receiving + verifying diffs forwarded by peers, and periodic
 * anti-entropy reconciliation.
 *
 * Trust model: a peer is "known" for a room once its ed25519 public key is
 * recorded in `federation_peers` (fetched eagerly from `${peerUrl}/server/identity`
 * when an admin adds the peer, or pinned on first successfully-signed
 * contact if that fetch failed/raced a peer that was briefly offline).
 * Every inbound federate/reconcile call must carry a valid ed25519
 * signature from a known peer key. Link signatures travel as metadata —
 * the server stores and relays them as-is; downstream consumers can
 * verify if they choose.
 *
 * To keep the topology simple (pairwise peers, not a full gossip mesh) this
 * server only forwards diffs it originated locally; diffs learned about via
 * federation or reconciliation are applied + pushed to local WebSocket
 * clients but never re-forwarded onward, which rules out forwarding loops
 * without needing per-diff provenance tracking.
 */

export interface FederationIdentity {
  publicKey: string;
  privateKey: string;
}

export interface FederationLogger {
  warn: (msg: string, err?: unknown) => void;
}

export interface FederationDeps {
  db: LinkServerDB;
  identity: FederationIdentity;
  ws: WsManager;
  selfUrl?: string;
  reconcileIntervalMs?: number;
  fetchImpl?: typeof fetch;
  logger?: FederationLogger;
}

export type FederateResult =
  | { ok: true; applied: number; revision: string; sequence: number }
  | { ok: false; status: 404 | 403 | 400; error: string };

export type ReconcileResult =
  | { ok: true; response: ReconcileResponseBody }
  | { ok: false; status: 400 | 404 | 403; error: string };

/** Hard ceiling on any single outbound federation HTTP request. Prevents
 *  a peer that accepts TCP but never responds from stalling reconciliation. */
const FEDERATION_FETCH_TIMEOUT_MS = 15_000;

export class FederationManager {
  private db: LinkServerDB;
  private identity: FederationIdentity;
  private ws: WsManager;
  private selfUrl?: string;
  private reconcileIntervalMs: number;
  private fetchImpl: typeof fetch;
  private log: (msg: string, err?: unknown) => void;
  private timer?: NodeJS.Timeout;
  /** Tracks an in-flight reconcileAll() so stop() can await it. */
  private reconcileInFlight: Promise<void> | null = null;

  constructor(deps: FederationDeps) {
    this.db = deps.db;
    this.identity = deps.identity;
    this.ws = deps.ws;
    this.selfUrl = deps.selfUrl;
    this.reconcileIntervalMs = deps.reconcileIntervalMs ?? 60_000;
    this.fetchImpl = deps.fetchImpl ?? fetch;
    this.log = deps.logger?.warn ?? (() => {});
  }

  private signPayload(payload: string): Promise<string> {
    return signHex(new Uint8Array(Buffer.from(this.identity.privateKey, "hex")), payload);
  }

  private verifyPeerSignature(publicKeyHex: string, payload: string, signatureHex: string): Promise<boolean> {
    return verifyHex(new Uint8Array(Buffer.from(publicKeyHex, "hex")), payload, signatureHex);
  }

  // ---- peer management ----

  /** Registers a federation peer for a room, best-effort fetching its identity key up front. */
  async addPeer(roomId: string, peerUrl: string): Promise<void> {
    let pubkey: string | undefined;
    try {
      const res = await this.fetchImpl(`${peerUrl}/server/identity`, {
        signal: AbortSignal.timeout(FEDERATION_FETCH_TIMEOUT_MS),
      });
      if (res.ok) {
        const body = (await res.json()) as { publicKey?: string };
        pubkey = body.publicKey;
      }
    } catch (err) {
      this.log(
        `federation: could not fetch identity for peer ${peerUrl} yet (will pin key on first signed contact)`,
        err
      );
    }
    this.db.addFederationPeer(roomId, peerUrl, pubkey);
  }

  removePeer(roomId: string, peerUrl: string): void {
    this.db.removeFederationPeer(roomId, peerUrl);
  }

  listPeers(roomId: string): string[] {
    return this.db.getFederationPeers(roomId).map((p) => p.peer_url);
  }

  private async resolvePeerTrust(
    roomId: string,
    serverPublicKey: string,
    serverUrl?: string
  ): Promise<boolean> {
    if (this.db.findFederationPeerByPublicKey(roomId, serverPublicKey)) return true;
    if (serverUrl) {
      const byUrl = this.db.findFederationPeerByUrl(roomId, serverUrl);
      if (byUrl) {
        if (!byUrl.peer_public_key) {
          // Peer was added by URL before its identity could be fetched.
          // Attempt to fetch the identity now — only pin the key if the
          // live fetch confirms it matches what the caller claims. This
          // closes the TOFU race where an attacker reaching the endpoint
          // before the real peer could pin a rogue key.
          try {
            const res = await this.fetchImpl(`${serverUrl}/server/identity`, {
              signal: AbortSignal.timeout(FEDERATION_FETCH_TIMEOUT_MS),
            });
            if (res.ok) {
              const body = (await res.json()) as { publicKey?: string };
              if (body.publicKey === serverPublicKey) {
                this.db.setFederationPeerPublicKey(roomId, serverUrl, serverPublicKey);
                return true;
              }
            }
          } catch {
            // Peer still unreachable — reject until admin re-adds or peer comes online.
          }
          return false;
        }
        if (byUrl.peer_public_key === serverPublicKey) return true;
      }
    }
    return false;
  }

  /** Rejects payloads whose timestamp falls outside the allowed window. */
  private isPayloadFresh(timestamp: string | undefined): boolean {
    if (!timestamp) return false;
    const payloadTime = new Date(timestamp).getTime();
    if (!Number.isFinite(payloadTime)) return false;
    const age = Math.abs(Date.now() - payloadTime);
    return age <= FEDERATION_PAYLOAD_MAX_AGE_MS;
  }

  // ---- outbound: forward a locally-committed diff ----

  /** Fire-and-forget (internally awaited via allSettled) push of a diff to every peer for the room. */
  async forwardDiff(
    roomId: string,
    diff: PerspectiveDiff,
    sequence: number,
    revision: string
  ): Promise<void> {
    const peers = this.db.getFederationPeers(roomId);
    if (peers.length === 0) return;
    const timestamp = new Date().toISOString();
    const payload = canonicalFederationPayload("federate", roomId, { diff, sequence, revision }, timestamp);
    const serverSignature = await this.signPayload(payload);
    const body: FederateRequestBody = {
      diff,
      sequence,
      revision,
      timestamp,
      serverPublicKey: this.identity.publicKey,
      serverSignature,
      serverUrl: this.selfUrl,
    };
    await Promise.allSettled(
      peers.map((peer) =>
        this.postJson(`${peer.peer_url}/rooms/${encodeURIComponent(roomId)}/federate`, body).catch(
          (err) => {
            this.log(`federation: failed to forward diff to ${peer.peer_url}`, err);
          }
        )
      )
    );
  }

  // ---- inbound: receive a diff forwarded by a peer ----

  async handleIncomingFederate(roomId: string, body: FederateRequestBody): Promise<FederateResult> {
    const room = this.db.getRoom(roomId);
    if (!room) return { ok: false, status: 404, error: "room not found" };

    if (!this.isPayloadFresh(body.timestamp)) {
      return { ok: false, status: 400, error: "stale or missing timestamp" };
    }

    const trusted = await this.resolvePeerTrust(roomId, body.serverPublicKey, body.serverUrl);
    if (!trusted) return { ok: false, status: 403, error: "unknown federation peer" };

    const payload = canonicalFederationPayload("federate", roomId, {
      diff: body.diff,
      sequence: body.sequence,
      revision: body.revision,
    }, body.timestamp);
    const validSig = await this.verifyPeerSignature(body.serverPublicKey, payload, body.serverSignature);
    if (!validSig) return { ok: false, status: 403, error: "invalid server signature" };

    const authorDid = `federation:${body.serverPublicKey.slice(0, 16)}`;
    const { sequence, revision } = this.db.applyDiffAndAppend(roomId, body.diff, authorDid);
    this.ws.broadcast(roomId, { type: "diff", payload: body.diff, revision, sequence });

    return {
      ok: true,
      applied: body.diff.additions.length + body.diff.removals.length,
      revision,
      sequence,
    };
  }

  // ---- inbound: reconciliation request from a peer ----

  async handleIncomingReconcile(roomId: string, body: ReconcileRequestBody): Promise<ReconcileResult> {
    const room = this.db.getRoom(roomId);
    if (!room) return { ok: false, status: 404, error: "room not found" };

    if (!this.isPayloadFresh(body.timestamp)) {
      return { ok: false, status: 400, error: "stale or missing timestamp" };
    }

    const trusted = await this.resolvePeerTrust(roomId, body.serverPublicKey, body.serverUrl);
    if (!trusted) return { ok: false, status: 403, error: "unknown federation peer" };

    const payload = canonicalFederationPayload("reconcile", roomId, {
      revision: body.revision,
      sinceSequence: body.sinceSequence,
    }, body.timestamp);
    const validSig = await this.verifyPeerSignature(body.serverPublicKey, payload, body.serverSignature);
    if (!validSig) return { ok: false, status: 403, error: "invalid server signature" };

    // Return diffs the peer hasn't seen, using sequence-based fast-forward.
    const rows = this.db.getDiffsSinceParsed(roomId, body.sinceSequence);
    const diffs = rows.map((r) => r.diff);

    return {
      ok: true,
      response: {
        diffs,
        revision: this.db.getRoomRevision(roomId),
        sequence: this.db.getMaxSequence(roomId),
      },
    };
  }

  // ---- outbound: periodic anti-entropy reconciliation ----

  private async reconcileWithPeer(roomId: string, peerUrl: string): Promise<void> {
    const sinceSequence = this.db.getPeerLastSequence(roomId, peerUrl);
    const revision = this.db.getRoomRevision(roomId);
    const timestamp = new Date().toISOString();
    const payload = canonicalFederationPayload("reconcile", roomId, { revision, sinceSequence }, timestamp);
    const serverSignature = await this.signPayload(payload);
    const body: ReconcileRequestBody = {
      revision,
      sinceSequence,
      timestamp,
      serverPublicKey: this.identity.publicKey,
      serverSignature,
      serverUrl: this.selfUrl,
    };

    let response: ReconcileResponseBody;
    try {
      response = await this.postJson<ReconcileResponseBody>(
        `${peerUrl}/rooms/${encodeURIComponent(roomId)}/reconcile`,
        body
      );
    } catch (err) {
      this.log(`federation: reconcile with ${peerUrl} failed`, err);
      return;
    }

    for (const diff of response.diffs) {
      if (diff.additions.length === 0 && diff.removals.length === 0) continue;
      const { sequence, revision: newRevision } = this.db.applyDiffAndAppend(
        roomId,
        diff,
        `federation-reconcile:${peerUrl}`
      );
      this.ws.broadcast(roomId, { type: "diff", payload: diff, revision: newRevision, sequence });
    }

    // Track the peer's latest sequence so the next reconciliation starts
    // from where this one left off (fast-forward).
    if (response.sequence > sinceSequence) {
      this.db.setPeerLastSequence(roomId, peerUrl, response.sequence);
    }
  }

  /** Runs one reconciliation pass against every peer of one room. Exposed for tests to call deterministically. */
  async reconcileRoom(roomId: string): Promise<void> {
    const peers = this.db.getFederationPeers(roomId);
    for (const peer of peers) {
      await this.reconcileWithPeer(roomId, peer.peer_url);
    }
  }

  /** Runs one reconciliation pass across every room that has federation peers. */
  async reconcileAll(): Promise<void> {
    for (const roomId of this.db.listAllRoomsWithPeers()) {
      await this.reconcileRoom(roomId);
    }
  }

  start(): void {
    if (this.timer) return;
    this.timer = setInterval(() => {
      // Skip this tick if a previous pass still runs — prevents overlap
      // from accumulating when a pass exceeds the interval.
      if (this.reconcileInFlight) return;
      this.reconcileInFlight = this.reconcileAll().finally(() => {
        this.reconcileInFlight = null;
      });
    }, this.reconcileIntervalMs);
    this.timer.unref();
  }

  async stop(): Promise<void> {
    if (this.timer) {
      clearInterval(this.timer);
      this.timer = undefined;
    }
    // Await any reconcileAll() already in flight so it finishes before
    // the caller proceeds to close the DB.
    if (this.reconcileInFlight) {
      try {
        await this.reconcileInFlight;
      } catch {
        // Best-effort — the caller (server.close()) is shutting down anyway.
      }
    }
  }

  private async postJson<T = unknown>(url: string, body: unknown): Promise<T> {
    const res = await this.fetchImpl(url, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify(body),
      signal: AbortSignal.timeout(FEDERATION_FETCH_TIMEOUT_MS),
    });
    if (!res.ok) {
      const text = await res.text().catch(() => "");
      throw new Error(`POST ${url} failed: ${res.status} ${text}`);
    }
    return (await res.json()) as T;
  }
}
