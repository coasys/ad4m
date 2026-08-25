import { signHex, verifyHex, verifyLinkExpression } from "./auth.js";
import type { LinkServerDB } from "./db.js";
import {
  canonicalFederationPayload,
  type FederateRequestBody,
  type LinkExpression,
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
 * signature from a known peer key, and every individual link inside a
 * federated diff must independently pass `verifyLinkExpression` — a peer
 * vouches for relaying agent-signed content, it cannot forge it.
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
  | { ok: false; status: 404 | 403; error: string };

export class FederationManager {
  private db: LinkServerDB;
  private identity: FederationIdentity;
  private ws: WsManager;
  private selfUrl?: string;
  private reconcileIntervalMs: number;
  private fetchImpl: typeof fetch;
  private log: (msg: string, err?: unknown) => void;
  private timer?: NodeJS.Timeout;

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
    return signHex(Buffer.from(this.identity.privateKey, "hex"), payload);
  }

  private verifyPeerSignature(publicKeyHex: string, payload: string, signatureHex: string): Promise<boolean> {
    return verifyHex(Buffer.from(publicKeyHex, "hex"), payload, signatureHex);
  }

  // ---- peer management ----

  /** Registers a federation peer for a room, best-effort fetching its identity key up front. */
  async addPeer(roomId: string, peerUrl: string): Promise<void> {
    let pubkey: string | undefined;
    try {
      const res = await this.fetchImpl(`${peerUrl}/server/identity`);
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
          // First signed contact from a peer we added before it was reachable: pin its key now.
          this.db.setFederationPeerPublicKey(roomId, serverUrl, serverPublicKey);
          return true;
        }
        if (byUrl.peer_public_key === serverPublicKey) return true;
      }
    }
    return false;
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
    const payload = canonicalFederationPayload("federate", roomId, { diff, sequence, revision });
    const serverSignature = await this.signPayload(payload);
    const body: FederateRequestBody = {
      diff,
      sequence,
      revision,
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

    const trusted = await this.resolvePeerTrust(roomId, body.serverPublicKey, body.serverUrl);
    if (!trusted) return { ok: false, status: 403, error: "unknown federation peer" };

    const payload = canonicalFederationPayload("federate", roomId, {
      diff: body.diff,
      sequence: body.sequence,
      revision: body.revision,
    });
    const validSig = await this.verifyPeerSignature(body.serverPublicKey, payload, body.serverSignature);
    if (!validSig) return { ok: false, status: 403, error: "invalid server signature" };

    for (const link of [...body.diff.additions, ...body.diff.removals]) {
      if (!(await verifyLinkExpression(link))) {
        return { ok: false, status: 400, error: `invalid link signature for author ${link.author}` };
      }
    }

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

    const trusted = await this.resolvePeerTrust(roomId, body.serverPublicKey, body.serverUrl);
    if (!trusted) return { ok: false, status: 403, error: "unknown federation peer" };

    const payload = canonicalFederationPayload("reconcile", roomId, {
      revision: body.revision,
      linkHashes: body.linkHashes,
    });
    const validSig = await this.verifyPeerSignature(body.serverPublicKey, payload, body.serverSignature);
    if (!validSig) return { ok: false, status: 403, error: "invalid server signature" };

    const theirs = new Set(body.linkHashes);
    const missing = this.db
      .getActiveLinkRows(roomId)
      .filter((row) => !theirs.has(row.link_hash))
      .map((row) => JSON.parse(row.link_data) as LinkExpression);

    return {
      ok: true,
      response: {
        diffs: missing.length > 0 ? [{ additions: missing, removals: [] }] : [],
        revision: this.db.getRoomRevision(roomId),
        sequence: this.db.getMaxSequence(roomId),
      },
    };
  }

  // ---- outbound: periodic anti-entropy reconciliation ----

  private async reconcileWithPeer(roomId: string, peerUrl: string): Promise<void> {
    const linkHashes = this.db.getActiveHashes(roomId);
    const revision = this.db.getRoomRevision(roomId);
    const payload = canonicalFederationPayload("reconcile", roomId, { revision, linkHashes });
    const serverSignature = await this.signPayload(payload);
    const body: ReconcileRequestBody = {
      revision,
      linkHashes,
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
      let allValid = true;
      for (const link of [...diff.additions, ...diff.removals]) {
        if (!(await verifyLinkExpression(link))) {
          allValid = false;
          break;
        }
      }
      if (!allValid) {
        this.log(`federation: reconcile response from ${peerUrl} had an invalid link signature, skipping`);
        continue;
      }
      const { sequence, revision: newRevision } = this.db.applyDiffAndAppend(
        roomId,
        diff,
        `federation-reconcile:${peerUrl}`
      );
      this.ws.broadcast(roomId, { type: "diff", payload: diff, revision: newRevision, sequence });
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
      void this.reconcileAll();
    }, this.reconcileIntervalMs);
    this.timer.unref();
  }

  stop(): void {
    if (this.timer) {
      clearInterval(this.timer);
      this.timer = undefined;
    }
  }

  private async postJson<T = unknown>(url: string, body: unknown): Promise<T> {
    const res = await this.fetchImpl(url, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify(body),
    });
    if (!res.ok) {
      const text = await res.text().catch(() => "");
      throw new Error(`POST ${url} failed: ${res.status} ${text}`);
    }
    return (await res.json()) as T;
  }
}
