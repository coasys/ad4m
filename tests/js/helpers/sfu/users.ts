/**
 * Per-peer user provisioning for the wind tunnel.
 *
 * Production AD4M deployments authenticate participants via per-user
 * JWTs minted from `user.create` + `user.login`.  The wind tunnel
 * follows the same flow so each synthetic peer has a real distinct
 * `did`, the SFU sees genuine multi-user calls, and no admin escape
 * hatches are needed.
 *
 * Each `PeerSession` carries:
 *   - the email/password the admin client created,
 *   - the per-user JWT,
 *   - an authenticated `InstrumentedClient` opened with that JWT.
 *
 * The admin client passed to `provisionPeers` must already be
 * connected and authorised; it's used to call `user.create` and
 * `user.login` on behalf of every synthetic peer.
 */

import { InstrumentedClient } from "./client.js";

export interface PeerSession {
  /** Stable identifier used in logging — e.g. `t1-peer-3`. */
  label: string;
  /** Per-user email the wind tunnel created on the executor. */
  email: string;
  /** Per-user password — random per session, never reused. */
  password: string;
  /** JWT minted by `user.login` for this user. */
  token: string;
  /** AD4M `did:key:…` for this user (as reported by `user.create`). */
  did: string;
  /** Authenticated client opened with `token`. */
  client: InstrumentedClient;
}

interface ProvisionOptions {
  /**
   * Admin client used to call `user.create` and `user.login`.  Must
   * already be connected.
   */
  admin: InstrumentedClient;
  /** Port the executor is listening on. */
  port: number;
  host?: string;
  /** Number of peers to provision. */
  count: number;
  /** Label prefix — `<prefix>-<i>`.  Default `"peer"`. */
  labelPrefix?: string;
  /**
   * Email-domain suffix — every user gets `<label>@<emailDomain>`.
   * Defaults to a per-call random suffix so re-runs of the same
   * scenario don't collide with previously-created users in the same
   * data dir.
   */
  emailDomain?: string;
}

function randomSuffix(): string {
  return Math.random().toString(36).slice(2, 10);
}

export async function provisionPeers(opts: ProvisionOptions): Promise<PeerSession[]> {
  const host = opts.host ?? "127.0.0.1";
  const labelPrefix = opts.labelPrefix ?? "peer";
  const emailDomain = opts.emailDomain ?? `windtunnel-${randomSuffix()}.local`;

  const sessions: PeerSession[] = [];
  for (let i = 0; i < opts.count; i++) {
    const label = `${labelPrefix}-${i}`;
    const email = `${label}@${emailDomain}`;
    const password = `pw-${randomSuffix()}`;

    const create = await opts.admin.call<{
      did: string;
      success: boolean;
      error?: string;
    }>("user.create", { email, password });
    if (!create.success) {
      throw new Error(
        `provisionPeers: user.create failed for ${label} (${email}): ${
          create.error ?? "unknown"
        }`,
      );
    }

    const token = await opts.admin.call<string>("user.login", {
      email,
      password,
      appName: "wind-tunnel",
    });
    if (typeof token !== "string" || token.length === 0) {
      throw new Error(`provisionPeers: user.login returned empty token for ${label}`);
    }

    const client = new InstrumentedClient({ port: opts.port, host, adminToken: token });
    await client.connect();

    sessions.push({
      label,
      email,
      password,
      token,
      did: create.did,
      client,
    });
  }
  return sessions;
}

export async function disconnectPeers(sessions: PeerSession[]): Promise<void> {
  for (const s of sessions) {
    try {
      await s.client.disconnect();
    } catch {
      /* best-effort */
    }
  }
}

/**
 * Register provisioned peers as neighbourhood members via
 * `sfu.ensureMembership`.
 *
 * Must run AFTER provisioning peers and BEFORE `sfu.callJoin`.  Each
 * peer's DID gets written into the perspective_handle owners list for
 * `neighbourhoodUrl` — the same data store that `callJoin` queries.
 */
export async function registerSfuMembers(opts: {
  admin: InstrumentedClient;
  neighbourhoodUrl: string;
  sessions: PeerSession[];
}): Promise<void> {
  for (const s of opts.sessions) {
    await opts.admin.call("sfu.ensureMembership", {
      neighbourhoodUrl: opts.neighbourhoodUrl,
      did: s.did,
    });
  }
}

export interface ClusterNodeAuth {
  /** Cluster identifier (matches the DID/label of the SFU node). */
  nodeId: string;
  /** Admin client to this executor (already connected). */
  admin: InstrumentedClient;
  /** WS port for this executor. */
  port: number;
  /** Host (defaults to 127.0.0.1). */
  host?: string;
}

export interface ClusterPeerSession {
  /** Stable label, e.g. `t3-peer-3`. */
  label: string;
  email: string;
  password: string;
  /** Per-node user state.  Same email/password/DID across all nodes,
   *  but each node mints its own JWT and we open a fresh client per
   *  node so any node can be used for callJoin / callLeave. */
  byNode: Map<string, { token: string; did: string; client: InstrumentedClient }>;
}

/**
 * Provision N peers across an entire SFU cluster.  Each peer is created
 * as a user on every cluster node so it can callJoin to any node
 * (handy for cascade scenarios where peers may follow redirects).  The
 * email/password is shared; each node mints its own JWT.
 *
 * Returns the sessions plus a convenience helper `pickClient(peer,
 * nodeId)` for scenario code.
 */
export async function provisionClusterPeers(opts: {
  nodes: ClusterNodeAuth[];
  count: number;
  labelPrefix?: string;
  emailDomain?: string;
}): Promise<ClusterPeerSession[]> {
  const labelPrefix = opts.labelPrefix ?? "peer";
  const emailDomain = opts.emailDomain ?? `windtunnel-${randomSuffix()}.local`;
  const out: ClusterPeerSession[] = [];

  for (let i = 0; i < opts.count; i++) {
    const label = `${labelPrefix}-${i}`;
    const email = `${label}@${emailDomain}`;
    const password = `pw-${randomSuffix()}`;
    const byNode = new Map<string, { token: string; did: string; client: InstrumentedClient }>();

    for (const node of opts.nodes) {
      const create = await node.admin.call<{
        did: string;
        success: boolean;
        error?: string;
      }>("user.create", { email, password });
      if (!create.success) {
        throw new Error(
          `provisionClusterPeers: user.create failed on ${node.nodeId} for ${label}: ${
            create.error ?? "unknown"
          }`,
        );
      }
      const token = await node.admin.call<string>("user.login", {
        email,
        password,
        appName: "wind-tunnel",
      });
      if (typeof token !== "string" || token.length === 0) {
        throw new Error(`provisionClusterPeers: empty token on ${node.nodeId} for ${label}`);
      }
      const client = new InstrumentedClient({
        port: node.port,
        host: node.host ?? "127.0.0.1",
        adminToken: token,
      });
      await client.connect();
      byNode.set(node.nodeId, { token, did: create.did, client });
    }

    out.push({ label, email, password, byNode });
  }
  return out;
}

export async function disconnectClusterPeers(sessions: ClusterPeerSession[]): Promise<void> {
  for (const s of sessions) {
    for (const [, entry] of s.byNode) {
      try {
        await entry.client.disconnect();
      } catch {
        /* best-effort */
      }
    }
  }
}

/**
 * Register cluster-provisioned peers as neighbourhood members on every
 * node via `sfu.ensureMembership`.
 *
 * Each node generates its own DID for each user (independent keypairs),
 * so we must register EACH node's DID on EVERY node.  A peer calling
 * `callJoin` on node B presents node B's JWT, which carries node B's
 * DID — if we only registered node A's DID, node B would reject.
 */
export async function registerClusterSfuMembers(opts: {
  nodes: { nodeId: string; admin: InstrumentedClient }[];
  neighbourhoodUrl: string;
  sessions: ClusterPeerSession[];
}): Promise<void> {
  for (const session of opts.sessions) {
    // Collect ALL DIDs this user has across the cluster.
    const allDids = new Set<string>();
    for (const [, entry] of session.byNode) {
      allDids.add(entry.did);
    }
    // Register every DID on every node so the user can join anywhere.
    for (const did of allDids) {
      for (const node of opts.nodes) {
        await node.admin.call("sfu.ensureMembership", {
          neighbourhoodUrl: opts.neighbourhoodUrl,
          did,
        });
      }
    }
  }
}
