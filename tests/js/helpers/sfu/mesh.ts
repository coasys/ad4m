/**
 * MeshHost — a participant in a mesh call.
 *
 * Each MeshHost maintains one `WebRtcPeer` (== one `RTCPeerConnection`)
 * per remote participant.  That's the only shape that works for mesh
 * with `@roamhq/wrtc`: a single `RTCPeerConnection` can't be both
 * offerer and answerer across multiple remote partners without
 * tripping the "Failed to set SSL role for the transport" DTLS error.
 *
 * Usage:
 *
 *   const a = new MeshHost("alice", { audioToneHz: 440 });
 *   const b = new MeshHost("bob",   { audioToneHz: 660 });
 *   const c = new MeshHost("carol", { audioToneHz: 880 });
 *   await connectAll([a, b, c]);
 *   // … wait, sample stats, etc.
 *   await Promise.all([a, b, c].map(h => h.close()));
 *
 * Stats are aggregated across the host's individual peers.  The
 * "upload" bandwidth for the host is the sum of bytesSent across all
 * of its peers (every remote is a separate sink).  In mesh that grows
 * as O(N-1).
 */

import { WebRtcPeer, pairPeers, PeerStats, PeerOptions } from "./peer.js";

export class MeshHost {
  readonly id: string;
  private peers = new Map<string, WebRtcPeer>();
  private opts: PeerOptions;

  constructor(id: string, opts: PeerOptions = {}) {
    this.id = id;
    this.opts = opts;
  }

  /** Get or lazily create the peer side for connection to `remoteId`. */
  async getPeer(remoteId: string): Promise<WebRtcPeer> {
    let p = this.peers.get(remoteId);
    if (p) return p;
    p = new WebRtcPeer(`${this.id}→${remoteId}`, this.opts);
    await p.attachSyntheticStream(this.opts);
    this.peers.set(remoteId, p);
    return p;
  }

  /** Begin 1Hz `getStats()` polling on all of this host's peers. */
  startStats(): void {
    for (const p of this.peers.values()) p.startStats();
  }

  stopStats(): void {
    for (const p of this.peers.values()) p.stopStats();
  }

  /** Total bytes sent across every peer == this host's upload bandwidth. */
  totalBytesSent(): number {
    let total = 0;
    for (const p of this.peers.values()) {
      total += p.getLastStats()?.bytesSent ?? 0;
    }
    return total;
  }

  totalBytesReceived(): number {
    let total = 0;
    for (const p of this.peers.values()) {
      total += p.getLastStats()?.bytesReceived ?? 0;
    }
    return total;
  }

  totalPacketsLost(): number {
    let total = 0;
    for (const p of this.peers.values()) {
      total += p.getLastStats()?.packetsLost ?? 0;
    }
    return total;
  }

  /** Worst (largest) RTT across peers, in ms.  Best-effort. */
  worstRttMs(): number | null {
    let worst: number | null = null;
    for (const p of this.peers.values()) {
      const r = p.getLastStats()?.currentRoundTripTimeMs;
      if (r == null) continue;
      worst = worst == null ? r : Math.max(worst, r);
    }
    return worst;
  }

  async close(): Promise<void> {
    for (const p of this.peers.values()) {
      await p.close().catch(() => {});
    }
    this.peers.clear();
  }

  peerCount(): number {
    return this.peers.size;
  }
}

/**
 * Connect every host to every other host (full mesh).  Returns the
 * total SDP+ICE round-trip wall time.
 */
export async function connectAll(hosts: MeshHost[]): Promise<number> {
  const start = Date.now();
  for (let i = 0; i < hosts.length; i++) {
    for (let j = i + 1; j < hosts.length; j++) {
      const a = await hosts[i].getPeer(hosts[j].id);
      const b = await hosts[j].getPeer(hosts[i].id);
      await pairPeers(a, b);
    }
  }
  return Date.now() - start;
}

export type { PeerStats };
