#!/usr/bin/env -S npx tsx
/**
 * Standalone peer-server for multi-machine WebRTC scenarios.
 *
 * Hosts a single `WebRtcPeer` exposed over HTTP so a remote
 * scenario-runner can drive it.  The wire shape is intentionally
 * dumb — POST JSON in, JSON out.  Designed for trusted LANs (no
 * auth); never expose this over the public internet.
 *
 * Endpoints:
 *
 *   POST /peer/start { audioToneHz?: number, videoFps?: number }
 *     Creates the peer, attaches synthetic media, returns { ok: true }.
 *
 *   POST /peer/offer { offer: RTCSessionDescriptionInit }
 *     Sets the remote offer and returns the local answer:
 *     { answer: RTCSessionDescriptionInit }.
 *
 *   POST /peer/answer { answer: RTCSessionDescriptionInit }
 *     Applies an answer that the remote produced from our offer.
 *
 *   POST /peer/createOffer  { }
 *     Returns { offer: RTCSessionDescriptionInit }.
 *
 *   POST /peer/iceCandidate { candidate: RTCIceCandidateInit }
 *     Accepts a single ICE candidate from the other side.
 *
 *   GET  /peer/iceCandidates
 *     Returns { candidates: RTCIceCandidateInit[] } — drained, the
 *     candidates we've produced since the last poll.
 *
 *   GET  /peer/stats
 *     Returns the most recent `PeerStats` snapshot.
 *
 *   POST /peer/startStats { }
 *     Begin 1Hz `getStats()` polling internally.
 *
 *   POST /peer/close { }
 *     Tears the peer down.  The process keeps running so a subsequent
 *     /peer/start can re-arm it.
 *
 * Usage:
 *   tsx src/peer-server.ts --port 7000 [--bind 0.0.0.0]
 */

import { createServer, IncomingMessage, ServerResponse } from "node:http";
import { WebRtcPeer } from "./peer.js";

interface Args {
  port: number;
  bind: string;
}

function parseArgs(argv: string[]): Args {
  let port = 7000;
  let bind = "0.0.0.0";
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--port" || a === "-p") port = Number(argv[++i]);
    else if (a.startsWith("--port=")) port = Number(a.split("=")[1]);
    else if (a === "--bind") bind = argv[++i];
    else if (a.startsWith("--bind=")) bind = a.split("=")[1];
  }
  return { port, bind };
}

let peer: WebRtcPeer | null = null;
let pendingIceCandidates: RTCIceCandidateInit[] = [];

function readBody(req: IncomingMessage): Promise<string> {
  return new Promise((resolve, reject) => {
    const chunks: Buffer[] = [];
    req.on("data", (c) => chunks.push(c));
    req.on("end", () => resolve(Buffer.concat(chunks).toString("utf8")));
    req.on("error", reject);
  });
}

function reply(res: ServerResponse, status: number, body: unknown): void {
  res.writeHead(status, { "content-type": "application/json" });
  res.end(JSON.stringify(body));
}

async function handle(req: IncomingMessage, res: ServerResponse): Promise<void> {
  try {
    const url = req.url ?? "/";
    if (req.method === "GET" && url === "/peer/iceCandidates") {
      const drained = pendingIceCandidates;
      pendingIceCandidates = [];
      return reply(res, 200, { candidates: drained });
    }
    if (req.method === "GET" && url === "/peer/stats") {
      return reply(res, 200, { stats: peer?.getLastStats() ?? null });
    }
    if (req.method === "GET" && url === "/peer/health") {
      return reply(res, 200, { ok: true, hasPeer: peer !== null });
    }

    if (req.method !== "POST") {
      return reply(res, 405, { error: "method not allowed" });
    }

    const bodyText = await readBody(req);
    const body = bodyText ? JSON.parse(bodyText) : {};

    switch (url) {
      case "/peer/start": {
        if (peer) await peer.close();
        peer = new WebRtcPeer("remote", {
          audioToneHz: body.audioToneHz,
          videoFps: body.videoFps,
        });
        // Capture our outbound ICE candidates so the remote can poll.
        peer.peerConnection().addEventListener("icecandidate", (ev: any) => {
          if (ev.candidate) pendingIceCandidates.push(ev.candidate);
        });
        await peer.attachSyntheticStream();
        return reply(res, 200, { ok: true });
      }
      case "/peer/createOffer": {
        if (!peer) return reply(res, 400, { error: "peer not started" });
        const offer = await peer.createOffer();
        return reply(res, 200, { offer });
      }
      case "/peer/offer": {
        if (!peer) return reply(res, 400, { error: "peer not started" });
        const answer = await peer.createAnswer(body.offer);
        return reply(res, 200, { answer });
      }
      case "/peer/answer": {
        if (!peer) return reply(res, 400, { error: "peer not started" });
        await peer.acceptAnswer(body.answer);
        return reply(res, 200, { ok: true });
      }
      case "/peer/iceCandidate": {
        if (!peer) return reply(res, 400, { error: "peer not started" });
        await peer.peerConnection().addIceCandidate(body.candidate);
        return reply(res, 200, { ok: true });
      }
      case "/peer/startStats": {
        if (!peer) return reply(res, 400, { error: "peer not started" });
        peer.startStats();
        return reply(res, 200, { ok: true });
      }
      case "/peer/close": {
        if (peer) await peer.close();
        peer = null;
        pendingIceCandidates = [];
        return reply(res, 200, { ok: true });
      }
      default:
        return reply(res, 404, { error: "not found", url });
    }
  } catch (err) {
    return reply(res, 500, {
      error: err instanceof Error ? err.message : String(err),
    });
  }
}

const args = parseArgs(process.argv.slice(2));
const server = createServer(handle);
server.listen(args.port, args.bind, () => {
  console.log(`[peer-server] listening on http://${args.bind}:${args.port}`);
});

process.on("SIGINT", async () => {
  console.log("[peer-server] SIGINT — closing");
  if (peer) await peer.close();
  server.close(() => process.exit(0));
});
