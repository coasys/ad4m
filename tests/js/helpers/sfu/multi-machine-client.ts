#!/usr/bin/env -S npx tsx
/**
 * SFU multi-machine client.  Runs on each test machine.
 *
 * Connects to a remote executor's SFU, joins a room with synthetic
 * media, collects WebRTC stats, captures a received video frame as
 * PNG, and outputs a JSON result object on stdout.
 *
 * Usage:
 *   npx tsx multi-machine-client.ts \
 *     --host <executor-host> --port 15200 \
 *     --token <JWT> --neighbourhood windtunnel://mm \
 *     --room mm-test --duration 30 --label <machine-id> \
 *     --output-dir /tmp/sfu-verify --tone-hz 440
 */

import { WebRtcPeer, PeerStats } from "./peer.js";
import { InstrumentedClient } from "./client.js";
import { wireRenegotiation } from "./renegotiation.js";
import { hostname } from "node:os";
import { mkdirSync, writeFileSync, existsSync } from "node:fs";
import { join } from "node:path";
import { execSync } from "node:child_process";
import { createRequire } from "node:module";

const require_ = createRequire(import.meta.url);
let wrtc: any;
try {
  wrtc = require_("@roamhq/wrtc");
} catch {
  wrtc = null;
}

interface ClientArgs {
  executorHost: string;
  executorPort: number;
  token: string;
  neighbourhoodUrl: string;
  roomName: string;
  durationSec: number;
  outputDir: string;
  label: string;
  audioToneHz: number;
}

function parseArgs(): ClientArgs {
  const argv = process.argv.slice(2);
  const get = (flag: string, def: string): string => {
    const i = argv.indexOf(flag);
    return i >= 0 && i + 1 < argv.length ? argv[i + 1] : def;
  };
  return {
    executorHost: get("--host", "127.0.0.1"),
    executorPort: Number(get("--port", "15200")),
    token: get("--token", ""),
    neighbourhoodUrl: get("--neighbourhood", "windtunnel://multi-machine"),
    roomName: get("--room", "multi-machine-verify"),
    durationSec: Number(get("--duration", "30")),
    outputDir: get("--output-dir", "/tmp/sfu-verify"),
    label: get("--label", hostname()),
    audioToneHz: Number(get("--tone-hz", "440")),
  };
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}

async function captureFrame(
  track: any,
  outputPath: string,
): Promise<{ width: number; height: number } | null> {
  if (!wrtc?.nonstandard?.RTCVideoSink) return null;
  const { RTCVideoSink } = wrtc.nonstandard;
  return new Promise((resolve) => {
    const sink = new RTCVideoSink(track);
    const timer = setTimeout(() => {
      sink.stop();
      resolve(null);
    }, 10_000);
    sink.onframe = ({ frame }: any) => {
      clearTimeout(timer);
      sink.stop();
      const { width, height, data } = frame;
      const yuvPath = outputPath.replace(/\.png$/, ".yuv");
      writeFileSync(yuvPath, Buffer.from(data));
      try {
        execSync(
          `ffmpeg -f rawvideo -pix_fmt yuv420p -s ${width}x${height} ` +
            `-i "${yuvPath}" -frames:v 1 -y "${outputPath}" 2>/dev/null`,
        );
        try {
          execSync(`rm -f "${yuvPath}"`);
        } catch {}
      } catch {
        // ffmpeg unavailable — YUV file remains as evidence
      }
      resolve({ width, height });
    };
  });
}

async function main(): Promise<void> {
  const args = parseArgs();
  mkdirSync(args.outputDir, { recursive: true });

  const result: Record<string, unknown> = {
    machine: args.label,
    hostname: hostname(),
    startTime: Date.now(),
  };

  let client: InstrumentedClient | null = null;
  let peer: WebRtcPeer | null = null;

  try {
    client = new InstrumentedClient({
      port: args.executorPort,
      host: args.executorHost,
      adminToken: args.token,
    });
    await client.connect();
    result.connected = true;

    peer = new WebRtcPeer(args.label, { audioToneHz: args.audioToneHz });
    await peer.attachSyntheticStream();

    const wire = await wireRenegotiation({
      client,
      peer,
      token: args.token,
      port: args.executorPort,
      host: args.executorHost,
      neighbourhoodUrl: args.neighbourhoodUrl,
      roomName: args.roomName,
    });

    // Create offer.  ICE candidates trickle in after setLocalDescription;
    // the SFU's answer carries the server's candidates, and the peer
    // initiates STUN bindings to those — the SFU learns the peer's
    // transport address from the incoming binding request (peer-reflexive).
    const offer = await peer.createOffer();

    const joinStart = Date.now();
    const joinResponse = await client.call<{
      sdpAnswer: string;
      participantId: string;
      redirectTo?: string;
    }>("sfu.callJoin", {
      neighbourhoodUrl: args.neighbourhoodUrl,
      roomName: args.roomName,
      sdpOffer: JSON.stringify(offer),
    });
    result.joinDurationMs = Date.now() - joinStart;
    result.participantId = joinResponse.participantId;
    if (joinResponse.redirectTo) {
      result.redirectTo = joinResponse.redirectTo;
    }

    await peer.acceptAnswer(JSON.parse(joinResponse.sdpAnswer));
    result.sdpExchangeComplete = true;

    // Collect stats and capture a frame eagerly when the first remote video arrives
    const statsHistory: PeerStats[] = [];
    peer.on("stats", (s: PeerStats) => statsHistory.push(s));

    const screenshotPath = join(args.outputDir, `${args.label}-received.png`);
    let framePromise: Promise<{ width: number; height: number } | null> | null = null;
    peer.on("remote-track", (ev: any) => {
      if (ev.track?.kind === "video" && !framePromise) {
        framePromise = captureFrame(ev.track, screenshotPath);
      }
    });

    peer.startStats();
    await sleep(args.durationSec * 1000);
    peer.stopStats();

    // Await the frame capture that started during the media session
    if (framePromise) {
      const frameInfo = await framePromise;
      if (frameInfo && existsSync(screenshotPath)) {
        result.screenshot = { path: screenshotPath, ...frameInfo };
      } else {
        const yuvFallback = screenshotPath.replace(/\.png$/, ".yuv");
        result.screenshot = existsSync(yuvFallback)
          ? { path: yuvFallback, format: "yuv420p", ...frameInfo }
          : { error: "no frame captured" };
      }
    } else {
      result.screenshot = { error: "no video track received" };
    }

    const lastStats = peer.getLastStats();
    result.renegotiations = wire.count();
    result.renegotiationFailures = wire.failures();
    result.statsSamples = statsHistory.length;

    if (lastStats) {
      result.bytesSent = lastStats.bytesSent;
      result.bytesReceived = lastStats.bytesReceived;
      result.packetsLost = lastStats.packetsLost;
      result.roundTripMs = lastStats.currentRoundTripTimeMs;
      result.candidatePair = lastStats.selectedCandidatePair;
      result.localCandidateType = lastStats.selectedLocalCandidateType;
      result.remoteCandidateType = lastStats.selectedRemoteCandidateType;
      result.framesEncoded = lastStats.framesEncoded;
      result.framesDecoded = lastStats.framesDecoded;
      result.framesDropped = lastStats.framesDropped;
      result.jitter = lastStats.jitter;
    }

    result.passed =
      (lastStats?.bytesSent ?? 0) > 0 && (lastStats?.bytesReceived ?? 0) > 0;

    await wire.detach();
    try {
      await client.call("sfu.callLeave", {
        neighbourhoodUrl: args.neighbourhoodUrl,
        roomName: args.roomName,
      });
    } catch {}
  } catch (err: any) {
    result.error = err.message;
    result.passed = false;
  } finally {
    if (peer) await peer.close().catch(() => {});
    if (client) await client.disconnect().catch(() => {});
  }

  result.endTime = Date.now();
  result.durationMs = (result.endTime as number) - (result.startTime as number);

  console.log("---RESULT---");
  console.log(JSON.stringify(result, null, 2));

  // Force exit — dangling wrtc timers can keep the event loop alive.
  process.exit(result.passed ? 0 : 1);
}

main().catch((err) => {
  console.error("Fatal:", err);
  process.exit(1);
});
