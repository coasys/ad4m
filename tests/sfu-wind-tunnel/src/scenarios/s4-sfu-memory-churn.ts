/**
 * S4 (SFU): SFU memory under peer churn.
 *
 * Stress-tests the executor's memory stability by continuously
 * cycling peers in and out of an SFU room for 60 seconds.  Every 5
 * seconds a new peer joins and the oldest active peer leaves.
 *
 * Tracks the executor process's RSS at each iteration.  After the
 * churn window, asserts that RSS growth stayed < 50 MB (detecting
 * unbounded leaks in the SFU's per-peer state management).
 *
 * Reports the full RSS timeline for trend analysis.
 */

import { execSync } from "node:child_process";

import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { WebRtcPeer } from "../peer.js";
import { InstrumentedClient } from "../client.js";
import { provisionPeers, PeerSession, registerSfuMembers } from "../users.js";
import { wireRenegotiation, RenegotiationWire } from "../renegotiation.js";

const ROOM_NAME = "s4-sfu-memory-churn";
const NEIGHBOURHOOD = `windtunnel://s4-sfu`;
const CHURN_DURATION_MS = 60_000;
const CHURN_INTERVAL_MS = 5_000;

interface ActivePeer {
  peer: WebRtcPeer;
  session: PeerSession;
  wire: RenegotiationWire;
}

export const s4SfuMemoryChurn: Scenario = {
  id: "s4-sfu",
  name: "SFU memory under peer churn",
  description:
    "60 s of join/leave churn — track executor RSS, assert growth < 50 MB",

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { client: admin, branch, port } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};
    let passed = false;

    await admin.call("sfu.startRoom", {
      neighbourhoodUrl: NEIGHBOURHOOD,
      roomName: ROOM_NAME,
    });

    // Locate the executor's PID so we can sample its RSS.
    const executorPid = findListeningPid(port);
    metrics["executorPid"] = executorPid;
    if (!executorPid) {
      metrics["skipped"] = true;
      metrics["skipReason"] =
        "Could not determine executor PID for RSS tracking";
    }

    const activePeers: ActivePeer[] = [];
    const rssTimeline: Array<{ t: number; rssKb: number | null }> = [];
    let nextPeerId = 0;

    try {
      // Record initial RSS.
      const initialRss = executorPid ? readRssKb(executorPid) : null;
      rssTimeline.push({ t: 0, rssKb: initialRss });
      metrics["initialRssKb"] = initialRss;

      const loopStart = Date.now();
      while (Date.now() - loopStart < CHURN_DURATION_MS) {
        const iterStart = Date.now();
        const elapsed = iterStart - loopStart;

        // Leave the oldest peer if at least one active peer exists.
        if (activePeers.length > 0) {
          const oldest = activePeers.shift()!;
          await leavePeer(oldest);
        }

        // Join a new peer.
        const fresh = await joinPeer(admin, port, nextPeerId++);
        activePeers.push(fresh);

        // Sample RSS.
        const rss = executorPid ? readRssKb(executorPid) : null;
        rssTimeline.push({ t: elapsed, rssKb: rss });
        samples.push({
          name: `churn_iter_${nextPeerId - 1}`,
          durationMs: Date.now() - iterStart,
          timestamp: Date.now(),
        });

        // Pace the churn to one cycle every CHURN_INTERVAL_MS.
        const iterElapsed = Date.now() - iterStart;
        if (iterElapsed < CHURN_INTERVAL_MS) {
          await sleep(CHURN_INTERVAL_MS - iterElapsed);
        }
      }

      // Record final RSS.
      const finalRss = executorPid ? readRssKb(executorPid) : null;
      rssTimeline.push({
        t: Date.now() - loopStart,
        rssKb: finalRss,
      });
      metrics["finalRssKb"] = finalRss;

      // Compute growth.
      const initialVal = metrics["initialRssKb"] as number | null;
      const finalVal = metrics["finalRssKb"] as number | null;
      if (initialVal != null && finalVal != null) {
        const growthKb = finalVal - initialVal;
        const growthMb = growthKb / 1024;
        metrics["rssGrowthKb"] = growthKb;
        metrics["rssGrowthMb"] = Math.round(growthMb * 10) / 10;
        metrics["rssWithinBudget"] = growthMb < 50;
      } else {
        metrics["rssGrowthKb"] = null;
        metrics["rssGrowthMb"] = null;
        metrics["rssWithinBudget"] = null;
      }
      metrics["rssTimeline"] = rssTimeline;
      metrics["totalPeersJoined"] = nextPeerId;
      passed = metrics["rssWithinBudget"] === true;
    } finally {
      // Drain remaining active peers.
      for (const ap of activePeers) {
        try {
          await leavePeer(ap);
        } catch {
          /* best-effort */
        }
      }
      try {
        await admin.call("sfu.stopRoom", {
          neighbourhoodUrl: NEIGHBOURHOOD,
          roomName: ROOM_NAME,
        });
      } catch {
        /* best-effort */
      }
    }

    const endTime = Date.now();
    return {
      scenario: "s4-sfu-memory-churn",
      branch,
      passed,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      metrics,
      samples,
      summary:
        `S4: memory churn — peersJoined=${metrics["totalPeersJoined"]} ` +
        `initialRssKb=${metrics["initialRssKb"]} ` +
        `finalRssKb=${metrics["finalRssKb"]} ` +
        `growthMb=${metrics["rssGrowthMb"]} ` +
        `withinBudget=${metrics["rssWithinBudget"]}`,
    };
  },
};

// ── Helpers ──

async function joinPeer(
  admin: InstrumentedClient,
  port: number,
  idx: number,
): Promise<ActivePeer> {
  const [session] = await provisionPeers({
    admin,
    port,
    count: 1,
    labelPrefix: `s4-peer-${idx}`,
  });
  await registerSfuMembers({
    admin,
    neighbourhoodUrl: NEIGHBOURHOOD,
    sessions: [session],
  });
  const peer = new WebRtcPeer(session.label, {
    audioToneHz: 440 + (idx % 20) * 10,
  });
  await peer.attachSyntheticStream();
  const wire = await wireRenegotiation({
    client: session.client,
    peer,
    token: session.token,
    port,
    neighbourhoodUrl: NEIGHBOURHOOD,
    roomName: ROOM_NAME,
  });
  const offer = await peer.createOffer();
  const resp = await session.client.call<{
    sdpAnswer: string;
    participantId: string;
    redirectTo?: string;
    streamMapping: string[];
  }>("sfu.callJoin", {
    neighbourhoodUrl: NEIGHBOURHOOD,
    roomName: ROOM_NAME,
    sdpOffer: JSON.stringify(offer),
  });
  await peer.acceptAnswer(JSON.parse(resp.sdpAnswer));
  return { peer, session, wire };
}

async function leavePeer(ap: ActivePeer): Promise<void> {
  try {
    await ap.session.client.call("sfu.callLeave", {
      neighbourhoodUrl: NEIGHBOURHOOD,
      roomName: ROOM_NAME,
    });
  } catch {
    /* best-effort */
  }
  try {
    await ap.wire.detach();
  } catch {
    /* best-effort */
  }
  try {
    await ap.peer.close();
  } catch {
    /* best-effort */
  }
  try {
    await ap.session.client.disconnect();
  } catch {
    /* best-effort */
  }
}

/**
 * Find the PID of the process listening on the given TCP port.
 * Returns `null` if detection fails.
 */
function findListeningPid(port: number): string | null {
  // Try lsof first (most portable), then fall back to fuser.
  for (const cmd of [
    `lsof -t -i :${port} -sTCP:LISTEN 2>/dev/null | head -1`,
    `fuser ${port}/tcp 2>/dev/null | tr -dc '0-9 ' | awk '{print $1}'`,
  ]) {
    try {
      const out = execSync(cmd, { timeout: 5000 }).toString().trim();
      if (/^\d+$/.test(out)) return out;
    } catch {
      continue;
    }
  }
  return null;
}

/**
 * Read VmRSS from `/proc/<pid>/status`.
 * Returns RSS in kB, or `null` if the read fails.
 */
function readRssKb(pid: string): number | null {
  try {
    const out = execSync(`cat /proc/${pid}/status 2>/dev/null`, {
      timeout: 2000,
    }).toString();
    const match = out.match(/VmRSS:\s+(\d+)\s+kB/);
    return match ? parseInt(match[1], 10) : null;
  } catch {
    return null;
  }
}

function sleep(ms: number): Promise<void> {
  return new Promise((r) => setTimeout(r, ms));
}
