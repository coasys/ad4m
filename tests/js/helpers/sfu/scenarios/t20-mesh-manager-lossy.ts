/**
 * T20: The SDK's `MeshManager` on real libwebrtc, over a signalling channel
 * that loses, delays and reorders messages.
 *
 * T18 and T19 drive peer connections by hand. This scenario runs the SDK's own
 * mesh — the code a `Session` uses in mesh topology — with three agents, each
 * a `MeshManager` on `@roamhq/wrtc` peer connections. The channel between them
 * models telepresence as it behaves: delivery is delayed and reordered, the
 * publisher hears a result for every send, the first description each agent
 * sends to each peer is lost, and so is one renegotiating offer sent after its
 * pair has connected.
 *
 *   3 agents join → every pair connects despite the lost handshakes →
 *   audio flows on every pair, both ways → one pair is rebuilt by hand and its
 *   audio comes back → one agent leaves → the other two stay connected.
 *
 * Both ways matters: the answerer's media travels in a second offer, sent once
 * the pair is up. Reordered behind the answer it follows, an impolite peer
 * reads that offer as glare and ignores it, and the pair stays `connected`
 * with media in one direction only — which this scenario caught.
 *
 * No executor: the signalling is in-process, so what is under test is the
 * negotiation, the recovery ladder and the media, not the transport.
 *
 * Asserts:
 *   - Every pair reaches `connected` with the opening descriptions dropped.
 *   - The mesh repaired at least one pair to get there.
 *   - Every agent receives audio from both others.
 *   - `reconnect()` rebuilds a pair and its audio comes back both ways.
 *   - After one agent leaves, the remaining pair is still connected.
 */

import { createRequire } from "node:module";
import { MeshManager, type SignallingChannel } from "@coasys/ad4m";
import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";

const require_ = createRequire(import.meta.url);
let wrtc: any;
try {
  wrtc = require_("@roamhq/wrtc");
} catch (_e) {
  wrtc = null;
}

const CALL_ID = "t20-mesh-manager-lossy";
const AGENTS = ["did:test:t20-alice", "did:test:t20-bob", "did:test:t20-carol"];
const CONNECT_TIMEOUT_MS = 90_000;

/**
 * A `@roamhq/wrtc` peer connection with the spec behaviours the binding lacks
 * and every current browser has, and which perfect negotiation relies on:
 *
 * - `setLocalDescription()` with no argument creates the offer or answer the
 *   signalling state calls for *when it runs*;
 * - an incoming offer in `have-local-offer` rolls the local one back;
 * - both run on one operations chain, so a description applied while an offer
 *   is being created waits its turn instead of racing it.
 */
function specPeerConnection(configuration: RTCConfiguration): RTCPeerConnection {
  // Host candidates only: the agents share a machine, and CI has no STUN route.
  const pc = new wrtc.RTCPeerConnection({ ...configuration, iceServers: [] });
  const setLocal = pc.setLocalDescription.bind(pc);
  const setRemote = pc.setRemoteDescription.bind(pc);
  let operations: Promise<unknown> = Promise.resolve();
  const chain = <T>(operation: () => Promise<T>): Promise<T> => {
    const result = operations.then(operation);
    operations = result.catch(() => {});
    return result;
  };
  pc.setLocalDescription = (description?: RTCSessionDescriptionInit) =>
    chain(async () => {
      if (description?.sdp) return setLocal(description);
      const type = description?.type ?? (pc.signalingState === "have-remote-offer" ? "answer" : "offer");
      if (type === "rollback") return setLocal({ type: "rollback" });
      return setLocal(type === "offer" ? await pc.createOffer() : await pc.createAnswer());
    });
  pc.setRemoteDescription = (description: RTCSessionDescriptionInit) =>
    chain(async () => {
      if (description.type === "offer" && pc.signalingState === "have-local-offer") {
        await setLocal({ type: "rollback" });
      }
      return setRemote(description);
    });
  return pc as RTCPeerConnection;
}

/**
 * Telepresence as the mesh meets it. Every message is delivered after a random
 * delay, so messages overtake each other. The first description each agent
 * sends to each peer is lost. Every send is reported to its publisher, lost or
 * not — the executor took it, which is all the real transport confirms.
 */
class LossyHub {
  private listeners = new Map<string, Set<(from: string, payload: unknown) => void>>();
  private results = new Map<string, Set<(result: { ok: boolean; ms: number }) => void>>();
  private droppedPairs = new Set<string>();
  dropped = 0;
  delivered = 0;
  renegotiationsDropped = 0;
  private timers = new Set<ReturnType<typeof setTimeout>>();

  channel(selfId: string): SignallingChannel {
    return {
      publish: (payload) => {
        const delay = 20 + Math.random() * 400;
        const to = (payload as { to?: unknown }).to;
        const kind = (payload as { kind?: unknown }).kind;
        const pair = `${selfId}->${String(to)}`;
        let lose = kind === "description" && typeof to === "string" && !this.droppedPairs.has(pair);
        if (lose) this.droppedPairs.add(pair);
        // And once, carol's renegotiating offer to bob: the offer that carries
        // her media, sent after the pair already reads `connected`.
        const description = (payload as { description?: RTCSessionDescriptionInit }).description;
        const mlines = description?.sdp?.match(/^m=/gm)?.length ?? 0;
        if (!lose && pair === `${AGENTS[2]}->${AGENTS[1]}` && description?.type === "offer" && mlines > 2 && !this.renegotiationsDropped) {
          lose = true;
          this.renegotiationsDropped += 1;
        }

        const timer = setTimeout(() => {
          this.timers.delete(timer);
          for (const cb of this.results.get(selfId) ?? []) cb({ ok: true, ms: delay });
          if (lose) {
            this.dropped += 1;
            return;
          }
          for (const [id, cbs] of this.listeners) {
            if (id === selfId) continue;
            for (const cb of cbs) cb(selfId, JSON.parse(JSON.stringify(payload)));
          }
          this.delivered += 1;
        }, delay);
        this.timers.add(timer);
      },
      onMessage: (cb) => {
        if (!this.listeners.has(selfId)) this.listeners.set(selfId, new Set());
        this.listeners.get(selfId)!.add(cb);
        return () => this.listeners.get(selfId)?.delete(cb);
      },
      onPublishResult: (cb) => {
        if (!this.results.has(selfId)) this.results.set(selfId, new Set());
        this.results.get(selfId)!.add(cb);
        return () => this.results.get(selfId)?.delete(cb);
      },
    };
  }

  close() {
    for (const timer of this.timers) clearTimeout(timer);
    this.timers.clear();
  }
}

interface Agent {
  id: string;
  mesh: MeshManager;
  stopAudio: () => void;
  recoveries: { peer: string; rung: string }[];
  /** Audio frames received, per remote agent. */
  frames: Map<string, number>;
  sinks: any[];
}

function syntheticAudio(toneHz: number): { stream: MediaStream; stop: () => void } {
  const source = new wrtc.nonstandard.RTCAudioSource();
  const track = source.createTrack();
  const sampleRate = 48_000;
  const samplesPerFrame = sampleRate / 100;
  let t = 0;
  const timer = setInterval(() => {
    const samples = new Int16Array(samplesPerFrame);
    for (let i = 0; i < samplesPerFrame; i++) {
      samples[i] = Math.round(Math.sin(2 * Math.PI * toneHz * t) * 8000);
      t += 1 / sampleRate;
    }
    source.onData({ samples, sampleRate, bitsPerSample: 16, channelCount: 1, numberOfFrames: samplesPerFrame });
  }, 10);
  return {
    stream: new wrtc.MediaStream([track]),
    stop: () => {
      clearInterval(timer);
      track.stop();
    },
  };
}

function allConnected(agents: Agent[]): boolean {
  return agents.every((agent) => {
    const states = agent.mesh.peerStates();
    return agents.every((other) => other === agent || states.get(other.id) === "connected");
  });
}

async function waitFor(condition: () => boolean, timeoutMs: number, what: string): Promise<number> {
  const start = Date.now();
  while (!condition()) {
    if (Date.now() - start > timeoutMs) throw new Error(`Timed out after ${timeoutMs}ms waiting for ${what}`);
    await sleep(100);
  }
  return Date.now() - start;
}

/**
 * Count audio frames arriving from each remote agent, on whatever audio
 * tracks that agent's stream holds now. Called repeatedly: a pair's media in
 * the answerer's direction arrives with a second negotiation, after the pair
 * already reads `connected`.
 */
function listen(agent: Agent): void {
  for (const participant of agent.mesh.getParticipants()) {
    if (!agent.frames.has(participant.did)) agent.frames.set(participant.did, 0);
    for (const track of participant.stream.getAudioTracks()) {
      if (agent.sinks.some((sink) => sink.track === track)) continue;
      const sink = new wrtc.nonstandard.RTCAudioSink(track);
      sink.track = track;
      sink.ondata = () => agent.frames.set(participant.did, (agent.frames.get(participant.did) ?? 0) + 1);
      agent.sinks.push(sink);
    }
  }
}

function hearsEveryone(agents: Agent[]): boolean {
  for (const agent of agents) listen(agent);
  return agents.every((agent) => agents.every((other) => other === agent || (agent.frames.get(other.id) ?? 0) > 0));
}

export const t20MeshManagerLossy: Scenario = {
  id: "t20",
  name: "Mesh manager over a lossy channel",
  description: "SDK MeshManager, 3 agents on libwebrtc, with lost, delayed and reordered signalling",
  managesOwnEnvironment: true,

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const { branch } = ctx;
    const startTime = Date.now();
    const samples: ScenarioResult["samples"] = [];
    const metrics: Record<string, unknown> = {};
    const failures: string[] = [];

    if (!wrtc?.nonstandard) {
      throw new Error("T20 needs @roamhq/wrtc (npm install --include=optional).");
    }

    // The mesh constructs `MediaStream`s for its outbound and inbound tracks.
    const previousMediaStream = (globalThis as any).MediaStream;
    (globalThis as any).MediaStream = wrtc.MediaStream;

    const hub = new LossyHub();
    const agents: Agent[] = [];

    try {
      for (const [i, id] of AGENTS.entries()) {
        const audio = syntheticAudio(440 + 220 * i);
        const mesh = new MeshManager({
          channel: hub.channel(id),
          callId: CALL_ID,
          selfId: id,
          createPeerConnection: specPeerConnection,
          sweepMs: 500,
        });
        const agent: Agent = { id, mesh, stopAudio: audio.stop, recoveries: [], frames: new Map(), sinks: [] };
        mesh.on("peer-recovery", (peer: string, attempt: { rung: string }) => {
          agent.recoveries.push({ peer, rung: attempt.rung });
        });
        mesh.on("error", (error: Error) => console.warn(`[t20] ${id}: ${error.message}`));
        await mesh.join(audio.stream);
        agents.push(agent);
      }

      // ── Every pair connects, though every opening description was lost ──
      for (const agent of agents) agent.mesh.setRoster(AGENTS);
      const connectMs = await waitFor(() => allConnected(agents), CONNECT_TIMEOUT_MS, "every pair to connect");
      samples.push({ name: "mesh_connected", durationMs: connectMs, timestamp: Date.now() });

      const repairs = agents.flatMap((agent) => agent.recoveries);
      metrics["descriptions_dropped"] = hub.dropped;
      metrics["messages_delivered"] = hub.delivered;
      metrics["repairs"] = repairs.length;
      metrics["repair_rungs"] = [...new Set(repairs.map((repair) => repair.rung))];
      if (hub.dropped === 0) failures.push("the channel dropped no description, so recovery was not exercised");
      if (repairs.length === 0) failures.push("every pair connected without a repair, despite lost descriptions");

      // ── Audio flows on every pair, both ways ─────────────────────────
      // Including carol → bob, whose renegotiating offer the channel lost
      // after the pair had already connected.
      try {
        const audioMs = await waitFor(() => hearsEveryone(agents), CONNECT_TIMEOUT_MS, "audio on every pair");
        samples.push({ name: "audio_everywhere", durationMs: connectMs + audioMs, timestamp: Date.now() });
      } catch (error) {
        failures.push(error instanceof Error ? error.message : String(error));
      }
      for (const agent of agents) {
        for (const other of agents) {
          if (other !== agent) metrics[`frames_${short(agent.id)}_from_${short(other.id)}`] = agent.frames.get(other.id) ?? 0;
        }
      }
      metrics["renegotiations_dropped"] = hub.renegotiationsDropped;

      // ── A pair rebuilt by hand reconnects, and its audio comes back ──
      const [alice, bob, carol] = agents;
      const before = alice.mesh.getParticipants().find((p) => p.did === bob.id)?.stream;
      alice.mesh.reconnect(bob.id);
      alice.frames.delete(bob.id);
      bob.frames.delete(alice.id);
      try {
        const reconnectMs = await waitFor(() => hearsEveryone([alice, bob]), CONNECT_TIMEOUT_MS, "alice and bob to hear each other again");
        samples.push({ name: "reconnect", durationMs: reconnectMs, timestamp: Date.now() });
      } catch (error) {
        failures.push(error instanceof Error ? error.message : String(error));
      }
      const after = alice.mesh.getParticipants().find((p) => p.did === bob.id)?.stream;
      if (!after || after === before) failures.push("reconnect did not give alice a new stream from bob");

      // ── One agent leaves; the others stay connected ───────────────────
      await carol.mesh.leave();
      alice.mesh.setRoster([alice.id, bob.id]);
      bob.mesh.setRoster([alice.id, bob.id]);
      await sleep(1_000);
      if (alice.mesh.getParticipants().length !== 1) failures.push("alice still holds carol after she left");
      if (alice.mesh.peerStates().get(bob.id) !== "connected") failures.push("alice and bob dropped when carol left");
    } catch (error) {
      failures.push(error instanceof Error ? error.message : String(error));
    } finally {
      for (const agent of agents) {
        for (const sink of agent.sinks) sink.stop();
        agent.stopAudio();
        await agent.mesh.destroy();
      }
      hub.close();
      (globalThis as any).MediaStream = previousMediaStream;
    }

    const passed = failures.length === 0;
    metrics["failures"] = failures;
    const endTime = Date.now();
    return {
      scenario: "t20",
      branch,
      startTime,
      endTime,
      durationMs: endTime - startTime,
      passed,
      metrics,
      samples,
      summary: passed
        ? `3-agent mesh connected over a lossy channel (${metrics["repairs"]} repairs), audio on every pair, reconnect and leave clean`
        : `FAILED: ${failures.join("; ")} — ${JSON.stringify(metrics)}`,
    };
  },
};

function short(did: string): string {
  return did.split("-").pop() ?? did;
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
