/**
 * One participant: a managed user's SDK session, in its own process.
 *
 * Driven only the way an app drives a call — `createSession` on the user's
 * neighbourhood proxy, `join` with local media, `replaceTrack`, `sendData`,
 * `leave` — and reporting only what an app can observe: the session's
 * events, its participants, and the media that arrives on their streams.
 *
 * Its own process, so killing it is a closed tab, and a crash in the
 * libwebrtc binding ends this participant rather than the run.
 */

import { Ad4mClient, type Session } from "@coasys/ad4m";
import { Receiver, SCREEN, Source, toneSource, videoSource, wrtc } from "./media.js";
import type { Command, JoinOptions, Snapshot, Remote } from "./protocol.js";
import { installWebRtc, selectedRemoteAddresses } from "./webrtc.js";

installWebRtc();

// A browser's global object is an EventTarget that fires `online` when a
// network comes up; Node's is not. Give it that much, so the SDK hears a
// scenario's network change where it listens for one in a browser.
if (typeof (globalThis as any).addEventListener !== "function") {
  const target = new EventTarget();
  Object.assign(globalThis, {
    addEventListener: target.addEventListener.bind(target),
    removeEventListener: target.removeEventListener.bind(target),
    dispatchEvent: target.dispatchEvent.bind(target),
  });
}

let client: Ad4mClient | null = null;
let session: Session | null = null;
let options: JoinOptions | null = null;
let tone: Source | null = null;
let camera: Source | null = null;
let screen: Source | null = null;

const state = {
  topology: null as string | null,
  /** Who the session's events say is in the call. */
  roster: new Set<string>(),
  left: new Set<string>(),
  /** `participant-left` events so far; a rebuilt connection fires one per participant. */
  departures: 0,
  receivers: new Map<string, Receiver>(),
  /** Which participant each received track arrived for, by track id. */
  trackOwner: new Map<string, string>(),
  data: [] as { from: string; label: string; data: string; binary: boolean }[],
  errors: [] as string[],
  peerStates: {} as Record<string, string>,
  recoveries: 0,
};

function didOf(participant: any): string {
  return participant?.did ?? participant?.agentDid;
}

/** Attach a receiver to every track of every participant that has none yet. */
function receiveAll(): void {
  if (!session) return;
  for (const participant of session.participants) {
    const did = participant.agentDid;
    for (const track of participant.stream.getTracks()) {
      if (!state.receivers.has(track.id)) state.receivers.set(track.id, new Receiver(track));
      state.trackOwner.set(track.id, did);
    }
  }
}

async function snapshot(): Promise<Snapshot> {
  receiveAll();
  const remotes: Record<string, Remote> = {};
  const live = new Set(session?.participants.map((p) => p.agentDid) ?? []);
  for (const [trackId, receiver] of state.receivers) {
    const did = state.trackOwner.get(trackId)!;
    if (!live.has(did)) continue;
    const remote = (remotes[did] ??= { audioFrames: 0, audioRecent: 0, tone: null, videoFrames: 0, videoRecent: 0, width: 0, height: 0 });
    if (receiver.track.kind === "audio") {
      remote.audioFrames += receiver.frames;
      remote.audioRecent += receiver.recentFrames;
      remote.tone = remote.tone ?? receiver.dominantTone(options?.tones ?? []);
    } else {
      // The resolution of the track receiving most now: a replaced track keeps its last frame size.
      if (receiver.recentFrames > remote.videoRecent) {
        remote.width = receiver.width;
        remote.height = receiver.height;
      }
      remote.videoFrames += receiver.frames;
      remote.videoRecent = Math.max(remote.videoRecent, receiver.recentFrames);
    }
  }
  return {
    joined: session?.getState() === "active",
    state: session?.getState() ?? "none",
    topology: state.topology,
    roster: [...state.roster],
    participants: [...live],
    left: [...state.left],
    departures: state.departures,
    remotes,
    data: state.data,
    errors: state.errors,
    peerStates: state.peerStates,
    recoveries: state.recoveries,
    selectedRemotes: await selectedRemoteAddresses(),
  };
}

async function join(opts: JoinOptions): Promise<void> {
  options = opts;
  client ??= new Ad4mClient(`http://127.0.0.1:${opts.port}`, opts.token);
  const perspective = (await client.perspective.all()).find((p) => p.sharedUrl === opts.neighbourhoodUrl);
  if (!perspective) throw new Error(`no perspective for ${opts.neighbourhoodUrl}`);
  const proxy = perspective.getNeighbourhoodProxy();

  if (!session) {
    // As an app does (WE's call adapter): the space's stored policy decides the topology.
    const sfuConfig = await proxy.sfuConfig(opts.neighbourhoodUrl).catch(() => undefined);
    session = await proxy.createSession(opts.room, {
      neighbourhoodUrl: opts.neighbourhoodUrl,
      topology: sfuConfig?.mode === "mesh" ? "mesh" : "auto",
      ...(sfuConfig ? { sfuConfig } : {}),
    });
    session.on("topology-changed", (topology: string) => { state.topology = topology; });
    session.on("participant-joined", (p: any) => { state.roster.add(didOf(p)); state.left.delete(didOf(p)); receiveAll(); });
    session.on("participant-updated", () => receiveAll());
    session.on("stream-added", () => receiveAll());
    session.on("participant-left", (p: any) => { state.roster.delete(didOf(p)); state.left.add(didOf(p)); state.departures += 1; });
    const debug = (line: string) => {
      if (process.env.E2E_DEBUG) console.log(`[e2e ${process.env.E2E_WHO}] ${new Date().toISOString().slice(17, 23)} ${line}`);
    };
    session.on("peer-state", (did: string, peerState: string) => {
      state.peerStates[did] = peerState;
      debug(`peer ${did.slice(-6)} ${peerState}`);
    });
    session.on("peer-recovery", (did: string, attempt: { rung: string }) => {
      state.recoveries += 1;
      debug(`peer ${did.slice(-6)} recovery: ${attempt?.rung}`);
    });
    session.on("error", (error: unknown) => {
      state.errors.push(error instanceof Error ? error.message : String(error));
      console.error(`[${process.env.E2E_WHO}] session error:`, error);
    });
    session.onData((message: any) => {
      state.data.push({ from: message.senderDid, label: message.channelLabel, data: message.data, binary: message.binary });
    });
  }

  tone ??= toneSource(opts.tone);
  if (opts.video) camera ??= videoSource(opts.video.width, opts.video.height);
  const tracks = [tone.track, ...(camera ? [camera.track] : [])];
  await session.join(new wrtc.MediaStream(tracks));
}

async function run(command: Command): Promise<unknown> {
  switch (command.kind) {
    case "join":
      return join(command.options);
    case "rejoin":
      return join(options!);
    case "leave":
      await session?.leave();
      state.roster.clear();
      return;
    case "mute":
      return session!.replaceTrack(command.media, null);
    case "unmute":
      return session!.replaceTrack(command.media, command.media === "audio" ? tone!.track : camera!.track);
    case "shareScreen":
      screen ??= videoSource(SCREEN.width, SCREEN.height);
      return session!.replaceTrack("video", screen.track);
    case "stopScreen":
      return session!.replaceTrack("video", camera?.track ?? null);
    case "quality":
      return session!.setQualityPreference(command.preference);
    case "send":
      return session!.sendData(command.label, command.data, command.binary);
    case "networkChanged":
      (globalThis as unknown as EventTarget).dispatchEvent(new Event("online"));
      return;
    case "snapshot":
      return snapshot();
    case "close":
      for (const receiver of state.receivers.values()) receiver.stop();
      await session?.destroy().catch(() => {});
      for (const source of [tone, camera, screen]) source?.stop();
      setTimeout(() => process.exit(0), 50);
      return;
  }
}

process.on("message", async ({ id, command }: { id: number; command: Command }) => {
  try {
    const result = await run(command);
    process.send!({ id, ok: true, result });
  } catch (error) {
    process.send!({ id, ok: false, error: error instanceof Error ? error.message : String(error) });
  }
});
process.send!({ ready: true });
