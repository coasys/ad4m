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
import { Hearing, SCREEN, Source, toneSource, videoSource, wrtc } from "./media.js";
import type { Command, JoinOptions, Snapshot } from "./protocol.js";
import { installWebRtc, selectedRemoteAddresses } from "./webrtc.js";

installWebRtc();

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
  hearing: new Hearing(),
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
  if (session) state.hearing.attach(session.participants);
}

async function snapshot(): Promise<Snapshot> {
  const participants = session?.participants ?? [];
  const remotes = state.hearing.remotes(participants, options?.tones ?? []);
  return {
    joined: session?.getState() === "active",
    state: session?.getState() ?? "none",
    topology: state.topology,
    roster: [...state.roster],
    participants: participants.map((p) => p.agentDid),
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
    case "snapshot":
      return snapshot();
    case "close":
      state.hearing.stop();
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
