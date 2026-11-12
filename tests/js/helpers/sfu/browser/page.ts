/**
 * What runs inside each browser page: a call joined through the SDK's
 * browser build, as an app joins one, and a report of what arrived, read
 * from WebRTC's own stats and from the decoded media.
 *
 * `installCallHarness` is an init script: Playwright serialises it, so it
 * must not reach anything outside its own body. It records every peer
 * connection the page creates and exposes `window.__call` with `join`,
 * `report` and `leave`.
 */

/** What the page is told to join. */
export interface JoinArgs {
  executorUrl: string;
  token: string;
  neighbourhoodUrl: string;
  room: string;
  name: string;
}

/** What a page received from one remote participant. */
export interface Inbound {
  /** RTP packets and frames received on the participant's tracks, from each receiver's `inbound-rtp` stats. */
  audioPackets: number;
  videoPackets: number;
  framesDecoded: number;
  frameWidth: number;
  /** The payload types this page's connection receives that participant's media on, from the `codec` stats. */
  audioPt: number | null;
  videoPt: number | null;
  /**
   * How loud the participant's decoded audio is, 0..1. With `audioMeter`
   * "web-audio": the loudest sample a Web Audio analyser read in the last
   * three seconds. With "rtp-stats": the receiver's `inbound-rtp` `audioLevel`
   * (the decoder's recent peak), used where the page's AudioContext does not
   * run: Firefox keeps it `suspended` on a host with no audio output (no
   * PulseAudio/PipeWire), so its graph reads silence though audio decodes.
   */
  audioPeak: number;
  audioMeter: "web-audio" | "rtp-stats";
  /** Mean luminance (0..255) of the participant's video as drawn on a canvas, or null where it cannot be drawn. */
  luma: number | null;
}

/** The payload types one connection's descriptions put on opus and VP8. */
export interface SdpPayloadTypes {
  local: { opus: number | null; vp8: number | null };
  remote: { opus: number | null; vp8: number | null };
}

export interface PageReport {
  state: string;
  topology: string | null;
  /** The remote participants' DIDs, as the session lists them. */
  participants: string[];
  inbound: Record<string, Inbound>;
  /** The payload types this page sends on, from its `outbound-rtp` stats. */
  outbound: { audioPt: number | null; videoPt: number | null };
  sdp: SdpPayloadTypes[];
  /** Each live connection's states and candidates, for a failure's message. */
  connections: { connection: string; ice: string; signaling: string; local: string[]; remote: string[]; pair: string | null }[];
  joinError: string | null;
  sessionErrors: string[];
}

export function installCallHarness(): void {
  const w = window as any;
  const Native = w.RTCPeerConnection;
  const connections: RTCPeerConnection[] = [];
  w.RTCPeerConnection = class extends Native {
    constructor(...args: any[]) {
      super(...args);
      connections.push(this as unknown as RTCPeerConnection);
    }
  };

  let session: any = null;
  let topology: string | null = null;
  let joinError: string | null = null;
  const sessionErrors: string[] = [];
  let audio: AudioContext | null = null;
  /** Per remote audio track: its analyser and the peaks it read, with their times. */
  const meters = new Map<string, { analyser: AnalyserNode; peaks: { at: number; value: number }[] }>();

  const meter = (track: MediaStreamTrack) => {
    if (meters.has(track.id) || track.readyState === "ended") return;
    audio ??= new AudioContext();
    // Web Audio starts suspended without a user gesture in Firefox and WebKit; resuming needs none here.
    if (audio.state === "suspended") void audio.resume().catch(() => {});
    const analyser = audio.createAnalyser();
    audio.createMediaStreamSource(new MediaStream([track])).connect(analyser);
    meters.set(track.id, { analyser, peaks: [] });
  };
  const samples = new Float32Array(2048);
  setInterval(() => {
    const now = performance.now();
    for (const entry of meters.values()) {
      entry.analyser.getFloatTimeDomainData(samples);
      let peak = 0;
      for (const sample of samples) peak = Math.max(peak, Math.abs(sample));
      entry.peaks.push({ at: now, value: peak });
      while (entry.peaks.length && entry.peaks[0].at < now - 3_000) entry.peaks.shift();
    }
  }, 50);

  /** Put each participant's stream on a muted video element, as an app's tile does. */
  const render = () => {
    for (const participant of session?.participants ?? []) {
      let video = document.getElementById(participant.agentDid) as HTMLVideoElement | null;
      if (!video) {
        video = document.createElement("video");
        video.id = participant.agentDid;
        video.muted = true;
        video.autoplay = true;
        video.playsInline = true;
        video.width = 160;
        video.height = 120;
        document.body.append(video);
      }
      if (video.srcObject !== participant.stream) {
        video.srcObject = participant.stream;
        // Not awaited: headless WebKit never settles it, though frames decode.
        void video.play().catch(() => {});
      }
      for (const track of participant.stream.getAudioTracks()) meter(track);
    }
  };

  const canvas = document.createElement("canvas");
  canvas.width = 32;
  canvas.height = 24;
  const luma = (did: string): number | null => {
    const video = document.getElementById(did) as HTMLVideoElement | null;
    if (!video || video.videoWidth === 0) return null;
    const context = canvas.getContext("2d", { willReadFrequently: true })!;
    context.drawImage(video, 0, 0, canvas.width, canvas.height);
    const pixels = context.getImageData(0, 0, canvas.width, canvas.height).data;
    let sum = 0;
    for (let i = 0; i < pixels.length; i += 4) sum += 0.299 * pixels[i] + 0.587 * pixels[i + 1] + 0.114 * pixels[i + 2];
    return sum / (pixels.length / 4);
  };

  const payloadTypes = (sdp: string | undefined) => {
    const of = (codec: string) => {
      const match = sdp?.match(new RegExp(`a=rtpmap:(\\d+) ${codec}/`, "i"));
      return match ? Number(match[1]) : null;
    };
    return { opus: of("opus"), vp8: of("VP8") };
  };

  async function join(args: JoinArgs): Promise<void> {
    joinError = null;
    document.title = args.name;
    try {
      const client = new w.ad4m.Ad4mClient(args.executorUrl, args.token);
      const perspective = (await client.perspective.all()).find((p: any) => p.sharedUrl === args.neighbourhoodUrl);
      if (!perspective) throw new Error(`no perspective for ${args.neighbourhoodUrl}`);
      const proxy = perspective.getNeighbourhoodProxy();
      // As an app does (WE's call adapter): the space's stored policy decides the topology.
      const sfuConfig = await proxy.sfuConfig(args.neighbourhoodUrl);
      session = await proxy.createSession(args.room, {
        neighbourhoodUrl: args.neighbourhoodUrl,
        topology: sfuConfig.mode === "mesh" ? "mesh" : "auto",
        sfuConfig,
      });
      session.on("topology-changed", (t: string) => { topology = t; });
      session.on("error", (error: any) => { sessionErrors.push(String(error?.message ?? error)); });
      for (const event of ["participant-joined", "stream-added", "participant-updated"]) session.on(event, render);
      setInterval(render, 500);
      const stream = await navigator.mediaDevices.getUserMedia({ audio: true, video: { width: 320, height: 240 } });
      // Not awaited: a join that never completes shows in the reports, within the test's bound.
      session.join(stream).catch((error: any) => { joinError = String(error?.message ?? error); });
    } catch (error: any) {
      joinError = String(error?.message ?? error);
    }
  }

  async function report(): Promise<PageReport> {
    const live = connections.filter((pc) => pc.connectionState !== "closed");
    const inbound: Record<string, Inbound> = {};
    const now = performance.now();
    for (const participant of session?.participants ?? []) {
      const tracks = new Set(participant.stream.getTracks().map((t: MediaStreamTrack) => t.id));
      const counts: Inbound = {
        audioPackets: 0, videoPackets: 0, framesDecoded: 0, frameWidth: 0,
        audioPt: null, videoPt: null, audioPeak: 0, audioMeter: "web-audio", luma: luma(participant.agentDid),
      };
      let decodedLevel = 0;
      for (const pc of live) {
        for (const receiver of pc.getReceivers()) {
          if (!tracks.has(receiver.track.id)) continue;
          const stats = await receiver.getStats();
          stats.forEach((s: any) => {
            if (s.type !== "inbound-rtp") return;
            const pt = stats.get(s.codecId)?.payloadType ?? null;
            if (s.kind === "audio") {
              counts.audioPackets += s.packetsReceived ?? 0;
              counts.audioPt ??= pt;
              decodedLevel = Math.max(decodedLevel, s.audioLevel ?? 0);
            } else if (s.kind === "video") {
              counts.videoPackets += s.packetsReceived ?? 0;
              counts.framesDecoded += s.framesDecoded ?? 0;
              counts.frameWidth = Math.max(counts.frameWidth, s.frameWidth ?? 0);
              counts.videoPt ??= pt;
            }
          });
        }
      }
      for (const track of participant.stream.getAudioTracks()) {
        for (const { at, value } of meters.get(track.id)?.peaks ?? []) {
          if (at >= now - 3_000) counts.audioPeak = Math.max(counts.audioPeak, value);
        }
      }
      // No audio output, no Web Audio: measure what the decoder produced instead.
      if (audio && audio.state !== "running") {
        counts.audioPeak = decodedLevel;
        counts.audioMeter = "rtp-stats";
      }
      inbound[participant.agentDid] = counts;
    }
    const outbound = { audioPt: null as number | null, videoPt: null as number | null };
    for (const pc of live) {
      for (const sender of pc.getSenders()) {
        if (!sender.track) continue;
        const stats = await sender.getStats();
        stats.forEach((s: any) => {
          if (s.type !== "outbound-rtp") return;
          const pt = stats.get(s.codecId)?.payloadType ?? null;
          if (s.kind === "audio") outbound.audioPt ??= pt;
          if (s.kind === "video") outbound.videoPt ??= pt;
        });
      }
    }
    const describe = (c: any) => (c ? `${c.candidateType} ${c.protocol} ${c.address ?? c.ip}:${c.port}` : "?");
    const connectionReports: PageReport["connections"] = [];
    for (const pc of live) {
      const stats = await pc.getStats();
      const local: string[] = [];
      const remote: string[] = [];
      let pair: string | null = null;
      stats.forEach((s: any) => {
        if (s.type === "local-candidate") local.push(describe(s));
        if (s.type === "remote-candidate") remote.push(describe(s));
        if (s.type === "candidate-pair" && (s.nominated || s.selected) && s.state === "succeeded") {
          pair = `${describe(stats.get(s.localCandidateId))} -> ${describe(stats.get(s.remoteCandidateId))}`;
        }
      });
      connectionReports.push({ connection: pc.connectionState, ice: pc.iceConnectionState, signaling: pc.signalingState, local, remote, pair });
    }
    return {
      state: session?.getState() ?? "none",
      topology,
      participants: (session?.participants ?? []).map((p: any) => p.agentDid),
      inbound,
      outbound,
      sdp: live.map((pc) => ({ local: payloadTypes(pc.localDescription?.sdp), remote: payloadTypes(pc.remoteDescription?.sdp) })),
      connections: connectionReports,
      joinError,
      sessionErrors: [...sessionErrors],
    };
  }

  async function leave(): Promise<void> {
    await session?.leave();
  }

  /** The full descriptions of every live connection, for debugging a failure. */
  function descriptions(): { local: string | null; remote: string | null }[] {
    return connections
      .filter((pc) => pc.connectionState !== "closed")
      .map((pc) => ({ local: pc.localDescription?.sdp ?? null, remote: pc.remoteDescription?.sdp ?? null }));
  }

  w.__call = { join, report, leave, descriptions };
}

/** The page every user opens: it loads the SDK's ESM bundle and exposes it. */
export const PAGE = `<!doctype html>
<html><head><meta charset="utf-8"><title>call</title></head><body>
<script type="module">
import * as ad4m from "./ad4m.js";
window.ad4m = ad4m;
window.ad4mReady = true;
</script></body></html>`;
