/**
 * WebRTC peer driver for SFU + mesh scenarios.
 *
 * Uses `@roamhq/wrtc` (a Node-native libwebrtc binding) so scenarios can
 * spawn N peers from a single test process without a browser harness.
 * The peer:
 *
 * - Generates a synthetic outbound stream (counter video, tone audio).
 *   Counter video lets us spot frame drops (the counter skips); the
 *   per-peer audio tone frequency lets the receiver verify active-speaker
 *   selection by reading received track energy.
 *
 * - Tracks `RTCPeerConnection.getStats()` at 1Hz, surfacing the inbound /
 *   outbound RTP and candidate-pair details that the scenarios assert on
 *   (bandwidth per peer, RTT, packet loss, simulcast layer in use).
 *
 * - Emits `'remote-track'`, `'ice-state'`, `'stats'` events for the
 *   scenario harness.
 *
 * The driver is signalling-agnostic: scenarios pass SDP between peers
 * (mesh) or to the executor SFU (T-series).  The peer itself just
 * owns its `RTCPeerConnection` lifecycle.
 */

import { EventEmitter } from "node:events";
import { createRequire } from "node:module";

// `@roamhq/wrtc` is the Node-native libwebrtc binding.  When it's not
// installed the scenarios fail at scenario-run time (not import time)
// with a clear error pointing at `npm install --include=optional`.
// We resolve it via `createRequire(import.meta.url)` because the wind
// tunnel project is ESM but `@roamhq/wrtc` exports CJS.
const require_ = createRequire(import.meta.url);
let wrtc: any;
try {
  wrtc = require_("@roamhq/wrtc");
} catch (_e) {
  wrtc = null;
}

export interface PeerStats {
  // Outbound (we are the sender)
  bytesSent: number;
  packetsSent: number;
  framesEncoded: number;
  // Inbound (we are the receiver) — totalled across all remote tracks
  bytesReceived: number;
  packetsReceived: number;
  packetsLost: number;
  jitter: number;
  framesDecoded: number;
  framesDropped: number;
  // Connection
  currentRoundTripTimeMs: number | null;
  selectedCandidatePair: string | null;
  /** "host" | "srflx" | "prflx" | "relay" — type of the local side of the nominated pair. */
  selectedLocalCandidateType: string | null;
  /** Same for the remote side. */
  selectedRemoteCandidateType: string | null;
  // For SFU scenarios
  simulcastLayerInUse: "f" | "h" | "q" | null;
  /** Per-rid outbound-rtp stats when simulcast encoding produces multiple layers. */
  simulcastLayers: Array<{
    rid: string;
    bytesSent: number;
    packetsSent: number;
    framesEncoded: number;
    active: boolean;
  }>;
}

// ---------------------------------------------------------------------------
// Goertzel filter — detect energy at a specific frequency in PCM samples.
//
// Returns the magnitude (unitless, proportional to amplitude²) for
// `targetHz` over `samples.length` samples at the given `sampleRate`.
// ---------------------------------------------------------------------------
export function goertzelMagnitude(
  samples: Int16Array,
  sampleRate: number,
  targetHz: number,
): number {
  const N = samples.length;
  if (N === 0) return 0;
  const k = Math.round((N * targetHz) / sampleRate);
  const w = (2 * Math.PI * k) / N;
  const coeff = 2 * Math.cos(w);
  let s1 = 0;
  let s2 = 0;
  for (let i = 0; i < N; i++) {
    const s0 = samples[i] + coeff * s1 - s2;
    s2 = s1;
    s1 = s0;
  }
  return Math.sqrt(s1 * s1 + s2 * s2 - coeff * s1 * s2);
}

/** Result of frequency detection on a single received audio track. */
export interface DetectedTone {
  /** Track ID (from the MediaStreamTrack). */
  trackId: string;
  /** Dominant frequency in Hz (highest Goertzel magnitude). */
  dominantHz: number;
  /** Goertzel magnitude at the dominant frequency. */
  magnitude: number;
  /** Total PCM samples analysed. */
  samplesAnalysed: number;
}

/**
 * Accumulates raw PCM from an RTCAudioSink and runs Goertzel detection
 * against a set of candidate frequencies once enough samples arrive.
 */
class AudioFingerprinter {
  private sink: any;
  private buffer: Int16Array[] = [];
  private totalSamples = 0;
  private readonly candidateHz: number[];
  private readonly sampleRate = 48000;
  /** Accumulate at least this many samples before analysing (~0.5s). */
  private readonly minSamples = 24000;
  private _result: { hz: number; magnitude: number } | null = null;

  constructor(track: any, candidateHz: number[], wrtcImpl: any) {
    this.candidateHz = candidateHz;
    const { RTCAudioSink } = wrtcImpl.nonstandard;
    this.sink = new RTCAudioSink(track);
    this.sink.ondata = (data: {
      samples: Int16Array;
      sampleRate: number;
    }) => {
      if (this._result) return; // already resolved
      this.buffer.push(new Int16Array(data.samples)); // copy — the buffer gets reused
      this.totalSamples += data.samples.length;
      if (this.totalSamples >= this.minSamples) {
        this.analyse();
      }
    };
  }

  private analyse(): void {
    // Concatenate buffered frames into one contiguous array.
    const all = new Int16Array(this.totalSamples);
    let offset = 0;
    for (const chunk of this.buffer) {
      all.set(chunk, offset);
      offset += chunk.length;
    }
    this.buffer = []; // free memory

    let bestHz = 0;
    let bestMag = 0;
    for (const hz of this.candidateHz) {
      const mag = goertzelMagnitude(all, this.sampleRate, hz);
      if (mag > bestMag) {
        bestMag = mag;
        bestHz = hz;
      }
    }
    this._result = { hz: bestHz, magnitude: bestMag };
  }

  get result(): { hz: number; magnitude: number } | null {
    return this._result;
  }

  get samplesCollected(): number {
    return this.totalSamples;
  }

  /** Clear accumulated data so the fingerprinter re-analyses fresh audio. */
  reset(): void {
    this._result = null;
    this.buffer = [];
    this.totalSamples = 0;
  }

  stop(): void {
    if (!this.sink.stopped) this.sink.stop();
  }
}

/** Per-layer encoding parameters for simulcast. */
export interface SimulcastEncoding {
  /** RTP stream ID — e.g. "f", "h", "q". */
  rid: string;
  /** Maximum bitrate in bps for this layer. */
  maxBitrate?: number;
  /** Scale factor (1 = full resolution, 2 = half, 4 = quarter). */
  scaleResolutionDownBy?: number;
}

export interface PeerOptions {
  /** Audio tone in Hz so receivers can identify the speaker. */
  audioToneHz?: number;
  /** Frame rate for the counter video. */
  videoFps?: number;
  /** ICE servers — defaults to Google STUN. */
  iceServers?: RTCIceServer[];
  /** ICE transport policy — "all" (default) or "relay" (force TURN). */
  iceTransportPolicy?: "all" | "relay";
  /**
   * Pre-allocate this many recv-only audio + video transceivers in the
   * initial offer.  The SFU's server-pushed renegotiation pipeline is
   * not wired into the wind tunnel client; without these slots,
   * remote peers' forwarded tracks have nowhere to land and
   * downloadMean stays at 0.  Set to (max-room-size - 1) on SFU
   * scenarios to surface the actual incoming bitrate.
   */
  recvSlots?: number;
  /**
   * Simulcast encodings for the video track.  When provided, the peer
   * uses `addTransceiver(track, { sendEncodings })` instead of
   * `addTrack`, and auto-bumps the video source to 1280x720 (unless
   * explicit videoWidth/videoHeight override).
   *
   * Order: lowest quality first (index 0 = smallest layer).
   * Example: [
   *   { rid: "q", maxBitrate: 150_000, scaleResolutionDownBy: 4 },
   *   { rid: "h", maxBitrate: 600_000, scaleResolutionDownBy: 2 },
   *   { rid: "f", maxBitrate: 2_500_000 },
   * ]
   */
  simulcastEncodings?: SimulcastEncoding[];
  /** Video source width — defaults to 320 (or 1280 with simulcast). */
  videoWidth?: number;
  /** Video source height — defaults to 240 (or 720 with simulcast). */
  videoHeight?: number;
  /** `wrtc` module override (for tests). */
  wrtcImpl?: any;
}

export class WebRtcPeer extends EventEmitter {
  readonly id: string;
  private pc: RTCPeerConnection;
  private localStream: MediaStream | null = null;
  private statsTimer: NodeJS.Timeout | null = null;
  private firstFrameAt: number | null = null;
  private lastStats: PeerStats | null = null;
  private fingerprinters: AudioFingerprinter[] = [];
  private fingerprintCandidates: number[] | null = null;
  private wrtcImpl: any;
  private audioToneHz: number;
  /** RIDs from simulcast encodings — drives SDP munging in acceptAnswer. */
  private simulcastRids: string[] = [];

  constructor(id: string, opts: PeerOptions = {}) {
    super();
    this.id = id;
    const impl = opts.wrtcImpl ?? wrtc;
    this.wrtcImpl = impl;
    this.audioToneHz = opts.audioToneHz ?? 440;
    if (opts.simulcastEncodings) {
      this.simulcastRids = opts.simulcastEncodings.map((e) => e.rid);
    }
    if (!impl) {
      throw new Error(
        "WebRtcPeer: @roamhq/wrtc not available. Run `npm install --include=optional @roamhq/wrtc` " +
          "to enable the WebRTC scenarios.",
      );
    }
    const { RTCPeerConnection } = impl;
    this.pc = new RTCPeerConnection({
      iceServers: opts.iceServers ?? [{ urls: "stun:stun.l.google.com:19302" }],
      iceTransportPolicy: opts.iceTransportPolicy ?? "all",
    } as any);
    this.pc.addEventListener("track", (event: any) => {
      if (this.firstFrameAt === null) this.firstFrameAt = Date.now();
      this.emit("remote-track", { track: event.track, streams: event.streams });
      // Auto-fingerprint audio tracks when enabled.
      if (
        this.fingerprintCandidates &&
        event.track &&
        event.track.kind === "audio"
      ) {
        const fp = new AudioFingerprinter(
          event.track,
          this.fingerprintCandidates,
          this.wrtcImpl,
        );
        this.fingerprinters.push(fp);
      }
    });
    this.pc.addEventListener("iceconnectionstatechange", () => {
      this.emit("ice-state", this.pc.iceConnectionState);
    });
  }

  /**
   * Enable audio fingerprinting on all future received audio tracks.
   * Pass the list of candidate frequencies (the tone Hz values used by
   * each peer in the room).  Call BEFORE media starts flowing.
   */
  enableAudioFingerprinting(candidateHz: number[]): void {
    this.fingerprintCandidates = candidateHz;
  }

  /**
   * Return frequency detection results for every received audio track.
   * Each entry maps a received track to the dominant frequency detected
   * in its PCM stream.  Returns only tracks that have collected enough
   * samples for analysis (~0.5s of audio).
   */
  /**
   * Clear accumulated fingerprint data on all active fingerprinters.
   * Use between test phases (e.g. pre/post peer departure) so the next
   * `getDetectedTones()` reflects only fresh audio.
   */
  resetFingerprinting(): void {
    for (const fp of this.fingerprinters) fp.reset();
  }

  getDetectedTones(): DetectedTone[] {
    const tones: DetectedTone[] = [];
    for (const fp of this.fingerprinters) {
      const r = fp.result;
      if (r) {
        tones.push({
          trackId: `track-${tones.length}`,
          dominantHz: r.hz,
          magnitude: r.magnitude,
          samplesAnalysed: fp.samplesCollected,
        });
      }
    }
    return tones;
  }

  /**
   * Generate and attach a synthetic outbound stream.  When `@roamhq/wrtc`
   * is available we use its `nonstandard.RTCAudioSource` / `RTCVideoSource`
   * to drive media without a real camera/microphone.
   */
  async attachSyntheticStream(opts: PeerOptions = {}): Promise<void> {
    const impl = opts.wrtcImpl ?? wrtc;
    if (!impl?.nonstandard) {
      throw new Error("WebRtcPeer: synthetic media requires @roamhq/wrtc nonstandard sources.");
    }
    const { RTCAudioSource, RTCVideoSource } = impl.nonstandard;

    // 32-bit PCM tone — 48 kHz, 10ms frames.
    const audioSource = new RTCAudioSource();
    const audioTrack = audioSource.createTrack();
    const audioToneHz = opts.audioToneHz ?? this.audioToneHz;
    const sampleRate = 48000;
    const samplesPerFrame = sampleRate / 100; // 10ms
    let t = 0;
    const audioTimer = setInterval(() => {
      const samples = new Int16Array(samplesPerFrame);
      for (let i = 0; i < samplesPerFrame; i++) {
        samples[i] = Math.round(Math.sin(2 * Math.PI * audioToneHz * t) * 8000);
        t += 1 / sampleRate;
      }
      audioSource.onData({
        samples,
        sampleRate,
        bitsPerSample: 16,
        channelCount: 1,
        numberOfFrames: samplesPerFrame,
      });
    }, 10);

    // Counter video — I420.  Defaults to 320x240; bumps to 1280x720
    // when simulcast encodings are configured (the encoder needs room
    // to scale down for lower layers).
    const videoSource = new RTCVideoSource();
    const videoTrack = videoSource.createTrack();
    const fps = opts.videoFps ?? 15;
    const hasSimulcast = (opts.simulcastEncodings ?? this.simulcastRids).length > 0;
    const width = opts.videoWidth ?? (hasSimulcast ? 1280 : 320);
    const height = opts.videoHeight ?? (hasSimulcast ? 720 : 240);
    const frameInterval = Math.round(1000 / fps);
    let frameCounter = 0;
    const yLen = width * height;
    const uvLen = (width / 2) * (height / 2);
    const videoTimer = setInterval(() => {
      const data = new Uint8Array(yLen + 2 * uvLen);
      // Generate spatially-varied luma so the encoder produces
      // meaningful bitrate (uniform flat frames compress to near-zero,
      // starving the bandwidth estimator and preventing simulcast
      // layer ramp-up).  The pattern shifts each frame so every frame
      // differs — the receiver can still spot skips via the counter
      // embedded in the top-left corner region.
      const v = (frameCounter * 3) & 0xff;
      for (let y = 0; y < height; y++) {
        const rowBase = y * width;
        for (let x = 0; x < width; x++) {
          data[rowBase + x] = ((x + y + v) * 7) & 0xff;
        }
      }
      data.fill(128, yLen, yLen + uvLen); // U
      data.fill(128, yLen + uvLen, yLen + 2 * uvLen); // V
      videoSource.onFrame({ width, height, data });
      frameCounter++;
    }, frameInterval);

    // Construct the MediaStream by adding tracks individually (the
    // wrtc shim exposes MediaStream on `impl.MediaStream`).
    const stream = new impl.MediaStream();
    stream.addTrack(audioTrack);
    stream.addTrack(videoTrack);
    this.localStream = stream;
    this.pc.addTrack(audioTrack, stream);

    // For simulcast: use addTransceiver with sendEncodings so libwebrtc
    // creates multiple encoder instances.  Without this, addTrack
    // creates a single-layer transceiver that ignores encodings set
    // later via setParameters.
    const simEnc = opts.simulcastEncodings;
    if (simEnc && simEnc.length > 0) {
      (this.pc as any).addTransceiver(videoTrack, {
        direction: "sendonly",
        sendEncodings: simEnc.map((e) => ({
          rid: e.rid,
          ...(e.maxBitrate != null ? { maxBitrate: e.maxBitrate } : {}),
          ...(e.scaleResolutionDownBy != null
            ? { scaleResolutionDownBy: e.scaleResolutionDownBy }
            : {}),
        })),
      });
      // Store rids if not already set from constructor.
      if (this.simulcastRids.length === 0) {
        this.simulcastRids = simEnc.map((e) => e.rid);
      }
    } else {
      this.pc.addTrack(videoTrack, stream);
    }

    // Pre-allocate recv-only transceivers if requested.  Each slot is
    // one audio + one video m-line in the initial offer; the SFU's
    // answer matches these and forwarding to them works without a
    // renegotiation round-trip from the server.
    const recvSlots = opts.recvSlots ?? 0;
    for (let i = 0; i < recvSlots; i++) {
      try {
        (this.pc as any).addTransceiver?.("audio", { direction: "recvonly" });
        (this.pc as any).addTransceiver?.("video", { direction: "recvonly" });
      } catch {
        // Some @roamhq/wrtc versions ignore addTransceiver; safe to
        // skip — the scenarios will fall back to downloadMean=0.
      }
    }

    // Hold timers so they live for the peer's lifetime.
    (this as any)._audioTimer = audioTimer;
    (this as any)._videoTimer = videoTimer;
  }

  /** Create an SDP offer (caller side). */
  async createOffer(): Promise<RTCSessionDescriptionInit> {
    const offer = await this.pc.createOffer();
    await this.pc.setLocalDescription(offer);
    return offer;
  }

  /** Accept an SDP offer and produce an answer (callee side). */
  async createAnswer(offer: RTCSessionDescriptionInit): Promise<RTCSessionDescriptionInit> {
    await this.pc.setRemoteDescription(offer);
    const answer = await this.pc.createAnswer();
    await this.pc.setLocalDescription(answer);
    return answer;
  }

  /**
   * Apply an SDP answer (caller side, after createOffer + signalling).
   *
   * When the peer has simulcast encodings configured, this method
   * auto-applies SDP munging to inject missing `a=rid`/`a=simulcast`
   * lines.  libwebrtc M106's `createAnswer()` strips these lines,
   * causing the sender to disable all layers except one.  Real SFUs
   * (str0m, mediasoup) produce proper simulcast answers — the munging
   * short-circuits when those lines already exist.
   */
  async acceptAnswer(answer: RTCSessionDescriptionInit): Promise<void> {
    let sdp = answer.sdp ?? "";
    if (this.simulcastRids.length > 0) {
      sdp = mungeAnswerForSimulcast(sdp, this.simulcastRids);
    }
    await this.pc.setRemoteDescription({ type: answer.type, sdp });
  }

  /** Begin 1Hz `getStats()` sampling.  Emits `'stats'` events. */
  startStats(): void {
    if (this.statsTimer) return;
    this.statsTimer = setInterval(async () => {
      try {
        const reports = await this.pc.getStats();
        const aggregated = aggregateStats(reports);
        this.lastStats = aggregated;
        this.emit("stats", aggregated);
      } catch (e) {
        this.emit("stats-error", e);
      }
    }, 1000);
  }

  stopStats(): void {
    if (this.statsTimer) {
      clearInterval(this.statsTimer);
      this.statsTimer = null;
    }
  }

  getLastStats(): PeerStats | null {
    return this.lastStats;
  }

  /** Time from peer construction to first remote-track event, or null. */
  getFirstFrameAt(): number | null {
    return this.firstFrameAt;
  }

  /** Internal: expose the underlying RTCPeerConnection for harness use only. */
  peerConnection(): RTCPeerConnection {
    return this.pc;
  }

  async close(): Promise<void> {
    this.stopStats();
    for (const fp of this.fingerprinters) fp.stop();
    this.fingerprinters = [];
    const at = (this as any)._audioTimer;
    const vt = (this as any)._videoTimer;
    if (at) clearInterval(at);
    if (vt) clearInterval(vt);
    if (this.localStream) {
      for (const track of this.localStream.getTracks()) track.stop();
    }
    this.pc.close();
  }
}

/** Roll up the noisy getStats() report into a single flat snapshot. */
function aggregateStats(reports: Map<string, any>): PeerStats {
  const stats: PeerStats = {
    bytesSent: 0,
    packetsSent: 0,
    framesEncoded: 0,
    bytesReceived: 0,
    packetsReceived: 0,
    packetsLost: 0,
    jitter: 0,
    framesDecoded: 0,
    framesDropped: 0,
    currentRoundTripTimeMs: null,
    selectedCandidatePair: null,
    selectedLocalCandidateType: null,
    selectedRemoteCandidateType: null,
    simulcastLayerInUse: null,
    simulcastLayers: [],
  };

  // Resolve candidate types by chasing localCandidateId / remoteCandidateId
  // through the report map after the loop.
  let localCandidateId: string | null = null;
  let remoteCandidateId: string | null = null;
  let inboundRtpCount = 0;
  for (const report of reports.values()) {
    if (report.type === "outbound-rtp") {
      stats.bytesSent += report.bytesSent ?? 0;
      stats.packetsSent += report.packetsSent ?? 0;
      stats.framesEncoded += report.framesEncoded ?? 0;
      if (report.rid) {
        stats.simulcastLayerInUse =
          (report.rid as "f" | "h" | "q") ?? stats.simulcastLayerInUse;
        stats.simulcastLayers.push({
          rid: report.rid as string,
          bytesSent: report.bytesSent ?? 0,
          packetsSent: report.packetsSent ?? 0,
          framesEncoded: report.framesEncoded ?? 0,
          active: (report.bytesSent ?? 0) > 0,
        });
      }
    } else if (report.type === "inbound-rtp") {
      stats.bytesReceived += report.bytesReceived ?? 0;
      stats.packetsReceived += report.packetsReceived ?? 0;
      stats.packetsLost += report.packetsLost ?? 0;
      stats.jitter += report.jitter ?? 0;
      inboundRtpCount++;
      stats.framesDecoded += report.framesDecoded ?? 0;
      stats.framesDropped += report.framesDropped ?? 0;
    } else if (report.type === "candidate-pair" && report.nominated) {
      stats.currentRoundTripTimeMs =
        report.currentRoundTripTime != null ? report.currentRoundTripTime * 1000 : null;
      stats.selectedCandidatePair = `${report.localCandidateId}↔${report.remoteCandidateId}`;
      localCandidateId = report.localCandidateId ?? null;
      remoteCandidateId = report.remoteCandidateId ?? null;
    }
  }
  // Average jitter across inbound-rtp reports (not sum).
  if (inboundRtpCount > 1) {
    stats.jitter /= inboundRtpCount;
  }
  if (localCandidateId) {
    const lc = reports.get(localCandidateId);
    stats.selectedLocalCandidateType = lc?.candidateType ?? null;
  }
  if (remoteCandidateId) {
    const rc = reports.get(remoteCandidateId);
    stats.selectedRemoteCandidateType = rc?.candidateType ?? null;
  }
  return stats;
}

// ---------------------------------------------------------------------------
// SDP munging — inject simulcast acknowledgment into answers that lack it.
// ---------------------------------------------------------------------------

/**
 * Inject `a=rid` and `a=simulcast:recv` lines into an SDP answer that
 * omits them.
 *
 * **Why this exists:** `@roamhq/wrtc` wraps libwebrtc M106.  When a
 * second `RTCPeerConnection` answers a simulcast offer via
 * `createAnswer()`, the answer SDP contains zero `a=rid:` and zero
 * `a=simulcast:` lines.  The offerer interprets this as "the remote
 * side does not want simulcast" and disables all layers except one.
 *
 * In SFU deployments, the SFU generates its own answer with proper
 * simulcast lines — so this function short-circuits (returns the SDP
 * unchanged) when `a=simulcast:` already appears.
 *
 * @param sdp  The raw SDP answer string.
 * @param rids The RTP stream IDs to acknowledge (e.g. ["q","h","f"]).
 * @returns    The (possibly munged) SDP string.
 */
export function mungeAnswerForSimulcast(sdp: string, rids: string[]): string {
  if (sdp.includes("a=simulcast:")) return sdp;
  if (rids.length === 0) return sdp;

  const lines = sdp.split("\r\n");
  const result: string[] = [];
  let inVideo = false;
  let inserted = false;

  for (const line of lines) {
    if (line.startsWith("m=video")) inVideo = true;
    else if (line.startsWith("m=") && !line.startsWith("m=video")) {
      inVideo = false;
    }

    result.push(line);

    // Insert right after a=recvonly or a=sendrecv in the video section.
    if (
      inVideo &&
      !inserted &&
      (line === "a=recvonly" || line === "a=sendrecv")
    ) {
      for (const rid of rids) {
        result.push(`a=rid:${rid} recv`);
      }
      result.push(`a=simulcast:recv ${rids.join(";")}`);
      inserted = true;
    }
  }

  return result.join("\r\n");
}

/** Manual SDP + ICE exchange between two peers — the mesh signalling helper.
 *
 * Trickle ICE: each peer's local candidates get added to the other peer's
 * connection as they arrive.  Without this the SDP advertises no
 * candidates and the peers never establish a media path (you see
 * `iceConnectionState === "checking"` indefinitely and zero bytes flow). */
export async function pairPeers(a: WebRtcPeer, b: WebRtcPeer): Promise<void> {
  a.peerConnection().addEventListener("icecandidate", (ev: any) => {
    if (ev.candidate) {
      b.peerConnection()
        .addIceCandidate(ev.candidate)
        .catch((e: any) => a.emit("ice-error", e));
    }
  });
  b.peerConnection().addEventListener("icecandidate", (ev: any) => {
    if (ev.candidate) {
      a.peerConnection()
        .addIceCandidate(ev.candidate)
        .catch((e: any) => b.emit("ice-error", e));
    }
  });

  const offer = await a.createOffer();
  const answer = await b.createAnswer(offer);
  await a.acceptAnswer(answer);
}
