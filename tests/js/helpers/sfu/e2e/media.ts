/**
 * What a participant sends and what a receiver measures.
 *
 * Each participant sends its own tone, so a receiver can tell who it is
 * hearing from the audio alone and check that against the DID the SDK
 * attributed the track to. Video carries its resolution, which is how a
 * receiver tells a camera from a screen share and one simulcast layer from
 * another.
 */

import { createRequire } from "node:module";
import type { Remote } from "./protocol.js";

const require_ = createRequire(import.meta.url);
export const wrtc: any = (() => {
  try {
    return require_("@roamhq/wrtc");
  } catch {
    return null;
  }
})();

const SAMPLE_RATE = 48_000;
const SAMPLES_PER_FRAME = SAMPLE_RATE / 100;
/** Half a second of audio: enough for the tones below to stand apart. */
const ANALYSIS_SAMPLES = SAMPLE_RATE / 2;
/** Below this a track carries no tone worth naming: silence or noise. */
const TONE_THRESHOLD = 1e6;

/** The energy at `hz` in `samples` (Goertzel). */
function magnitudeAt(samples: Int16Array, hz: number): number {
  const k = Math.round((samples.length * hz) / SAMPLE_RATE);
  const coeff = 2 * Math.cos((2 * Math.PI * k) / samples.length);
  let s1 = 0;
  let s2 = 0;
  for (const sample of samples) {
    const s0 = sample + coeff * s1 - s2;
    s2 = s1;
    s1 = s0;
  }
  return Math.sqrt(s1 * s1 + s2 * s2 - coeff * s1 * s2);
}

/** The tone for the participant at `index`: far enough apart that half a second separates them. */
export function toneFor(index: number): number {
  return 300 + 80 * index;
}

/** What a participant's camera and screen share send: different sizes, so a receiver can tell them apart. */
export const CAMERA = { width: 640, height: 480 };
export const SCREEN = { width: 1280, height: 720 };

export interface Source {
  track: MediaStreamTrack;
  stop(): void;
}

export function toneSource(hz: number): Source {
  const source = new wrtc.nonstandard.RTCAudioSource();
  const track = source.createTrack();
  let t = 0;
  const timer = setInterval(() => {
    const samples = new Int16Array(SAMPLES_PER_FRAME);
    for (let i = 0; i < SAMPLES_PER_FRAME; i++) {
      samples[i] = Math.round(Math.sin(2 * Math.PI * hz * t) * 8000);
      t += 1 / SAMPLE_RATE;
    }
    source.onData({ samples, sampleRate: SAMPLE_RATE, bitsPerSample: 16, channelCount: 1, numberOfFrames: SAMPLES_PER_FRAME });
  }, 10);
  return { track, stop: () => { clearInterval(timer); track.stop(); } };
}

/**
 * Where this process's event loop stood still. The tone source is a timer on
 * that loop, and received audio reaches a `Receiver` through it: while the
 * loop stands still (a host too loaded to run the process), the participant
 * sends no tone and hears nothing, whatever the call does.
 */
export class PauseWatch {
  /** How often the watch runs: a gap well past this is a pause. */
  private static readonly TICK_MS = 20;
  /** Gaps shorter than this are timer jitter, not pauses. */
  private static readonly MIN_PAUSE_MS = 100;
  private last = Date.now();
  private pauses: { end: number; ms: number }[] = [];
  private timer: ReturnType<typeof setInterval>;

  constructor() {
    this.timer = setInterval(() => {
      const now = Date.now();
      const gap = now - this.last;
      this.last = now;
      if (gap >= PauseWatch.MIN_PAUSE_MS) this.pauses.push({ end: now, ms: gap });
      while (this.pauses.length && this.pauses[0].end < now - 60_000) this.pauses.shift();
    }, PauseWatch.TICK_MS);
    this.timer.unref();
  }

  /** The longest pause that lasted into the last `windowMs`, the current one included. */
  longestWithin(windowMs: number): number {
    const now = Date.now();
    const ongoing = now - this.last;
    const past = this.pauses.filter((p) => p.end >= now - windowMs).map((p) => p.ms);
    return Math.max(ongoing >= PauseWatch.MIN_PAUSE_MS ? ongoing : 0, ...past, 0);
  }

  stop(): void {
    clearInterval(this.timer);
  }
}

/** The ramp's period, in pixels (a power of two, for the mask). */
export const RAMP_SIZE = 512;
/** Smooth luma 48..208: a wide gradient the encoder predicts well. */
const RAMP = Uint8Array.from({ length: RAMP_SIZE }, (_, i) => Math.round(128 + 80 * Math.sin((2 * Math.PI * i) / RAMP_SIZE)));
const NOISE_SIZE = 1 << 20;
/** Random numbers 0..255, drawn once for the process (a seeded generator, so a run repeats). */
const NOISE = (() => {
  const noise = new Uint8Array(NOISE_SIZE);
  let state = 0x9e3779b9;
  for (let i = 0; i < NOISE_SIZE; i++) {
    state = (Math.imul(state, 1664525) + 1013904223) >>> 0;
    noise[i] = state >>> 24;
  }
  return noise;
})();

/**
 * A moving picture that compresses like camera video and still costs bits:
 * a smooth gradient that drifts, soft discs that move across it, and sensor
 * noise (a different draw each frame, `noise` levels wide). The encoder
 * spends its target on it, a keyframe is at most about twice a steady frame
 * (high layer: 17.6 kB steady, up to 32 kB), and frames are not dropped after
 * one. At 1280x720 a frame costs about 1.5 ms of CPU, 2% of a core at 15 fps.
 */
export function videoSource(width: number, height: number, fps = 15, noise = 10): Source {
  const source = new wrtc.nonstandard.RTCVideoSource();
  const track = source.createTrack();
  const luma = width * height;
  const chroma = (width / 2) * (height / 2);
  const discs = [0, 1, 2, 3].map((i) => ({
    radius: Math.round(height / (5 + i)),
    gain: i % 2 ? -70 : 70,
    speedX: 0.011 + 0.004 * i,
    speedY: 0.007 + 0.003 * i,
    phase: i * 1.7,
  }));
  let n = 0;
  const timer = setInterval(() => {
    const data = new Uint8Array(luma + 2 * chroma);
    // Written through a clamping view: overlapping discs saturate, not wrap.
    const pixels = new Uint8ClampedArray(data.buffer);
    const frame = n++;
    const drift = frame * 2;
    // Noise in -noise/2 .. noise/2, from a window of the table that moves every frame.
    const offset = (frame * 7919) & (NOISE_SIZE - 1);
    const scale = noise / 256;
    const bias = noise / 2;
    for (let y = 0; y < height; y++) {
      const row = y * width;
      for (let x = 0; x < width; x++) {
        pixels[row + x] = RAMP[(x + y + drift) & (RAMP_SIZE - 1)] + NOISE[(row + x + offset) & (NOISE_SIZE - 1)] * scale - bias;
      }
    }
    for (const d of discs) {
      const cx = Math.round((0.5 + 0.4 * Math.sin(frame * d.speedX + d.phase)) * width);
      const cy = Math.round((0.5 + 0.4 * Math.cos(frame * d.speedY + d.phase)) * height);
      const r = d.radius;
      for (let y = Math.max(0, cy - r); y < Math.min(height, cy + r); y++) {
        const dy = y - cy;
        for (let x = Math.max(0, cx - r); x < Math.min(width, cx + r); x++) {
          const dx = x - cx;
          const q = (dx * dx + dy * dy) / (r * r);
          // A soft edge: the gain falls smoothly to 0 at the rim.
          if (q < 1) pixels[y * width + x] += d.gain * (1 - q) * (1 - q);
        }
      }
    }
    data.fill(128, luma);
    source.onFrame({ width, height, data });
  }, Math.round(1000 / fps));
  return { track, stop: () => { clearInterval(timer); track.stop(); } };
}

/** Frames arriving on one received track, with what they say. */
export class Receiver {
  frames = 0;
  width = 0;
  height = 0;
  private times: number[] = [];
  private audio: Int16Array[] = [];
  private audioSamples = 0;
  private sink: any;

  constructor(readonly track: MediaStreamTrack) {
    if (track.kind === "audio") {
      this.sink = new wrtc.nonstandard.RTCAudioSink(track);
      this.sink.ondata = ({ samples }: { samples: Int16Array }) => {
        this.tick();
        this.audio.push(new Int16Array(samples));
        this.audioSamples += samples.length;
        while (this.audioSamples - this.audio[0].length >= ANALYSIS_SAMPLES) {
          this.audioSamples -= this.audio.shift()!.length;
        }
      };
    } else {
      this.sink = new wrtc.nonstandard.RTCVideoSink(track);
      this.sink.onframe = ({ frame }: { frame: { width: number; height: number } }) => {
        this.tick();
        this.width = frame.width;
        this.height = frame.height;
      };
    }
  }

  private tick(): void {
    this.frames += 1;
    const now = Date.now();
    this.times.push(now);
    while (this.times.length && this.times[0] < now - 1000) this.times.shift();
  }

  /** Frames in the last second: whether media is arriving now, not whether it ever did. */
  get recentFrames(): number {
    const now = Date.now();
    return this.times.filter((t) => t >= now - 1000).length;
  }

  /** The candidate the last half second of audio is loudest at, or null for silence. */
  dominantTone(candidates: number[]): number | null {
    if (this.audioSamples < ANALYSIS_SAMPLES / 2) return null;
    const all = new Int16Array(this.audioSamples);
    let offset = 0;
    for (const chunk of this.audio) {
      all.set(chunk, offset);
      offset += chunk.length;
    }
    let best: number | null = null;
    let bestMagnitude = TONE_THRESHOLD;
    for (const hz of candidates) {
      const magnitude = magnitudeAt(all, hz);
      if (magnitude > bestMagnitude) {
        best = hz;
        bestMagnitude = magnitude;
      }
    }
    return best;
  }

  stop(): void {
    if (!this.sink.stopped) this.sink.stop();
  }
}

/** A participant as the SDK session reports it: who, and the stream their media arrives on. */
export interface Heard {
  agentDid: string;
  /** The participant's session; an agent on two devices is two participants. */
  participantId?: string;
  stream: MediaStream;
}

/**
 * What one user receives from everyone else, per participant: from the
 * tracks the session holds for each participant now. A track ends with its
 * connection, its last half second of audio frozen in it; a rejoin or a
 * rebuilt connection brings new tracks, and the old ones are no longer any
 * participant's, so what they froze is never read as what the user hears.
 */
export class Hearing {
  /** One receiver per track id, kept across snapshots so it collects frames. */
  private receivers = new Map<string, Receiver>();
  /** Who each receiver's track last belonged to, to credit its frames when the track goes. */
  private owners = new Map<string, Heard>();
  /** Frames of receivers whose track left, by the participant that held it. */
  private retired: { owner: Heard; audio: number; video: number }[] = [];

  /** Attach a receiver to every track of every participant that has none yet. */
  attach(participants: readonly Heard[]): void {
    for (const participant of participants) {
      for (const track of participant.stream.getTracks()) {
        if (!this.receivers.has(track.id)) this.receivers.set(track.id, new Receiver(track));
      }
    }
  }

  /**
   * Per participant — by DID unless `key` says otherwise — the frames
   * arriving, the tone their audio carries, their video's size. The frame
   * totals (`audioFrames`, `videoFrames`) never fall: a retired receiver's
   * frames stay in the total of the participant that held its track, so a
   * replaced track is not read as media that stopped. `audioRecent`,
   * `videoRecent`, the tone and the size come from the held tracks only.
   */
  remotes(participants: readonly Heard[], tones: number[], key: (p: Heard) => string = (p) => p.agentDid): Record<string, Remote> {
    this.attach(participants);
    const held = new Set<string>();
    const remotes: Record<string, Remote> = {};
    for (const participant of participants) {
      const remote = (remotes[key(participant)] ??= { audioFrames: 0, audioRecent: 0, tone: null, videoFrames: 0, videoRecent: 0, width: 0, height: 0 });
      for (const track of participant.stream.getTracks()) {
        held.add(track.id);
        this.owners.set(track.id, participant);
        const receiver = this.receivers.get(track.id)!;
        if (track.kind === "audio") {
          remote.audioFrames += receiver.frames;
          remote.audioRecent += receiver.recentFrames;
          remote.tone = remote.tone ?? receiver.dominantTone(tones);
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
    }
    for (const [id, receiver] of this.receivers) {
      if (held.has(id)) continue;
      receiver.stop();
      const owner = this.owners.get(id)!;
      this.retired.push({ owner, audio: receiver.track.kind === "audio" ? receiver.frames : 0, video: receiver.track.kind === "audio" ? 0 : receiver.frames });
      this.receivers.delete(id);
      this.owners.delete(id);
    }
    for (const { owner, audio, video } of this.retired) {
      const remote = remotes[key(owner)];
      if (!remote) continue;
      remote.audioFrames += audio;
      remote.videoFrames += video;
    }
    return remotes;
  }

  stop(): void {
    for (const receiver of this.receivers.values()) receiver.stop();
    this.receivers.clear();
  }
}
