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

export function videoSource(width: number, height: number, fps = 15): Source {
  const source = new wrtc.nonstandard.RTCVideoSource();
  const track = source.createTrack();
  const luma = width * height;
  const chroma = (width / 2) * (height / 2);
  let n = 0;
  const timer = setInterval(() => {
    const data = new Uint8Array(luma + 2 * chroma);
    // A moving pattern, so the encoder spends real bits on it.
    const shift = (n++ * 3) & 0xff;
    for (let y = 0; y < height; y++) {
      for (let x = 0; x < width; x++) data[y * width + x] = ((x + y + shift) * 7) & 0xff;
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

  /** Attach a receiver to every track of every participant that has none yet. */
  attach(participants: readonly Heard[]): void {
    for (const participant of participants) {
      for (const track of participant.stream.getTracks()) {
        if (!this.receivers.has(track.id)) this.receivers.set(track.id, new Receiver(track));
      }
    }
  }

  /** Per participant: the frames arriving, the tone their audio carries, their video's size. */
  remotes(participants: readonly Heard[], tones: number[]): Record<string, Remote> {
    this.attach(participants);
    const held = new Set<string>();
    const remotes: Record<string, Remote> = {};
    for (const participant of participants) {
      const remote = (remotes[participant.agentDid] ??= { audioFrames: 0, audioRecent: 0, tone: null, videoFrames: 0, videoRecent: 0, width: 0, height: 0 });
      for (const track of participant.stream.getTracks()) {
        held.add(track.id);
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
      this.receivers.delete(id);
    }
    return remotes;
  }

  stop(): void {
    for (const receiver of this.receivers.values()) receiver.stop();
    this.receivers.clear();
  }
}
