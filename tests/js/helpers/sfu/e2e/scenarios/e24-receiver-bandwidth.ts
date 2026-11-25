/**
 * E24: the SFU sends each receiver what its downlink can carry.
 *
 * Everyone sends its camera as three simulcast layers, and with a free
 * network every receiver gets full size. When one receiver's downlink (and
 * only its downlink) is capped at 700 kbit/s, the SFU's estimate of that
 * receiver's bandwidth falls and it must forward the low layer (a quarter of
 * the camera's size) to it, within SHAPED_MS, while the others keep full size.
 * Its video keeps arriving and everyone keeps hearing everyone. When the cap
 * is lifted, the estimate climbs and the receiver returns to full size.
 *
 * Needs `sudo -n tc`; skipped without it. Shares E6's host lease.
 */

import { leaseNet, netAvailable, socketPorts } from "../../net.js";
import type { Check } from "../call.js";
import type { Size, Snapshot } from "../protocol.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3"];
/** 720p: libwebrtc drops simulcast layers below its resolution thresholds, and a smaller camera sends fewer. */
const CAMERA: Size = { width: 1280, height: 720 };
const LOW: Size = { width: CAMERA.width / 4, height: CAMERA.height / 4 };
/**
 * The capped receiver's downlink. The premise, checked in the shaped step from
 * the senders' own numbers: the SFU steps a receiver down when its traffic
 * exceeds its estimate, and the estimate of a shaped link cannot exceed the
 * cap. So two low layers plus the audio must fit under the cap (the most the
 * estimate can be, and what the link carried in the shaped step is measured
 * beside it), while a low and a medium, or two mediums, plus the audio exceed
 * it, and must be stepped down.
 */
const CAP_KBIT = 700;
/** What the SFU budgets each audio track at least (`AUDIO_RESERVE_BPS`). */
const AUDIO_RESERVE_KBIT = 64;
/** Bytes the SFU adds to each RTP packet when it measures a layer (`PACKET_OVERHEAD_BYTES`). */
const PACKET_OVERHEAD_BYTES = 32;
/** From the cap to the capped receiver on the low layer. */
const SHAPED_MS = 45_000;
/** From u2 on low to every check true at once: the keyframes u2's switches asked for have arrived. */
const SETTLE_MS = 45_000;
/** How long the shaped state must hold, every check true at every instant: no oscillation, no audio gap. */
const STEADY_MS = 20_000;
/** A video that gets no new frame for this long has stalled: a sender's encoder pauses for up to 1.6 s when it starts, whatever the SFU does. */
const STALL_MS = 5_000;
/** From the cap's end to full size again: the estimate climbs, then an upgrade holds 4 s per layer. */
const RECOVERY_MS = 90_000;

export const e24ReceiverBandwidth = e2eScenario({
  id: "e24",
  name: "A receiver's bandwidth picks its simulcast layer",
  description: "One receiver's downlink is capped at 700 kbit/s: it gets the low layer, the others full size, audio keeps flowing; it recovers when the cap goes",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => (netAvailable() ? null : "needs passwordless `sudo -n tc` to shape the loopback interface"),
  async run({ call: inSpace, step, metric }) {
    const net = await leaseNet({ onWait: () => console.log("e24: waiting for another run's impairment on this host to end") });
    const call = inSpace("space", "e24", { video: CAMERA });
    /**
     * `name` receives everyone else's video now, at `size`. With `stallOk`, a
     * sender's encoder that paused for a moment passes (its size is not known
     * then): `advancing` bounds such a pause (up to 1.6 s when an encoder
     * starts, whatever the SFU does).
     */
    const receives = (name: string, size: Size, stallOk = false): Check => (snapshots) => {
      const snapshot = snapshots.get(name)!;
      const problems: string[] = [];
      for (const other of EVERYONE.filter((n) => n !== name)) {
        const remote = snapshot.remotes[call.who(other).did];
        if (!remote || remote.videoFrames === 0 || (!stallOk && remote.videoRecent === 0)) problems.push(`${name} receives no video from ${other}`);
        else if (remote.videoRecent === 0) continue;
        else if (remote.width !== size.width || remote.height !== size.height) {
          problems.push(`${name} receives ${other} at ${remote.width}x${remote.height}, expected ${size.width}x${size.height}`);
        }
      }
      return problems;
    };
    /** Every receiver gets new frames of everyone else's video, not one a second ago and none since. */
    const advancing = (): Check => {
      const last = new Map<string, { frames: number; at: number }>();
      return (snapshots) => {
        const now = Date.now();
        const problems: string[] = [];
        for (const [name, snapshot] of snapshots) {
          for (const other of EVERYONE.filter((n) => n !== name)) {
            const key = `${name}<-${other}`;
            const frames = snapshot.remotes[call.who(other).did]?.videoFrames ?? 0;
            const seen = last.get(key);
            if (!seen || frames > seen.frames) last.set(key, { frames, at: now });
            else if (now - seen.at > STALL_MS) problems.push(`${name} got no new frame from ${other} for ${now - seen.at} ms`);
          }
        }
        return problems;
      };
    };
    /** Every check, at the same instant: `sizes` per receiver, video advancing, every source tone heard. */
    const steady = (sizes: Record<string, Size>): Check => {
      const checks = [...Object.entries(sizes).map(([name, size]) => receives(name, size, true)), advancing(), call.hearsEveryone()];
      return (snapshots, holding) => checks.flatMap((check) => check(snapshots, holding));
    };
    try {
      await step("three join; with a free network everyone receives full size", async () => {
        await call.join(EVERYONE);
        await call.eventually(call.hearsEveryone(), 60_000, "audio");
        // A receiver starts on the low layer and climbs as its estimate allows.
        await call.eventually(call.seesEveryone(CAMERA), RECOVERY_MS, "full size");
      });

      metric("capped_to_low_ms", await step(`u2's downlink is capped at ${CAP_KBIT} kbit/s: u2 receives low within ${SHAPED_MS / 1000} s, the others full size`, async () => {
        const media = socketPorts(call.who("u2").pid).filter((s) => s.protocol === "udp");
        if (media.length === 0) throw new Error("u2 holds no media socket to cap");
        net.limitDownlink(media, CAP_KBIT);
        const capped = Date.now();
        await call.eventually(receives("u2", LOW), SHAPED_MS, "u2 at low");
        return Date.now() - capped;
      }));

      await step(`under the cap for ${STEADY_MS / 1000} s: u2 stays on low, u1 and u3 on full size, video advances, everyone hears everyone`, async () => {
        // u2's switches to low asked u1 and u3 for keyframes, and their encoders pause their other layers for a moment: the transition is over once everyone is back.
        await call.eventually(steady({ u2: LOW, u1: CAMERA, u3: CAMERA }), SETTLE_MS, "the shaped state settles");
        const counters = () => {
          const read = net.shaperCounters();
          if (!read) throw new Error("the shaper's counters could not be read (no tbf qdisc in `tc -s qdisc show`)");
          return read;
        };
        const senders = ["u1", "u3"];
        const sentBefore = await call.snapshots();
        const before = counters();
        const from = Date.now();
        await call.holds(steady({ u2: LOW, u1: CAMERA, u3: CAMERA }), STEADY_MS, "the shaped state");
        const after = counters();
        const sentAfter = await call.snapshots();
        const seconds = (Date.now() - from) / 1000;

        // What u2's filter passed meanwhile: not zero, or the shaper sat on the wrong ports.
        const passedBytes = after.sentBytes - before.sentBytes;
        const dropped = after.droppedPackets - before.droppedPackets;
        if (!(passedBytes > 0)) throw new Error(`the shaper passed no bytes of u2's downlink in ${seconds.toFixed(1)} s`);
        metric("shaped_downlink_kbit", Math.round((passedBytes * 8) / 1000 / seconds));
        metric("shaped_dropped_packets", dropped);
        console.log(`e24: the shaper passed ${passedBytes} bytes and dropped ${dropped} packets in ${seconds.toFixed(1)} s (cap ${CAP_KBIT} kbit/s)`);

        // What the senders u2 receives offered meanwhile, as the SFU measures it: payload plus 32 bytes per packet.
        const rate = (name: string, pick: (sent: Snapshot["sent"]) => { bytes: number; packets: number } | undefined): number => {
          const a = pick(sentBefore.get(name)!.sent);
          const b = pick(sentAfter.get(name)!.sent);
          if (!a || !b || b.packets <= a.packets) throw new Error(`${name} sent nothing there in ${seconds.toFixed(1)} s`);
          return ((b.bytes - a.bytes + (b.packets - a.packets) * PACKET_OVERHEAD_BYTES) * 8) / 1000 / seconds;
        };
        const offered = Object.fromEntries(
          senders.map((name) => [
            name,
            {
              low: rate(name, (sent) => sent.video.low),
              medium: rate(name, (sent) => sent.video.medium),
              // The SFU budgets each audio track at least AUDIO_RESERVE.
              audio: Math.max(AUDIO_RESERVE_KBIT, rate(name, (sent) => sent.audio)),
            },
          ]),
        );
        for (const [name, layers] of Object.entries(offered)) {
          metric(`offered_low_kbit_${name}`, Math.round(layers.low));
          metric(`offered_medium_kbit_${name}`, Math.round(layers.medium));
          metric(`offered_audio_kbit_${name}`, Math.round(layers.audio));
        }
        const [a, b] = senders.map((name) => offered[name]);
        const fixed = a.audio + b.audio;
        const carried = (passedBytes * 8) / 1000 / seconds;
        // The allocator steps a track down when the traffic exceeds the estimate (cost > estimate), and the estimate is at most the cap.
        const twoLows = a.low + b.low + fixed;
        const lowAndMedium = Math.min(a.low + b.medium, b.low + a.medium) + fixed;
        const twoMediums = a.medium + b.medium + fixed;
        metric("fixed_kbit", Math.round(fixed));
        metric("cap_kbit", CAP_KBIT);
        metric("two_lows_cost_kbit", Math.round(twoLows));
        metric("low_and_medium_cost_kbit", Math.round(lowAndMedium));
        metric("two_mediums_cost_kbit", Math.round(twoMediums));
        console.log(`e24: cap ${CAP_KBIT} kbit/s, carried ${carried.toFixed(0)}; two lows + fixed ${twoLows.toFixed(0)}, a low and a medium + fixed ${lowAndMedium.toFixed(0)}, two mediums + fixed ${twoMediums.toFixed(0)} (fixed ${fixed.toFixed(0)})`);
        const broken: string[] = [];
        if (!(twoLows <= CAP_KBIT)) broken.push(`two lows + fixed (${twoLows.toFixed(0)}) do not fit under the cap`);
        if (!(lowAndMedium > CAP_KBIT)) broken.push(`a low and a medium + fixed (${lowAndMedium.toFixed(0)}) fit under the cap`);
        if (!(twoMediums > CAP_KBIT)) broken.push(`two mediums + fixed (${twoMediums.toFixed(0)}) fit under the cap`);
        if (broken.length) throw new Error(`the cap's premise fails at ${CAP_KBIT} kbit/s: ${broken.join("; ")}`);
      });

      metric("recovery_to_full_ms", await step("the cap is lifted: u2 receives full size again, and stays there", async () => {
        net.clear();
        const lifted = Date.now();
        await call.eventually(receives("u2", CAMERA), RECOVERY_MS, "u2 at full size");
        const took = Date.now() - lifted;
        // Only u2's size: u1 and u3 held full size under the cap. On one host,
        // u2 climbing back by about 2 Mbit/s can delay the others' packets and
        // dip their estimates for a step; video and audio still flow for all.
        await call.holds(steady({ u2: CAMERA }), STEADY_MS / 2, "full size after the cap");
        return took;
      }));
    } finally {
      await net.release();
    }
  },
});
