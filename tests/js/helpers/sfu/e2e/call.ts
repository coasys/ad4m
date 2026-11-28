/**
 * One call in one neighbourhood room, and what its participants should observe.
 *
 * Expectations are checks that return what is wrong — "u3 hears nothing
 * from u7", "u5 hears 460 Hz from u2, who sends 380 Hz" — and `eventually`
 * retries them until the list is empty or time runs out, so a failure names
 * the pairs that did not work.
 */

import { CAMERA, toneFor } from "./media.js";
import { Participant } from "./participant.js";
import type { JoinOptions, Size, Snapshot } from "./protocol.js";
import { sleep, within } from "./time.js";
import type { World } from "./world.js";

/**
 * `holding`: the check runs inside `holds`, on a call that already worked,
 * rather than in `eventually`, waiting for it to work.
 */
export type Check = (snapshots: Map<string, Snapshot>, holding?: boolean) => string[];

/** Every one of `checks`. */
export function all(...checks: Check[]): Check {
  return (snapshots, holding) => checks.flatMap((check) => check(snapshots, holding));
}

/**
 * A participant process that stood still this long sent no tone for as long
 * (its source is a timer on that loop) and took in no audio: on a loaded
 * host, a fresh process can stall for a second. Half a second without the
 * tone is what a receiver calls silence, so a shorter pause cannot explain it.
 */
const PAUSE_MS = 250;

/** How often `eventually` looks again: root AGENTS.md asks for 100 ms or less when no event signals readiness. */
const POLL_MS = 100;

export class Call {
  readonly participants: Participant[] = [];
  /** Who is meant to be in the call right now. */
  readonly present = new Set<string>();
  /** Each participant's tone, by participant name. */
  private tones = new Map<string, number>();

  /**
   * `clients` names the participants beyond one per user: `user:client`, another
   * client of that user (a second tab, a phone), sending its own tone.
   */
  constructor(
    private world: World,
    readonly neighbourhood: string,
    readonly room: string,
    private onStart: (participant: Participant) => void = () => {},
    private video: Size | null = CAMERA,
    clients: string[] = [],
  ) {
    // Every participant the call may ever hold gets its tone up front, so receivers can name them all.
    for (const [index, name] of [...Object.keys(world.users), ...clients].entries()) this.tones.set(name, toneFor(index));
  }

  /**
   * Bring `names` into the call: each starts a process and joins. Concurrently unless `oneByOne`.
   * `device` is the device they call from (unset: each process its own); `failOffer` makes the join fail.
   */
  async join(names: string[], { oneByOne = false, device, failOffer }: { oneByOne?: boolean; device?: string; failOffer?: boolean } = {}): Promise<void> {
    const joinOne = async (name: string) => {
      // A participant whose process ended (killed, or crashed) comes back as a new process.
      const existing = this.who(name, false);
      const participant = existing && !existing.exit ? existing : await this.start(name);
      await participant.join({ ...this.options(participant, device), ...(failOffer ? { failOffer } : {}) });
      this.present.add(name);
    };
    if (oneByOne) for (const name of names) await joinOne(name);
    else await Promise.all(names.map(joinOne));
  }

  /** `name` opens the space's call session on `device` without joining, as a tab that shows the space. */
  async open(name: string, { device }: { device?: string } = {}): Promise<void> {
    const existing = this.who(name, false);
    const participant = existing && !existing.exit ? existing : await this.start(name);
    await participant.open(this.options(participant, device));
  }

  /** `name` destroys its session and its process ends, as a closed tab. */
  async close(name: string): Promise<void> {
    await this.who(name).close();
    this.present.delete(name);
  }

  private options(participant: Participant, device?: string): JoinOptions {
    return {
      port: participant.user.port,
      token: participant.user.token,
      neighbourhoodUrl: this.world.url(this.neighbourhood),
      room: this.room,
      tone: this.tones.get(participant.name)!,
      tones: [...this.tones.values()],
      video: this.video,
      ...(device ? { deviceId: device } : {}),
    };
  }

  private async start(name: string): Promise<Participant> {
    const user = this.world.users[userOf(name)];
    if (!user) throw new Error(`no user "${userOf(name)}"`);
    if (!this.tones.has(name)) throw new Error(`no tone for "${name}": name it in the call's clients`);
    const participant = await Participant.start(user, name);
    this.participants.push(participant);
    this.onStart(participant);
    return participant;
  }

  who(name: string): Participant;
  who(name: string, required: false): Participant | undefined;
  who(name: string, required = true): Participant | undefined {
    const found = [...this.participants].reverse().find((p) => p.name === name);
    if (!found && required) throw new Error(`${name} has not joined this call`);
    return found;
  }

  async leave(name: string): Promise<void> {
    await this.who(name).leave();
    this.present.delete(name);
  }

  async rejoin(name: string): Promise<void> {
    await this.who(name).rejoin();
    this.present.add(name);
  }

  /** End `name`'s process without a word, as a closed tab does. */
  kill(name: string): void {
    this.who(name).kill();
    this.present.delete(name);
  }

  async snapshots(): Promise<Map<string, Snapshot>> {
    const entries = await Promise.all(
      [...this.present].map(async (name) => [name, await this.who(name).snapshot()] as const),
    );
    return new Map(entries);
  }

  /** `names` join, then the call is an SFU call in which everyone lists, hears and sees everyone. */
  async settle(names: string[]): Promise<void> {
    await this.join(names);
    await this.eventually(this.topologyIs("sfu"), 30_000, "topology");
    await this.eventually(this.rosterIsEveryone(), 60_000, "roster");
    await this.eventually(this.hearsEveryone(), 60_000, "audio");
    await this.eventually(this.seesEveryone(CAMERA), 60_000, "video");
  }

  /**
   * Retry `check` until it is true, and return how long that took. Being true
   * counts only when the snapshots that show it came back within `timeoutMs`:
   * a snapshot started inside the bound and answered after it shows the call
   * after the bound, so it fails. Each snapshot waits at most the budget left.
   */
  async eventually(check: Check, timeoutMs: number, label: string): Promise<number> {
    const started = Date.now();
    const deadline = started + timeoutMs;
    let problems: string[] = [];
    for (;;) {
      try {
        problems = check(await within(deadline - Date.now(), this.snapshots(), `${label}: snapshots`), false);
      } catch (error) {
        problems = [error instanceof Error ? error.message : String(error)];
        break;
      }
      const took = Date.now() - started;
      if (problems.length === 0) {
        if (took <= timeoutMs) return took;
        problems = [`true only after ${took} ms`];
        break;
      }
      if (Date.now() >= deadline) break;
      await sleep(Math.min(POLL_MS, deadline - Date.now()));
    }
    const shown = problems.slice(0, 12).join("; ") + (problems.length > 12 ? `; … ${problems.length - 12} more` : "");
    throw new Error(`${label}: not true within ${timeoutMs} ms — ${shown}`);
  }

  /** `check` is true now, on one set of snapshots: there is no bound to wait out. */
  async now(check: Check, label: string): Promise<void> {
    const problems = check(await this.snapshots(), false);
    if (problems.length === 0) return;
    const shown = problems.slice(0, 12).join("; ") + (problems.length > 12 ? `; … ${problems.length - 12} more` : "");
    throw new Error(`${label}: not true — ${shown}`);
  }

  /** `check` stays true for all of `durationMs`: the first problem fails it. */
  async holds(check: Check, durationMs: number, label: string): Promise<void> {
    const until = Date.now() + durationMs;
    while (Date.now() < until) {
      const problems = check(await this.snapshots(), true);
      if (problems.length) throw new Error(`${label}: broke — ${problems.slice(0, 12).join("; ")}`);
      await new Promise((r) => setTimeout(r, 500));
    }
  }

  private did(name: string): string {
    return this.world.users[userOf(name)].did;
  }

  private nameOf(did: string): string {
    return Object.values(this.world.users).find((u) => u.did === did)?.name ?? did.slice(-8);
  }

  /**
   * For every present participant, every other present participant: `test` returns a problem or null.
   * `among` narrows both sides to those participants.
   */
  private pairs(test: (snapshot: Snapshot, other: string, me: string, snapshots: Map<string, Snapshot>, holding: boolean) => string | null, among?: string[]): Check {
    return (snapshots, holding = false) => {
      const problems: string[] = [];
      const others = among ? [...this.present].filter((name) => among.includes(name)) : [...this.present];
      for (const [me, snapshot] of snapshots) {
        if (among && !among.includes(me)) continue;
        for (const other of others) {
          if (other === me) continue;
          const problem = test(snapshot, other, me, snapshots, holding);
          if (problem) problems.push(`${me} ${problem}`);
        }
      }
      return problems;
    };
  }

  /** Each roster — from the session's events and its participants — is exactly the others present. */
  rosterIsEveryone(): Check {
    return (snapshots) => {
      const problems: string[] = [];
      for (const [me, snapshot] of snapshots) {
        const expected = [...this.present].filter((n) => n !== me).map(userOf).sort();
        for (const [source, dids] of [["roster", snapshot.roster], ["participants", snapshot.participants]] as const) {
          const actual = dids.map((d) => this.nameOf(d)).sort();
          if (actual.join() !== expected.join()) problems.push(`${me}'s ${source} is [${actual}], expected [${expected}]`);
        }
      }
      return problems;
    };
  }

  /**
   * Audio arriving now from every other participant, carrying *that* participant's tone,
   * on one of the participants the SDK attributes to their agent.
   *
   * In a hold, silence or nothing from a participant is not held against the
   * call while the sender's or the receiver's process has just stood still
   * (`pausedMs`): the harness, not the call, made no sound or heard none. A
   * wrong tone always counts, and so does silence still there once the pause
   * has passed out of the participants' window. `among` narrows it to those participants.
   */
  hearsEveryone(among?: string[]): Check {
    return this.pairs((snapshot, other, me, snapshots, holding) => {
      const theirs = snapshot.sessions.filter((s) => s.agentDid === this.did(other) && s.audioRecent > 0);
      const expected = this.tones.get(other);
      const wrongTone = theirs.some((s) => s.tone != null && s.tone !== expected);
      if (theirs.some((s) => s.tone === expected)) return null;
      if (holding && !wrongTone) {
        const paused = [me, other].filter((name) => (snapshots.get(name)?.pausedMs ?? 0) >= PAUSE_MS);
        if (paused.length) return null;
      }
      if (theirs.length === 0) return `hears nothing from ${other}`;
      const heard = theirs.map((s) => (s.tone == null ? "silence" : `${s.tone} Hz (${this.toneOwner(s.tone)})`)).join(", ");
      return `hears ${heard} from ${other}, who sends ${expected} Hz`;
    }, among);
  }

  /**
   * Audio and video frames from every other participant grew since `since`
   * (an earlier snapshot of the call, `spanMs` ago): the media flows now, not
   * only did once. A pair is skipped while `since` has no counters for it
   * (one of them joined after it) or while either process just stood still
   * (`pausedMs`: the harness stopped, not the call). `among` narrows it to
   * those participants.
   */
  framesGrow(since: Map<string, Snapshot>, spanMs: number, among?: string[]): Check {
    return this.pairs((snapshot, other, me, snapshots) => {
      if ([me, other].some((name) => (snapshots.get(name)?.pausedMs ?? 0) >= PAUSE_MS)) return null;
      const did = this.did(other);
      const now = snapshot.remotes[did];
      const then = since.get(me)?.remotes[did];
      if (!then) return null;
      const problems = ([["audio", "audioFrames"], ["video", "videoFrames"]] as const)
        .filter(([, key]) => !now || now[key] <= then[key])
        .map(([kind]) => `${kind} frames from ${other} did not grow over ${spanMs}ms`);
      return problems.length ? problems.join("; ") : null;
    }, among);
  }

  /** Nobody hears their own tone back. */
  hearsNoEcho(): Check {
    return (snapshots) => [...snapshots]
      .filter(([me, s]) => s.sessions.some((r) => r.audioRecent > 0 && r.tone === this.tones.get(me)))
      .map(([me]) => `${me} hears its own tone`);
  }

  /** Video arriving now from every other participant, at `size` when given; `among` narrows it to those participants. */
  seesEveryone(size?: Size, among?: string[]): Check {
    return this.pairs((snapshot, other) => {
      const remote = snapshot.remotes[this.did(other)];
      if (!remote || remote.videoRecent === 0) return `sees nothing from ${other}`;
      if (size && (remote.width !== size.width || remote.height !== size.height)) {
        return `sees ${other} at ${remote.width}x${remote.height}, expected ${size.width}x${size.height}`;
      }
      return null;
    }, among);
  }

  /**
   * Everyone else sees `name`'s video arriving now, at `size` or one of its
   * simulcast layers (`size` halved or quartered): each receiver's bandwidth
   * picks the layer, and e9 and e24 test that choice.
   */
  seesFrom(name: string, size: Size): Check {
    return (snapshots) => {
      const problems: string[] = [];
      for (const [me, snapshot] of snapshots) {
        if (me === name) continue;
        const remote = snapshot.remotes[this.did(name)];
        if (!remote || remote.videoRecent === 0) problems.push(`${me} sees nothing from ${name}`);
        else if (![1, 2, 4].some((d) => remote.width === size.width / d && remote.height === size.height / d)) {
          problems.push(`${me} sees ${name} at ${remote.width}x${remote.height}, expected ${size.width}x${size.height} or a layer of it`);
        }
      }
      return problems;
    };
  }

  /** Everyone else receives no video from `name` now. */
  noVideoFrom(name: string): Check {
    return (snapshots) => [...snapshots]
      .filter(([me, s]) => me !== name && (s.remotes[this.did(name)]?.videoRecent ?? 0) > 0)
      .map(([me]) => `${me} still sees video from ${name}`);
  }

  /** `name` sends no audio: everyone else hears silence from them, or no frames at all. */
  silentFrom(name: string): Check {
    return (snapshots) => {
      const problems: string[] = [];
      for (const [me, snapshot] of snapshots) {
        if (me === name) continue;
        const remote = snapshot.remotes[this.did(name)];
        if (remote && remote.audioRecent > 0 && remote.tone != null) problems.push(`${me} still hears ${remote.tone} Hz from ${name}`);
      }
      return problems;
    };
  }

  /** No present participant has seen anyone leave since `before` (absent there: since its start). */
  noDeparturesSince(before: Map<string, Snapshot> = new Map()): Check {
    return (snapshots) => [...snapshots]
      .filter(([me, s]) => s.departures > (before.get(me)?.departures ?? 0))
      .map(([me, s]) => `${me} saw ${s.departures - (before.get(me)?.departures ?? 0)} departure(s)`);
  }

  /** Nobody present reports a session error. */
  noErrors(): Check {
    return (snapshots) => [...snapshots].flatMap(([me, s]) => s.errors.map((e) => `${me} reported "${e}"`));
  }

  /** Every present participant resolved `topology`. */
  topologyIs(topology: string): Check {
    return (snapshots) => [...snapshots]
      .filter(([, s]) => s.topology !== topology)
      .map(([me, s]) => `${me} runs ${s.topology ?? "nothing"}, expected ${topology}`);
  }

  private toneOwner(hz: number): string {
    const name = [...this.tones].find(([, t]) => t === hz)?.[0];
    return name ? `${name}'s tone` : "nobody's tone";
  }

  async end(): Promise<void> {
    await Promise.all(this.participants.map((p) => p.close()));
  }
}

/** The user a participant name belongs to: `u1:phone` is u1's. */
export function userOf(name: string): string {
  return name.split(":")[0];
}
