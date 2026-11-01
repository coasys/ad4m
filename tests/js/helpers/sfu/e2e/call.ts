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
import type { World } from "./world.js";

export type Check = (snapshots: Map<string, Snapshot>) => string[];

/** Every one of `checks`. */
export function all(...checks: Check[]): Check {
  return (snapshots) => checks.flatMap((check) => check(snapshots));
}

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

  async eventually(check: Check, timeoutMs: number, label: string): Promise<number> {
    const started = Date.now();
    let problems: string[] = [];
    for (;;) {
      problems = check(await this.snapshots());
      if (problems.length === 0) return Date.now() - started;
      if (Date.now() - started > timeoutMs) break;
      await new Promise((r) => setTimeout(r, 500));
    }
    const shown = problems.slice(0, 12).join("; ") + (problems.length > 12 ? `; … ${problems.length - 12} more` : "");
    throw new Error(`${label}: not true within ${timeoutMs} ms — ${shown}`);
  }

  /** `check` stays true for all of `durationMs`: the first problem fails it. */
  async holds(check: Check, durationMs: number, label: string): Promise<void> {
    const until = Date.now() + durationMs;
    while (Date.now() < until) {
      const problems = check(await this.snapshots());
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

  /** For every present participant, every other present participant: `test` returns a problem or null. */
  private pairs(test: (snapshot: Snapshot, other: string, me: string) => string | null): Check {
    return (snapshots) => {
      const problems: string[] = [];
      for (const [me, snapshot] of snapshots) {
        for (const other of this.present) {
          if (other === me) continue;
          const problem = test(snapshot, other, me);
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
   */
  hearsEveryone(): Check {
    return this.pairs((snapshot, other) => {
      const theirs = snapshot.sessions.filter((s) => s.agentDid === this.did(other) && s.audioRecent > 0);
      if (theirs.length === 0) return `hears nothing from ${other}`;
      const expected = this.tones.get(other);
      if (!theirs.some((s) => s.tone === expected)) {
        const heard = theirs.map((s) => (s.tone == null ? "silence" : `${s.tone} Hz (${this.toneOwner(s.tone)})`)).join(", ");
        return `hears ${heard} from ${other}, who sends ${expected} Hz`;
      }
      return null;
    });
  }

  /** Nobody hears their own tone back. */
  hearsNoEcho(): Check {
    return (snapshots) => [...snapshots]
      .filter(([me, s]) => s.sessions.some((r) => r.audioRecent > 0 && r.tone === this.tones.get(me)))
      .map(([me]) => `${me} hears its own tone`);
  }

  /** Video arriving now from every other participant, at `size` when given. */
  seesEveryone(size?: Size): Check {
    return this.pairs((snapshot, other) => {
      const remote = snapshot.remotes[this.did(other)];
      if (!remote || remote.videoRecent === 0) return `sees nothing from ${other}`;
      if (size && (remote.width !== size.width || remote.height !== size.height)) {
        return `sees ${other} at ${remote.width}x${remote.height}, expected ${size.width}x${size.height}`;
      }
      return null;
    });
  }

  /** Everyone else sees `name`'s video arriving now, at `size`. */
  seesFrom(name: string, size: Size): Check {
    return (snapshots) => {
      const problems: string[] = [];
      for (const [me, snapshot] of snapshots) {
        if (me === name) continue;
        const remote = snapshot.remotes[this.did(name)];
        if (!remote || remote.videoRecent === 0) problems.push(`${me} sees nothing from ${name}`);
        else if (remote.width !== size.width || remote.height !== size.height) {
          problems.push(`${me} sees ${name} at ${remote.width}x${remote.height}, expected ${size.width}x${size.height}`);
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
