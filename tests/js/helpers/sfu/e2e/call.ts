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
import type { Size, Snapshot } from "./protocol.js";
import type { World } from "./world.js";

export type Check = (snapshots: Map<string, Snapshot>) => string[];

export class Call {
  readonly participants: Participant[] = [];
  /** Who is meant to be in the call right now. */
  readonly present = new Set<string>();
  private tones = new Map<string, number>();

  constructor(
    private world: World,
    readonly neighbourhood: string,
    readonly room: string,
    private onStart: (participant: Participant) => void = () => {},
    private video: Size | null = CAMERA,
  ) {}

  /** Bring `names` into the call: each starts a process and joins. Concurrently unless `oneByOne`. */
  async join(names: string[], { oneByOne = false } = {}): Promise<void> {
    const joinOne = async (name: string) => {
      // A participant whose process ended (killed, or crashed) comes back as a new process.
      const existing = this.who(name, false);
      const participant = existing && !existing.exit ? existing : await this.start(name);
      await participant.join({
        port: participant.user.port,
        token: participant.user.token,
        neighbourhoodUrl: this.world.url(this.neighbourhood),
        room: this.room,
        tone: this.tones.get(participant.did)!,
        tones: [...this.tones.values()],
        video: this.video,
      });
      this.present.add(name);
    };
    if (oneByOne) for (const name of names) await joinOne(name);
    else await Promise.all(names.map(joinOne));
  }

  private async start(name: string): Promise<Participant> {
    const user = this.world.users[name];
    if (!user) throw new Error(`no user "${name}"`);
    // Every participant the call may ever hold gets its tone up front, so receivers can name them all.
    for (const [index, u] of Object.values(this.world.users).entries()) {
      if (!this.tones.has(u.did)) this.tones.set(u.did, toneFor(index));
    }
    const participant = await Participant.start(user);
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

  private did(name: string): string {
    return this.world.users[name].did;
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
        const expected = [...this.present].filter((n) => n !== me).sort();
        for (const [source, dids] of [["roster", snapshot.roster], ["participants", snapshot.participants]] as const) {
          const actual = dids.map((d) => this.nameOf(d)).sort();
          if (actual.join() !== expected.join()) problems.push(`${me}'s ${source} is [${actual}], expected [${expected}]`);
        }
      }
      return problems;
    };
  }

  /** Audio arriving now from every other participant, carrying *that* participant's tone. */
  hearsEveryone(): Check {
    return this.pairs((snapshot, other) => {
      const remote = snapshot.remotes[this.did(other)];
      if (!remote || remote.audioRecent === 0) return `hears nothing from ${other}`;
      const expected = this.tones.get(this.did(other));
      if (remote.tone !== expected) {
        const heard = remote.tone == null ? "silence" : `${remote.tone} Hz (${this.toneOwner(remote.tone)})`;
        return `hears ${heard} from ${other}, who sends ${expected} Hz`;
      }
      return null;
    });
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
    const did = [...this.tones].find(([, t]) => t === hz)?.[0];
    return did ? `${this.nameOf(did)}'s tone` : "nobody's tone";
  }

  async end(): Promise<void> {
    await Promise.all(this.participants.map((p) => p.close()));
  }
}
