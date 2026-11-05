/** A scenario's handle on one participant process (see `participant-child.ts`). */

import { ChildProcess, fork } from "node:child_process";
import { fileURLToPath } from "node:url";
import type { SfuQualityPreference } from "@coasys/ad4m";
import type { Command, JoinOptions, Snapshot } from "./protocol.js";
import type { WorldUser } from "./world.js";

const CHILD = fileURLToPath(new URL("./participant-child.ts", import.meta.url));
const COMMAND_TIMEOUT_MS = 60_000;

export class Participant {
  /** How the process ended, when it has: a signal is a crash or a kill, never a clean exit. */
  exit: { code: number | null; signal: NodeJS.Signals | null } | null = null;
  private killed = false;
  private next = 1;
  private pending = new Map<number, { resolve(v: unknown): void; reject(e: Error): void }>();

  private constructor(readonly user: WorldUser, private proc: ChildProcess) {
    proc.on("message", (message: any) => {
      const waiter = this.pending.get(message.id);
      if (!waiter) return;
      this.pending.delete(message.id);
      if (message.ok) waiter.resolve(message.result);
      else waiter.reject(new Error(`${user.name}: ${message.error}`));
    });
    proc.on("exit", (code, signal) => {
      this.exit = { code, signal };
      for (const waiter of this.pending.values()) waiter.reject(new Error(`${user.name}'s process ended (${signal ?? code})`));
      this.pending.clear();
    });
  }

  static async start(user: WorldUser): Promise<Participant> {
    const proc = fork(CHILD, [], {
      execArgv: process.execArgv,
      env: { ...process.env, E2E_WHO: user.name },
      stdio: ["ignore", "inherit", "inherit", "ipc"],
    });
    await new Promise<void>((resolve, reject) => {
      proc.once("message", (message: any) => (message?.ready ? resolve() : reject(new Error("unexpected first message"))));
      proc.once("exit", (code, signal) => reject(new Error(`${user.name}'s process ended before it was ready (${signal ?? code})`)));
    });
    return new Participant(user, proc);
  }

  get name(): string {
    return this.user.name;
  }

  get did(): string {
    return this.user.did;
  }

  /** The participant process's pid: whose sockets an impairment of this participant alone cuts. */
  get pid(): number {
    return this.proc.pid!;
  }

  /** The process died of a signal nobody sent: the libwebrtc binding crashed. */
  get crashed(): boolean {
    return !this.killed && this.exit?.signal != null;
  }

  private send<T = void>(command: Command): Promise<T> {
    if (this.exit) return Promise.reject(new Error(`${this.name}'s process has ended`));
    const id = this.next++;
    return new Promise<T>((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pending.delete(id);
        reject(new Error(`${this.name}: "${command.kind}" did not answer within ${COMMAND_TIMEOUT_MS} ms`));
      }, COMMAND_TIMEOUT_MS);
      this.pending.set(id, {
        resolve: (value) => { clearTimeout(timer); resolve(value as T); },
        reject: (error) => { clearTimeout(timer); reject(error); },
      });
      this.proc.send({ id, command });
    });
  }

  join(options: JoinOptions): Promise<void> { return this.send({ kind: "join", options }); }
  rejoin(): Promise<void> { return this.send({ kind: "rejoin" }); }
  leave(): Promise<void> { return this.send({ kind: "leave" }); }
  mute(media: "audio" | "video"): Promise<void> { return this.send({ kind: "mute", media }); }
  unmute(media: "audio" | "video"): Promise<void> { return this.send({ kind: "unmute", media }); }
  shareScreen(): Promise<void> { return this.send({ kind: "shareScreen" }); }
  stopScreen(): Promise<void> { return this.send({ kind: "stopScreen" }); }
  quality(preference: SfuQualityPreference): Promise<void> { return this.send({ kind: "quality", preference }); }
  sendData(label: string, data: string, binary = false): Promise<void> { return this.send({ kind: "send", label, data, binary }); }
  snapshot(): Promise<Snapshot> { return this.send<Snapshot>({ kind: "snapshot" }); }
  switchTo(topology: "mesh" | "sfu"): Promise<void> { return this.send({ kind: "switch", topology }); }

  /** End the process at once, as a closed tab or a lost device does: no leave, no goodbye. */
  kill(): void {
    this.killed = true;
    this.proc.kill("SIGKILL");
  }

  async close(): Promise<void> {
    if (this.exit) return;
    await this.send({ kind: "close" }).catch(() => {});
    await new Promise<void>((resolve) => {
      if (this.exit) return resolve();
      const timer = setTimeout(() => { this.kill(); resolve(); }, 5_000);
      this.proc.once("exit", () => { clearTimeout(timer); resolve(); });
    });
  }
}
