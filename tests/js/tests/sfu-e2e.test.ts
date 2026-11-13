import { expect } from "chai";
import { execSync, spawn, spawnSync } from "node:child_process";
import { once } from "node:events";
import { mkdtempSync, readdirSync, readFileSync } from "node:fs";
import { createServer } from "node:http";
import type { AddressInfo } from "node:net";
import { tmpdir } from "node:os";
import { createInterface } from "node:readline";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { sfuScenario } from "../helpers/sfu/harness.js";
import { Call } from "../helpers/sfu/e2e/call.js";
import { Hearing, type Heard, PauseWatch, toneFor, toneSource, wrtc } from "../helpers/sfu/e2e/media.js";
import type { Snapshot } from "../helpers/sfu/e2e/protocol.js";
import { specPeerConnection } from "../helpers/sfu/e2e/webrtc.js";
import type { World } from "../helpers/sfu/e2e/world.js";
import { pollUntil } from "../utils/utils.js";
import { e1TwoUsers } from "../helpers/sfu/e2e/scenarios/e1-two-users.js";
import { e2ManyUsers } from "../helpers/sfu/e2e/scenarios/e2-many-users.js";
import { e3TrackChanges } from "../helpers/sfu/e2e/scenarios/e3-track-changes.js";
import { e4JoinStorms } from "../helpers/sfu/e2e/scenarios/e4-join-storms.js";
import { e5AbruptLoss } from "../helpers/sfu/e2e/scenarios/e5-abrupt-loss.js";
import { e6Network } from "../helpers/sfu/e2e/scenarios/e6-network.js";
import { e7ExecutorRestart } from "../helpers/sfu/e2e/scenarios/e7-executor-restart.js";
import { e8Capacity } from "../helpers/sfu/e2e/scenarios/e8-capacity.js";
import { e9Simulcast } from "../helpers/sfu/e2e/scenarios/e9-simulcast.js";
import { e10Data } from "../helpers/sfu/e2e/scenarios/e10-data.js";
import { e11Outsider } from "../helpers/sfu/e2e/scenarios/e11-outsider.js";
import { e12Topology } from "../helpers/sfu/e2e/scenarios/e12-topology.js";
import { e13Mesh } from "../helpers/sfu/e2e/scenarios/e13-mesh.js";
import { e14AdvertisedAddress } from "../helpers/sfu/e2e/scenarios/e14-advertised-address.js";
import { e17NetworkSwitch } from "../helpers/sfu/e2e/scenarios/e17-network-switch.js";
import { e18NetworkChangeEvent } from "../helpers/sfu/e2e/scenarios/e18-network-change-event.js";
import { e19TabOpensSpace } from "../helpers/sfu/e2e/scenarios/e19-tab-opens-space.js";
import { e20TabTakesOver } from "../helpers/sfu/e2e/scenarios/e20-tab-takes-over.js";
import { e21TabGivesUp } from "../helpers/sfu/e2e/scenarios/e21-tab-gives-up.js";
import { e22TwoDevices } from "../helpers/sfu/e2e/scenarios/e22-two-devices.js";
import type { Scenario } from "../helpers/sfu/scenario.js";

/** `E2E_ONLY=e19,e21` runs only those scenarios; unset, all of them. */
const only = process.env.E2E_ONLY?.split(",").map((id) => id.trim()).filter(Boolean);
const registered: string[] = [];
const scenario = (s: Scenario, timeoutMs: number) => {
  registered.push(s.id);
  if (!only?.length || only.includes(s.id)) sfuScenario(s, timeoutMs);
};
/** An `E2E_ONLY` id that names no scenario would run none and pass: refuse it. */
const checkOnly = () => {
  const unknown = only?.filter((id) => !registered.includes(id)) ?? [];
  if (unknown.length) throw new Error(`E2E_ONLY names no scenario: ${unknown.join(", ")} (known: ${registered.join(", ")})`);
};

/**
 * The JavaScript side of an @roamhq/wrtc connection, as `specPeerConnection`
 * uses it: `fire()` is the binding's native ICE callback. As in the binding's
 * EventTarget, listeners run on the next tick and read the state then.
 */
class BindingConnection {
  iceConnectionState = "new";
  connectionState = "new";
  signalingState = "stable";
  oniceconnectionstatechange: ((event: unknown) => void) | null = null;
  private listeners = new Map<string, Set<(event: unknown) => void>>();
  addEventListener(type: string, listener: (event: unknown) => void) {
    if (!this.listeners.has(type)) this.listeners.set(type, new Set());
    this.listeners.get(type)!.add(listener);
  }
  removeEventListener(type: string, listener: (event: unknown) => void) {
    this.listeners.get(type)?.delete(listener);
  }
  dispatchEvent(event: { type: string }) {
    process.nextTick(() => {
      const handler = (this as Record<string, unknown>)[`on${event.type}`];
      const all = [...(this.listeners.get(event.type) ?? []), ...(typeof handler === "function" ? [handler as (event: unknown) => void] : [])];
      for (const listener of all) listener.call(this, event);
    });
    return true;
  }
  fire() {
    this.dispatchEvent({ type: "iceconnectionstatechange" });
  }
  async setLocalDescription() {}
  async setRemoteDescription() {}
  async addIceCandidate() {}
  async createOffer() {
    return { type: "offer", sdp: "" };
  }
  async createAnswer() {
    return { type: "answer", sdp: "" };
  }
  restartIce() {}
  close() {
    this.iceConnectionState = "closed";
    this.connectionState = "closed";
  }
}

/**
 * Calls end to end: managed users in real neighbourhoods, each calling
 * through the SDK in its own process. One executor without Holochain: needs
 * the test languages prepared in local mode (`pnpm run prepare-test-local`).
 */
describe("SFU: end to end through the SDK", function () {
  this.timeout(1_800_000);

  it("calls only through the SDK, as an app does", () => {
    // Raw call RPCs, membership shortcuts and the raw WebRTC peers would test
    // something other than what an app runs. E11's outsider uses raw RPCs on
    // purpose: it attacks the API rather than calling.
    const shortcut = /\bsfu(CallJoin|SendData|GetConfig|ListRooms)\b|ensureMembership|WebRtcPeer|"\.\.\/(\.\.\/)?(peer|client|cascade)\.js"/;
    const e2e = join(dirname(fileURLToPath(import.meta.url)), "../helpers/sfu/e2e");
    const files = (dir: string): string[] =>
      readdirSync(dir, { withFileTypes: true }).flatMap((e) => (e.isDirectory() ? files(join(dir, e.name)) : [join(dir, e.name)]));
    const offenders = files(e2e)
      .filter((file) => file.endsWith(".ts") && !file.endsWith("e11-outsider.ts"))
      .filter((file) => shortcut.test(readFileSync(file, "utf8")));
    expect(offenders).to.deep.equal([]);
  });

  it("E6's loopback impairment ends with its run, however the run ends", function () {
    // On a real kernel but in a throwaway network namespace: its own `lo` and
    // lock file, never the host's. The owners drop back to this user, so tc
    // goes through `sudo -n` as in E6.
    try {
      execSync("sudo -n unshare -n true", { stdio: "pipe" });
    } catch {
      this.skip();
    }
    const check = join(dirname(fileURLToPath(import.meta.url)), "../helpers/sfu/net-lease-check.ts");
    const lock = join(mkdtempSync(join(tmpdir(), "wt-net-")), "lock");
    const asUser = 'ip link set lo up && uid=$1 gid=$2 && shift 2 && exec setpriv --reuid="$uid" --regid="$gid" --init-groups -- "$@"';
    const run = spawnSync("sudo", [
      "-n", "env", `PATH=${process.env.PATH}:/usr/sbin:/sbin`, `HOME=${process.env.HOME}`, `WT_NET_LOCK=${lock}`,
      "unshare", "-n", "--", "sh", "-c", asUser, "sh", String(process.getuid!()), String(process.getgid!()),
      process.execPath, ...process.execArgv, check,
    ], { stdio: "inherit", timeout: 180_000 });
    expect(run.status).to.equal(0);
  });

  it("a participant process ends with the run that started it, however the run ends", async function () {
    // #1485: participants left behind by a run that ended (mocha --exit after a failed batch, a
    // SIGKILLed job) kept the CI log pipe open, and the job ran until its time limit. Here the
    // run is SIGKILLed while its participant joins through an API that never answers.
    if (!wrtc) this.skip();
    this.timeout(60_000);
    const api = createServer(() => {});
    // Both waits end before mocha's timeout, which would skip the `finally` and leave the run
    // behind: on their own deadline, or as soon as the run exits without getting there.
    const signal = AbortSignal.timeout(30_000);
    const connected = once(api, "connection", { signal });
    connected.catch(() => {});
    await new Promise<void>((resolve) => api.listen(0, "127.0.0.1", resolve));
    const script = join(dirname(fileURLToPath(import.meta.url)), "../helpers/sfu/e2e/orphan-run.ts");
    const run = spawn(process.execPath, [...process.execArgv, script, String((api.address() as AddressInfo).port)], {
      stdio: ["ignore", "pipe", "inherit"],
    });
    const exited = once(run, "exit", { signal }).then(([code, sig]) => {
      throw new Error(`the run exited (${sig ?? code}) before its participant joined`);
    });
    exited.catch(() => {});
    let pid = 0;
    const alive = () => {
      try {
        process.kill(pid, 0);
      } catch {
        return false;
      }
      // A process that ended but that nobody reaped yet (no reaping init in a container)
      // still answers signal 0. It holds no sockets or pipes any more: it ended.
      try {
        return !/^\d+ \(.*\) Z/.test(readFileSync(`/proc/${pid}/stat`, "utf8"));
      } catch {
        return true;
      }
    };
    try {
      const [line] = (await Promise.race([once(createInterface({ input: run.stdout! }), "line", { signal }), exited])) as [string];
      pid = Number(line);
      expect(pid, "the run prints its participant's PID").to.be.greaterThan(0);
      await Promise.race([connected, exited]);
      run.kill("SIGKILL");
      await pollUntil(() => !alive(), { timeoutMs: 5_000, intervalMs: 100, label: "the participant ends with its run" });
    } finally {
      run.kill("SIGKILL");
      if (pid && alive()) process.kill(pid, "SIGKILL");
      api.closeAllConnections();
      api.close();
    }
  });

  it("a user hears a participant on the connection they are on now, not on a closed one", async function () {
    // A rejoin closes the old connection: its tracks end with their last
    // half second of audio frozen in them. What the user hears of someone
    // must come from the track the session holds for them now.
    if (!wrtc) this.skip();
    this.timeout(60_000);
    const did = "did:test:speaker";
    const tones = [940, 1020];
    const hearing = new Hearing();
    let participants: Heard[] = [];
    const heard = () => hearing.remotes(participants, tones)[did];
    const speaker = async (hz: number) => {
      const source = toneSource(hz);
      const sending = new wrtc.RTCPeerConnection();
      const receiving = new wrtc.RTCPeerConnection();
      sending.onicecandidate = (e: any) => e.candidate && receiving.addIceCandidate(e.candidate);
      receiving.onicecandidate = (e: any) => e.candidate && sending.addIceCandidate(e.candidate);
      const track = new Promise<MediaStreamTrack>((resolve) => {
        receiving.ontrack = (e: any) => resolve(e.track);
      });
      sending.addTrack(source.track);
      await sending.setLocalDescription(await sending.createOffer());
      await receiving.setRemoteDescription(sending.localDescription);
      await receiving.setLocalDescription(await receiving.createAnswer());
      await sending.setRemoteDescription(receiving.localDescription);
      const stream: MediaStream = new wrtc.MediaStream([await track]);
      return { stream, close: () => { receiving.close(); sending.close(); source.stop(); } };
    };

    const before = await speaker(940);
    participants = [{ agentDid: did, stream: before.stream }];
    await pollUntil(() => heard()?.tone === 940, { timeoutMs: 15_000, label: "the first connection's tone" });
    before.close();

    const after = await speaker(1020);
    participants = [{ agentDid: did, stream: after.stream }];
    try {
      await pollUntil(() => heard()?.tone === 1020 && heard()!.audioRecent > 0, { timeoutMs: 15_000, label: "the new connection's tone" }).catch(() => {});
      expect(heard()?.tone, "the tone heard after the rejoin").to.equal(1020);
    } finally {
      after.close();
      hearing.stop();
    }
  });

  it("a hold does not count silence while the sender's or the receiver's process stood still", () => {
    // A participant's tone is a timer on its process's event loop. On a loaded
    // host a fresh process stood still for a second (CI's E20: "u2 hears
    // silence from u1:tab2" mid-hold): the call carried on, but nothing sent
    // the tone. Silence then is the harness's, not the call's; a wrong tone,
    // or silence with nobody paused, still breaks the hold.
    const users = { u1: { name: "u1", did: "did:test:u1" }, u2: { name: "u2", did: "did:test:u2" } };
    const call = new Call({ users } as unknown as World, "space", "room", () => {}, null);
    call.present.add("u1");
    call.present.add("u2");
    const snapshot = (from: { did: string; tone: number | null }, pausedMs = 0) => ({
      sessions: [{ agentDid: from.did, audioFrames: 500, audioRecent: 100, tone: from.tone, videoFrames: 0, videoRecent: 0, width: 0, height: 0 }],
      pausedMs,
    }) as unknown as Snapshot;
    const check = (u1: Snapshot, u2: Snapshot, holding: boolean) => call.hearsEveryone()(new Map([["u1", u1], ["u2", u2]]), holding);
    const fine = snapshot({ did: users.u2.did, tone: toneFor(1) });
    const hearsSilence = (pausedMs = 0) => snapshot({ did: users.u1.did, tone: null }, pausedMs);

    expect(check(fine, snapshot({ did: users.u1.did, tone: toneFor(0) }), true)).to.deep.equal([]);
    expect(check(fine, hearsSilence(), true), "silence, nobody paused").to.deep.equal([`u2 hears silence from u1, who sends ${toneFor(0)} Hz`]);
    expect(check(snapshot({ did: users.u2.did, tone: toneFor(1) }, 900), hearsSilence(), true), "the sender stood still").to.deep.equal([]);
    expect(check(fine, hearsSilence(900), true), "the receiver stood still").to.deep.equal([]);
    expect(check(snapshot({ did: users.u2.did, tone: toneFor(1) }, 120), hearsSilence(), true), "a pause too short to silence anything").to.have.length(1);
    expect(check(snapshot({ did: users.u2.did, tone: toneFor(1) }, 900), hearsSilence(), false), "waiting for audio: a pause excuses nothing").to.have.length(1);
    expect(
      check(snapshot({ did: users.u2.did, tone: toneFor(1) }, 900), snapshot({ did: users.u1.did, tone: toneFor(1) }), true),
      "a wrong tone always counts",
    ).to.deep.equal([`u2 hears ${toneFor(1)} Hz (u2's tone) from u1, who sends ${toneFor(0)} Hz`]);
  });

  it("a participant's SDK sees each ICE state the binding reports, also one the binding's event read too early", async function () {
    // @roamhq/wrtc fires `iceconnectionstatechange` when libwebrtc's legacy ICE
    // state changes, but `iceConnectionState` returns the standardized state,
    // which libwebrtc changes just after. On a loaded host a listener reads the
    // old state. In E17 the one event of the cut read "completed", the state
    // was "disconnected" 2 ms later, and no event followed: the SDK did not
    // restart ICE, and the call stayed silent (soak, 14 Nov). A browser fires
    // the event for the state it reports, so the connection must too.
    if (!wrtc) this.skip();
    const native = wrtc.RTCPeerConnection;
    let binding: BindingConnection | undefined;
    wrtc.RTCPeerConnection = function RTCPeerConnection() {
      return (binding = new BindingConnection());
    };
    let pc: RTCPeerConnection;
    try {
      pc = specPeerConnection();
    } finally {
      wrtc.RTCPeerConnection = native;
    }
    const seen: string[] = [];
    pc.oniceconnectionstatechange = () => seen.push(pc.iceConnectionState);
    try {
      binding!.iceConnectionState = "completed";
      binding!.fire();
      await pollUntil(() => seen.length === 1, { timeoutMs: 1_000, intervalMs: 5, label: "the first event" });
      // The legacy state goes disconnected. Its event reads the standardized
      // state before libwebrtc changes it; then it changes with no event.
      binding!.fire();
      await new Promise((resolve) => setImmediate(resolve));
      binding!.iceConnectionState = "disconnected";
      await pollUntil(() => seen.at(-1) === "disconnected", { timeoutMs: 3_000, intervalMs: 20, label: "disconnected" }).catch(() => {});
      expect(seen).to.deep.equal(["completed", "completed", "disconnected"]);
    } finally {
      pc.close();
    }
  });

  it("a participant knows when its process stood still", async () => {
    const pauses = new PauseWatch();
    try {
      await new Promise((r) => setTimeout(r, 100));
      expect(pauses.longestWithin(2_000), "a process left to run").to.be.below(400);
      const until = Date.now() + 400;
      while (Date.now() < until);
      expect(pauses.longestWithin(2_000), "while the pause is still on").to.be.at.least(400);
      await new Promise((r) => setTimeout(r, 100));
      expect(pauses.longestWithin(2_000), "after it").to.be.at.least(400);
      expect(pauses.longestWithin(50), "once it has passed out of the window").to.be.below(400);
    } finally {
      pauses.stop();
    }
  });

  scenario(e1TwoUsers, 600_000);
  scenario(e2ManyUsers, 1_200_000);
  scenario(e3TrackChanges, 900_000);
  scenario(e4JoinStorms, 1_200_000);
  scenario(e5AbruptLoss, 900_000);
  scenario(e6Network, 900_000);
  scenario(e7ExecutorRestart, 900_000);
  scenario(e8Capacity, 1_200_000);
  scenario(e9Simulcast, 600_000);
  scenario(e10Data, 600_000);
  scenario(e11Outsider, 600_000);
  scenario(e12Topology, 900_000);
  scenario(e13Mesh, 900_000);
  scenario(e14AdvertisedAddress, 600_000);
  scenario(e17NetworkSwitch, 600_000);
  scenario(e18NetworkChangeEvent, 600_000);
  scenario(e19TabOpensSpace, 600_000);
  scenario(e20TabTakesOver, 600_000);
  scenario(e21TabGivesUp, 600_000);
  scenario(e22TwoDevices, 600_000);
  checkOnly();
});
