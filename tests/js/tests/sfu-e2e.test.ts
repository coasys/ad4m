import { expect } from "chai";
import { execSync, spawnSync } from "node:child_process";
import { mkdtempSync, readdirSync, readFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { sfuScenario } from "../helpers/sfu/harness.js";
import { Hearing, type Heard, toneSource, wrtc } from "../helpers/sfu/e2e/media.js";
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
import { e19TabOpensSpace } from "../helpers/sfu/e2e/scenarios/e19-tab-opens-space.js";
import { e20TabTakesOver } from "../helpers/sfu/e2e/scenarios/e20-tab-takes-over.js";
import { e21TabGivesUp } from "../helpers/sfu/e2e/scenarios/e21-tab-gives-up.js";
import { e22TwoDevices } from "../helpers/sfu/e2e/scenarios/e22-two-devices.js";
import type { Scenario } from "../helpers/sfu/scenario.js";

/** `E2E_ONLY=e19,e21` runs only those scenarios; unset, all of them. */
const only = process.env.E2E_ONLY?.split(",").map((id) => id.trim()).filter(Boolean);
const scenario = (s: Scenario, timeoutMs: number) => {
  if (!only?.length || only.includes(s.id)) sfuScenario(s, timeoutMs);
};

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
  scenario(e19TabOpensSpace, 600_000);
  scenario(e20TabTakesOver, 600_000);
  scenario(e21TabGivesUp, 600_000);
  scenario(e22TwoDevices, 600_000);
});
