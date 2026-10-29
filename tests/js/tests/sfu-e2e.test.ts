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

  sfuScenario(e1TwoUsers, 600_000);
  sfuScenario(e2ManyUsers, 1_200_000);
  sfuScenario(e3TrackChanges, 900_000);
  sfuScenario(e4JoinStorms, 1_200_000);
  sfuScenario(e5AbruptLoss, 900_000);
  sfuScenario(e6Network, 900_000);
  sfuScenario(e7ExecutorRestart, 900_000);
  sfuScenario(e8Capacity, 1_200_000);
  sfuScenario(e9Simulcast, 600_000);
  sfuScenario(e10Data, 600_000);
  sfuScenario(e11Outsider, 600_000);
  sfuScenario(e12Topology, 900_000);
  sfuScenario(e13Mesh, 900_000);
  sfuScenario(e14AdvertisedAddress, 600_000);
});
