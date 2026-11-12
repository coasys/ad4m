import { expect } from "chai";
import type { Server } from "node:http";
import {
  BROWSER_KINDS, deviceIdOf, joinCall, launch, openTab, outboundIp, receiveProblems, report, servePage, short,
  type BrowserKind, type BrowserUser, type Launched, type Profile,
} from "../helpers/sfu/browser/browsers.js";
import type { PageReport } from "../helpers/sfu/browser/page.js";
import { startWorld, type World } from "../helpers/sfu/e2e/world.js";

/**
 * Calls in real browsers, mixed: Chromium (Chrome, Edge, Brave), Firefox and
 * WebKit (Safari), each running the SDK's browser build in its own tab, on
 * both topologies: through the executor's SFU and peer to peer (mesh).
 *
 * The end-to-end suite runs the SDK on `@roamhq/wrtc`, which is Chromium's
 * WebRTC; a Firefox user could not connect to a call (#1684: the SFU
 * forwarded RTP with the sender's payload types, which Firefox numbers
 * differently). Here every pair of browsers must hear and see each other,
 * read from WebRTC's stats (packets and decoded frames keep growing) and
 * from the decoded media (audible audio, non-black video).
 *
 * One executor without Holochain (`pnpm run prepare-test-local`), managed
 * users, one neighbourhood per scenario; the space's creator sets its call
 * mode, and each tab joins through `createSession` as an app does. A
 * browser that cannot start here makes its tests pending, with the reason;
 * with `SFU_BROWSERS_REQUIRED` set (CI) it fails the run instead.
 */

type Topology = "sfu" | "mesh";
const TOPOLOGIES: Topology[] = ["sfu", "mesh"];

/** The users and the browser each calls from. */
const USERS: Record<string, BrowserKind> = {
  chromium: "chromium",
  "chromium-2": "chromium",
  firefox: "firefox",
  webkit: "webkit",
};
const SCENARIOS = ["chromium", "firefox", "webkit", "mixed", "rejoin", "lost-device"] as const;
type ScenarioName = (typeof SCENARIOS)[number];

const CONNECT_TIMEOUT_MS = 90_000;
/** A lost tab's rejoin: the SFU drops the old peer after 10 s of ICE `Disconnected`; the SDK rebuilds within seconds. */
const REJOIN_TIMEOUT_MS = 150_000;
/**
 * How soon a device that died without leaving must leave everyone's list,
 * while another device of its agent stays in the call. Mesh: each device
 * renews a beat in its agent's status every 30 s and a reader drops a device
 * whose beat it has seen unchanged for 90 s (`PRESENCE_STALE_MS` in
 * core/src/neighbourhood/Session.ts), at its next roster poll (3 s); the
 * margin covers the status reads and this test's own reads. The SFU drops
 * the peer after 10 s of ICE `Disconnected`, well inside the bound.
 */
const PRESENCE_STALE_MS = 90_000;
const ROSTER_POLL_MS = 3_000;
const GHOST_GONE_MS = PRESENCE_STALE_MS + ROSTER_POLL_MS + 30_000;

const neighbourhoodName = (topology: Topology, scenario: ScenarioName) => `${topology}-${scenario}`;

describe("SFU: calls in real browsers, mixed, through the SDK", function () {
  this.timeout(300_000);

  let world: World | null = null;
  let server: Server | null = null;
  let pageUrl = "";
  const browsers = new Map<BrowserKind, Launched>();
  let open: BrowserUser[] = [];

  before(async function () {
    this.timeout(600_000);
    for (const kind of BROWSER_KINDS) {
      const launched = await launch(kind);
      browsers.set(kind, launched);
      console.log(launched.browser ? `  ${kind} ${launched.browser.version()}` : `  ${launched.reason}`);
    }
    ({ url: pageUrl, server } = await servePage());
    // The SFU on this host's network interface, as an executor serving browsers beyond its own
    // loopback binds it. On loopback (a localhost-only executor's default) Firefox cannot reach
    // it: Firefox gathers no loopback candidate (`media.peerconnection.ice.loopback` is off) and
    // its LAN candidates never pair with the SFU's 127.0.0.1, so ICE stays `checking` and the
    // SFU's DTLS handshake times out. Chromium (through a peer-reflexive pair) and WebKit connect.
    // `SFU_BROWSERS_SFU_ADDR` overrides it, e.g. `127.0.0.1` to see the loopback case.
    const sfuIp = process.env.SFU_BROWSERS_SFU_ADDR || await outboundIp();
    if (!sfuIp) throw new Error("this host has no IPv4 address beyond loopback for the SFU to bind (see the comment above)");
    const names = Object.keys(USERS);
    world = await startWorld({
      id: "browsers",
      executorEnv: { AD4M_SFU_BIND_ADDR: sfuIp },
      users: names.map((name) => ({ name })),
      neighbourhoods: TOPOLOGIES.flatMap((topology) =>
        SCENARIOS.map((scenario) => ({
          name: neighbourhoodName(topology, scenario),
          owner: "chromium",
          members: names,
          call: { mode: topology === "mesh" ? "mesh" : "designated" },
        }))),
    });
  });

  afterEach(async function () {
    // Hang up as an app does, then close the tab. A tab closed mid-call holds its SFU slot until
    // the server notices (ICE consent, then 10 s), and a user is in at most 4 calls on a node:
    // without the leave, the next scenarios' joins are refused ("Already in 4 calls on this node").
    for (const user of open) {
      await Promise.race([
        user.page.evaluate(() => (window as any).__call.leave()).catch(() => {}),
        new Promise((resolve) => setTimeout(resolve, 10_000)),
      ]);
      await user.context.close().catch(() => {});
    }
    open = [];
  });

  after(async function () {
    this.timeout(120_000);
    for (const { browser } of browsers.values()) await browser?.close().catch(() => {});
    server?.close();
    await world?.stop().catch(() => {});
  });

  /** Skip, saying why, unless every browser `kinds` names started. */
  function requireBrowsers(context: Mocha.Context, kinds: BrowserKind[]): void {
    const missing = kinds.map((k) => browsers.get(k)!).filter((l): l is Extract<Launched, { browser: null }> => !l.browser);
    if (missing.length === 0) return;
    const reason = missing.map((m) => m.reason).join("; ");
    console.log(`    skipped: ${reason}`);
    context.test!.title += ` (skipped: ${missing.map((m) => m.kind).join(", ")} cannot start)`;
    context.skip();
  }

  /** Open a tab for `name` and join the scenario's call; in `profile` (an earlier tab's storage), when given. */
  async function enter(name: string, topology: Topology, scenario: ScenarioName, profile?: Profile): Promise<BrowserUser> {
    const kind = USERS[name];
    const { browser } = browsers.get(kind)!;
    const worldUser = world!.users[name];
    const user = await openTab(browser!, kind, name, worldUser.did, pageUrl, profile);
    open.push(user);
    await joinCall(user, {
      executorUrl: `http://127.0.0.1:${worldUser.port}`,
      token: worldUser.token,
      neighbourhoodUrl: world!.url(neighbourhoodName(topology, scenario)),
      room: `browsers-${topology}-${scenario}`,
      name,
    });
    return user;
  }

  /**
   * Wait until every user lists every other and receives fresh media from
   * each: two reads apart, packets and decoded frames grew, audio audible,
   * video not black. Returns the last reports, by user name.
   */
  async function everyoneReceivesEveryone(users: BrowserUser[], timeoutMs: number, label: string): Promise<Record<string, PageReport>> {
    const deadline = Date.now() + timeoutMs;
    const readAll = async () => {
      const reports: Record<string, PageReport> = {};
      for (const user of users) reports[user.name] = await report(user);
      return reports;
    };
    let before = await readAll();
    for (;;) {
      await new Promise((resolve) => setTimeout(resolve, 1_500));
      const now = await readAll();
      const problems = users.flatMap((user) =>
        users.filter((other) => other !== user).flatMap((other) =>
          receiveProblems(before[user.name], now[user.name], other.did).map((p) => `${user.name} (${user.kind}) ${p} [${other.name}]`)));
      if (problems.length === 0) return now;
      if (Date.now() > deadline) {
        if (process.env.SFU_BROWSERS_DEBUG) {
          for (const user of users) {
            const sdps = await user.page.evaluate(() => (window as any).__call.descriptions());
            console.log(`[${user.name}] descriptions:\n${JSON.stringify(sdps, null, 1).replace(/\\r\\n/g, "\n")}`);
          }
        }
        throw new Error(
          `${label}: not every tab received every other user within ${timeoutMs / 1000} s:\n  ${problems.join("\n  ")}\n` +
            users.map((u) => `${u.name} (${u.kind}, ${short(u.did)}): ${JSON.stringify(now[u.name])}\n  console errors: ${JSON.stringify(u.consoleErrors.slice(-10))}`).join("\n"),
        );
      }
      before = now;
    }
  }

  function expectCleanSessions(reports: Record<string, PageReport>, topology: Topology): void {
    for (const [name, r] of Object.entries(reports)) {
      expect(r.joinError, `${name}'s join`).to.equal(null);
      expect(r.topology, `${name}'s topology`).to.equal(topology);
      expect(r.state, `${name}'s session state`).to.equal("active");
      expect(r.sessionErrors, `${name}'s session errors`).to.deep.equal([]);
    }
  }

  for (const topology of TOPOLOGIES) {
    describe(topology === "sfu" ? "through the SFU" : "peer to peer (mesh)", function () {
      for (const kind of BROWSER_KINDS) {
        const peer = kind === "chromium" ? "chromium-2" : kind;
        it(`chromium and ${kind} hear and see each other`, async function () {
          requireBrowsers(this, ["chromium", kind]);
          const users = [await enter("chromium", topology, kind), await enter(peer, topology, kind)];
          const reports = await everyoneReceivesEveryone(users, CONNECT_TIMEOUT_MS, `chromium + ${kind}`);
          expectCleanSessions(reports, topology);

          if (topology === "sfu" && kind === "firefox") {
            // #1684 for real: each browser's leg to the SFU numbers opus and VP8 its own way,
            // and the SFU rewrites the payload type of what it forwards to the receiver's.
            const [c, f] = users;
            const cSdp = reports[c.name].sdp[0];
            const fSdp = reports[f.name].sdp[0];
            expect(cSdp?.local.opus, "chromium's opus payload type in its offer").to.be.a("number");
            expect(fSdp?.local.opus, "firefox's opus payload type in its offer").to.be.a("number");
            expect(fSdp.local.opus, "the two legs' opus payload types (SDP)").to.not.equal(cSdp.local.opus);
            expect(fSdp.local.vp8, "the two legs' VP8 payload types (SDP)").to.not.equal(cSdp.local.vp8);
            // What each sends on and what the other receives that media on, from the stats.
            const fromFirefox = reports[c.name].inbound[f.did];
            const fromChromium = reports[f.name].inbound[c.did];
            expect(fromFirefox.audioPt, "firefox sends opus on one PT, chromium receives it on another")
              .to.not.equal(reports[f.name].outbound.audioPt);
            expect(fromChromium.audioPt, "chromium sends opus on one PT, firefox receives it on another")
              .to.not.equal(reports[c.name].outbound.audioPt);
            expect(fromFirefox.audioPt).to.equal(cSdp.local.opus);
            expect(fromChromium.audioPt).to.equal(fSdp.local.opus);
            expect(fromFirefox.videoPt).to.equal(cSdp.local.vp8);
            expect(fromChromium.videoPt).to.equal(fSdp.local.vp8);
          }
        });
      }

      it("chromium, firefox and webkit together: every pair hears and sees each other", async function () {
        requireBrowsers(this, ["chromium", "firefox", "webkit"]);
        const users = [];
        for (const name of ["chromium", "firefox", "webkit"]) users.push(await enter(name, topology, "mixed"));
        const reports = await everyoneReceivesEveryone(users, CONNECT_TIMEOUT_MS, "chromium + firefox + webkit");
        expectCleanSessions(reports, topology);
      });

      it("firefox's tab is lost mid-call and reopened in the same browser profile: it rejoins as the same device and everyone gets its media again", async function () {
        const third = browsers.get("webkit")!.browser ? "webkit" : "chromium-2";
        requireBrowsers(this, ["chromium", "firefox"]);
        this.timeout(REJOIN_TIMEOUT_MS * 2 + 60_000);
        const users = [];
        for (const name of ["chromium", "firefox", third]) users.push(await enter(name, topology, "rejoin"));
        await everyoneReceivesEveryone(users, CONNECT_TIMEOUT_MS, "before the loss");

        // The tab goes without a word: no leave, its sockets and connections cut. The
        // browser profile stays, as a crashed tab's does, and so does the device id in it.
        const lost = users[1];
        const device = await deviceIdOf(lost);
        const profile = await lost.context.storageState();
        await lost.context.close();
        open = open.filter((u) => u !== lost);

        const back = await enter("firefox", topology, "rejoin", profile);
        expect(device, "the lost tab's device id").to.be.a("string");
        expect(await deviceIdOf(back), "the reopened tab's device id").to.equal(device);
        const now = [users[0], back, users[2]];
        const reports = await everyoneReceivesEveryone(now, REJOIN_TIMEOUT_MS, "after firefox rejoined");
        for (const user of now) {
          expect(reports[user.name].joinError, `${user.name}'s join`).to.equal(null);
          const listed = reports[user.name].participants;
          expect(listed.filter((did) => did === back.did).length, `${user.name} lists firefox once`).to.be.at.most(1);
        }
      });

      it("a second device of firefox's user dies mid-call: it leaves everyone's list while the first stays", async function () {
        // Not WebKit: in a mesh call, headless WebKit's tab crashed (Playwright's `crash` event)
        // about 50 s after the death, while it still listed the dead device and repaired its
        // connection to it. That ends this test before it sees the presence it is about.
        const third = "chromium-2";
        requireBrowsers(this, ["chromium", "firefox"]);
        this.timeout(CONNECT_TIMEOUT_MS * 3 + GHOST_GONE_MS + 60_000);
        const users = [];
        for (const name of ["chromium", "firefox", third]) users.push(await enter(name, topology, "lost-device"));
        await everyoneReceivesEveryone(users, CONNECT_TIMEOUT_MS, "before the second device");

        // The same user from another browser profile: a device of its own.
        const second = await enter("firefox", topology, "lost-device");
        expect(await deviceIdOf(second), "the second device's id").to.not.equal(await deviceIdOf(users[1]));
        const firefoxDid = second.did;
        const tabs: [string, BrowserUser][] = [["chromium", users[0]], ["firefox", users[1]], [third, users[2]], ["firefox-2", second]];
        /** How often each open tab lists the firefox user: its two devices, less the tab's own. */
        const counts = async () => {
          const counted: Record<string, number> = {};
          for (const [label, tab] of tabs) {
            if (!open.includes(tab)) continue;
            const r = await report(tab).catch((error) => {
              throw new Error(`${label}'s tab: ${error instanceof Error ? error.message : error}; its events: ${JSON.stringify(tab.consoleErrors.slice(-10))}`);
            });
            counted[label] = r.participants.filter((did) => did === firefoxDid).length;
          }
          return counted;
        };
        const until = async (want: (counted: Record<string, number>) => boolean, timeoutMs: number, what: string) => {
          const deadline = Date.now() + timeoutMs;
          for (;;) {
            const counted = await counts().catch((error) => {
              throw new Error(`${what}: ${error instanceof Error ? error.message : error}`);
            });
            if (want(counted)) return;
            if (Date.now() > deadline) throw new Error(`${what} within ${timeoutMs / 1000} s; firefox listed: ${JSON.stringify(counted)}`);
            await new Promise((resolve) => setTimeout(resolve, 1_500));
          }
        };
        // Every other user lists both firefox devices; each firefox device lists the other.
        await until((c) => c.chromium === 2 && c[third] === 2 && c.firefox === 1 && c["firefox-2"] === 1,
          CONNECT_TIMEOUT_MS, "not everyone listed both firefox devices");

        // The second device goes without a word: no leave, its sockets and connections cut,
        // and its profile with it, so nothing comes back as that device.
        await second.context.close();
        open = open.filter((u) => u !== second);
        const lostAt = Date.now();

        await until((c) => c.chromium === 1 && c[third] === 1 && c.firefox === 0,
          GHOST_GONE_MS, "the dead device did not leave every list");
        console.log(`    the dead device left every list ${Math.round((Date.now() - lostAt) / 1000)} s after it died`);
        // The first device is still in the call, heard and seen by everyone.
        const reports = await everyoneReceivesEveryone(users, CONNECT_TIMEOUT_MS, "after the second device died");
        expectCleanSessions(reports, topology);
      });
    });
  }
});
