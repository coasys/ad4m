import { expect } from "chai";
import type { Server } from "node:http";
import {
  BROWSER_KINDS, joinCall, launch, openTab, outboundIp, receiveProblems, report, servePage, short,
  type BrowserKind, type BrowserUser, type Launched,
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
const SCENARIOS = ["chromium", "firefox", "webkit", "mixed", "rejoin"] as const;
type ScenarioName = (typeof SCENARIOS)[number];

const CONNECT_TIMEOUT_MS = 90_000;
/** A lost tab's rejoin: the SFU drops the old peer after 10 s of ICE `Disconnected`; the SDK rebuilds within seconds. */
const REJOIN_TIMEOUT_MS = 150_000;

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

  async function enter(name: string, topology: Topology, scenario: ScenarioName): Promise<BrowserUser> {
    const kind = USERS[name];
    const { browser } = browsers.get(kind)!;
    const worldUser = world!.users[name];
    const user = await openTab(browser!, kind, name, worldUser.did, pageUrl);
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

      it("firefox's tab is lost mid-call and it rejoins: everyone gets its media again", async function () {
        const third = browsers.get("webkit")!.browser ? "webkit" : "chromium-2";
        requireBrowsers(this, ["chromium", "firefox"]);
        this.timeout(REJOIN_TIMEOUT_MS * 2 + 60_000);
        const users = [];
        for (const name of ["chromium", "firefox", third]) users.push(await enter(name, topology, "rejoin"));
        await everyoneReceivesEveryone(users, CONNECT_TIMEOUT_MS, "before the loss");

        // The tab goes without a word: no leave, its sockets and connections cut.
        const lost = users[1];
        await lost.context.close();
        open = open.filter((u) => u !== lost);

        const back = await enter("firefox", topology, "rejoin");
        const now = [users[0], back, users[2]];
        const reports = await everyoneReceivesEveryone(now, REJOIN_TIMEOUT_MS, "after firefox rejoined");
        for (const user of now) {
          expect(reports[user.name].joinError, `${user.name}'s join`).to.equal(null);
          const listed = reports[user.name].participants;
          expect(listed.filter((did) => did === back.did).length, `${user.name} lists firefox once`).to.be.at.most(1);
        }
      });
    });
  }
});
