import { expect } from "chai";
import { existsSync, mkdirSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import { homedir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, type Browser, type BrowserContext, type Page } from "playwright-core";
import { startWorld, type World } from "../helpers/sfu/e2e/world.js";

/**
 * A mesh call in real Chromium, through the SDK's browser build.
 *
 * The end-to-end suite runs the SDK on `@roamhq/wrtc` behind a shim; this
 * runs it on the WebRTC an app's users have. Three managed users of one
 * executor (local languages, no Holochain: `pnpm run prepare-test-local`),
 * each in its own browser context with Chromium's fake camera and
 * microphone, join a space whose creator chose mesh. Every page must list
 * the other two and receive audio and video packets from each of them.
 *
 * Opt-in (`pnpm run test-sfu-browser`), and pending without a Chromium:
 * `CHROMIUM_PATH`, else Playwright's cached build, else a system Chrome.
 * With `SFU_BROWSER_EVIDENCE_DIR` set, it writes what each page saw
 * (`evidence.json`) and a screenshot of each page there.
 */

const TESTS_JS = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const SDK_BUNDLE = path.resolve(TESTS_JS, "..", "..", "core", "lib", "index.js");
const USERS = ["u1", "u2", "u3"];
const CONNECT_TIMEOUT_MS = 60_000;

function findChromium(): string | null {
  // Any Chromium Playwright has cached, newest first: its own revision may not be the one downloaded.
  const cache = process.env.PLAYWRIGHT_BROWSERS_PATH ?? path.join(homedir(), ".cache", "ms-playwright");
  const cached = existsSync(cache)
    ? readdirSync(cache)
        .filter((name) => /^chromium-\d+$/.test(name))
        .sort((a, b) => Number(b.split("-")[1]) - Number(a.split("-")[1]))
        .flatMap((name) => [path.join(cache, name, "chrome-linux64", "chrome"), path.join(cache, name, "chrome-linux", "chrome")])
    : [];
  const candidates = [
    process.env.CHROMIUM_PATH,
    (() => {
      try {
        return chromium.executablePath();
      } catch {
        return undefined;
      }
    })(),
    ...cached,
    "/usr/bin/google-chrome",
    "/usr/bin/chromium",
    "/usr/bin/chromium-browser",
  ];
  return candidates.find((candidate): candidate is string => !!candidate && existsSync(candidate)) ?? null;
}

/** What a page reports: its session, its peers, and the packets it received from each. */
interface PageReport {
  state: string;
  topology: string | null;
  participants: string[];
  peerStates: Record<string, string>;
  inbound: Record<string, { audio: number; video: number; framesDecoded: number }>;
  joinError: string | null;
  sessionErrors: string[];
}

interface BrowserUser {
  name: string;
  did: string;
  executorUrl: string;
  page: Page;
  consoleErrors: string[];
}

/** The page every user opens: it loads the SDK's ESM bundle and exposes it. */
const PAGE = `<!doctype html>
<html><head><meta charset="utf-8"><title>mesh</title>
<style>body{font:12px sans-serif;margin:4px} figure{display:inline-block;margin:4px} video{width:320px;height:240px;background:#000}</style>
</head><body><h3 id="who"></h3>
<script type="module">
import * as ad4m from "./ad4m.js";
window.ad4m = ad4m;
window.ad4mReady = true;
</script></body></html>`;

/** Record every peer connection the page creates, so their stats can be read. */
function recordPeerConnections(): void {
  const w = window as any;
  const Native = w.RTCPeerConnection;
  w.peerConnections = [];
  w.RTCPeerConnection = class extends Native {
    constructor(...args: any[]) {
      super(...args);
      w.peerConnections.push(this);
    }
  };
}

/** In the page: join the mesh call as an app does, and render each remote's stream. */
async function joinInPage(args: { baseUrl: string; token: string; neighbourhoodUrl: string; room: string; name: string }): Promise<void> {
  const w = window as any;
  w.sessionErrors = [];
  w.joinError = null;
  w.topology = null;
  document.getElementById("who")!.textContent = args.name;
  try {
    const client = new w.ad4m.Ad4mClient(args.baseUrl, args.token);
    const perspective = (await client.perspective.all()).find((p: any) => p.sharedUrl === args.neighbourhoodUrl);
    if (!perspective) throw new Error(`no perspective for ${args.neighbourhoodUrl}`);
    const proxy = perspective.getNeighbourhoodProxy();
    const sfuConfig = await proxy.sfuConfig(args.neighbourhoodUrl).catch(() => undefined);
    const session = await proxy.createSession(args.room, {
      neighbourhoodUrl: args.neighbourhoodUrl,
      topology: "mesh",
      ...(sfuConfig ? { sfuConfig } : {}),
    });
    w.session = session;
    session.on("topology-changed", (topology: string) => { w.topology = topology; });
    session.on("error", (error: any) => { w.sessionErrors.push(String(error?.message ?? error)); });
    const render = () => {
      for (const participant of session.participants) {
        let video = document.getElementById(participant.agentDid) as HTMLVideoElement | null;
        if (!video) {
          const figure = document.createElement("figure");
          const caption = document.createElement("figcaption");
          caption.textContent = participant.agentDid.slice(-12);
          video = document.createElement("video");
          video.id = participant.agentDid;
          video.muted = true;
          video.autoplay = true;
          video.playsInline = true;
          figure.append(video, caption);
          document.body.append(figure);
        }
        if (video.srcObject !== participant.stream) {
          video.srcObject = participant.stream;
          video.play().catch(() => {});
        }
      }
    };
    for (const event of ["participant-joined", "stream-added", "participant-updated"]) session.on(event, render);
    setInterval(render, 1_000);
    const stream = await navigator.mediaDevices.getUserMedia({ audio: true, video: { width: 320, height: 240 } });
    // Not awaited: a join that never completes shows in the reports, within the test's bound.
    session.join(stream).catch((error: any) => { w.joinError = String(error?.message ?? error); });
  } catch (error: any) {
    w.joinError = String(error?.message ?? error);
  }
}

/** In the page: the session's view, and the inbound packets per remote, from WebRTC's own stats. */
async function reportInPage(): Promise<PageReport> {
  const w = window as any;
  const session = w.session;
  const stats: any[] = [];
  for (const pc of w.peerConnections as RTCPeerConnection[]) {
    if (pc.connectionState === "closed") continue;
    (await pc.getStats()).forEach((s: any) => stats.push(s));
  }
  const inbound: PageReport["inbound"] = {};
  for (const participant of session?.participants ?? []) {
    const tracks = new Set(participant.stream.getTracks().map((t: MediaStreamTrack) => t.id));
    const counts = { audio: 0, video: 0, framesDecoded: 0 };
    for (const s of stats) {
      if (s.type !== "inbound-rtp" || !tracks.has(s.trackIdentifier)) continue;
      if (s.kind === "audio") counts.audio += s.packetsReceived ?? 0;
      if (s.kind === "video") {
        counts.video += s.packetsReceived ?? 0;
        counts.framesDecoded += s.framesDecoded ?? 0;
      }
    }
    inbound[participant.agentDid] = counts;
  }
  return {
    state: session?.getState() ?? "none",
    topology: w.topology,
    participants: (session?.participants ?? []).map((p: any) => p.agentDid),
    peerStates: Object.fromEntries(session?.peerStates() ?? []),
    inbound,
    joinError: w.joinError,
    sessionErrors: w.sessionErrors ?? [],
  };
}

/** Whether a page lists every other user and has audio and video packets from each. */
function receivesEveryone(report: PageReport, others: string[]): boolean {
  return others.every((did) =>
    report.participants.includes(did) && report.inbound[did]?.audio > 0 && report.inbound[did]?.video > 0);
}

describe("SFU: a mesh call in Chromium through the SDK", function () {
  this.timeout(600_000);

  it("three users in their own browsers list each other and receive each other's audio and video", async function () {
    const executablePath = findChromium();
    if (!executablePath) this.skip();
    if (!existsSync(SDK_BUNDLE)) throw new Error(`${SDK_BUNDLE} is missing: build core first (\`pnpm run build\` in core).`);
    const bundle = readFileSync(SDK_BUNDLE, "utf8");
    const evidenceDir = process.env.SFU_BROWSER_EVIDENCE_DIR;
    if (evidenceDir) mkdirSync(evidenceDir, { recursive: true });

    let world: World | null = null;
    let browser: Browser | null = null;
    let server: Server | null = null;
    const contexts: BrowserContext[] = [];
    const users: BrowserUser[] = [];
    const reports: Record<string, PageReport> = {};
    let outcome = "not run";
    try {
      world = await startWorld({
        id: "browser-mesh",
        users: USERS.map((name) => ({ name })),
        neighbourhoods: [{ name: "space", owner: "u1", members: USERS, call: { mode: "mesh" } }],
      });
      const neighbourhoodUrl = world.url("space");
      browser = await chromium.launch({
        executablePath: executablePath!,
        headless: true,
        args: [
          "--use-fake-device-for-media-stream",
          "--use-fake-ui-for-media-stream",
          "--autoplay-policy=no-user-gesture-required",
        ],
      });

      // The page comes from a loopback server, as an app served next to its executor would:
      // Chromium lets a loopback page reach the executor on 127.0.0.1.
      server = createServer((request, response) => {
        const isBundle = request.url === "/ad4m.js";
        response.writeHead(200, { "content-type": isBundle ? "text/javascript" : "text/html" });
        response.end(isBundle ? bundle : PAGE);
      });
      await new Promise<void>((resolve) => server!.listen(0, "127.0.0.1", resolve));
      const pageUrl = `http://127.0.0.1:${(server.address() as AddressInfo).port}/index.html`;

      for (const name of USERS) {
        const { port, did } = world.users[name];
        const executorUrl = `http://127.0.0.1:${port}`;
        const context = await browser.newContext({ permissions: ["camera", "microphone"], viewport: { width: 720, height: 340 } });
        contexts.push(context);
        await context.addInitScript(recordPeerConnections);
        const page = await context.newPage();
        const consoleErrors: string[] = [];
        page.on("console", (message) => {
          // Without the user's token, which a failed socket's URL carries.
          if (message.type() === "error") consoleErrors.push(message.text().replace(/token=[^'"\s]+/g, "token=…"));
        });
        page.on("pageerror", (error) => consoleErrors.push(`pageerror: ${error.message}`));
        await page.goto(pageUrl);
        await page.waitForFunction(() => (window as any).ad4mReady === true);
        users.push({ name, did, executorUrl, page, consoleErrors });
      }

      await Promise.all(users.map((user) =>
        user.page.evaluate(joinInPage, { baseUrl: user.executorUrl, token: world!.users[user.name].token, neighbourhoodUrl, room: "browser-mesh", name: user.name })));

      const othersOf = (user: BrowserUser) => users.filter((u) => u !== user).map((u) => u.did);
      const readAll = async () => {
        for (const user of users) reports[user.name] = await user.page.evaluate(reportInPage);
      };
      const deadline = Date.now() + CONNECT_TIMEOUT_MS;
      for (;;) {
        await readAll();
        if (users.every((user) => receivesEveryone(reports[user.name], othersOf(user)))) break;
        if (Date.now() > deadline) {
          throw new Error(
            `not every page received every other user within ${CONNECT_TIMEOUT_MS / 1000} s:\n` +
              users.map((u) => `${u.name}: ${JSON.stringify(reports[u.name])}\n  console errors: ${JSON.stringify(u.consoleErrors)}`).join("\n"),
          );
        }
        await new Promise((resolve) => setTimeout(resolve, 1_000));
      }

      // Packets keep arriving: media flows, not just a burst at connect.
      const first = JSON.parse(JSON.stringify(reports)) as Record<string, PageReport>;
      await new Promise((resolve) => setTimeout(resolve, 3_000));
      await readAll();
      for (const user of users) {
        expect(reports[user.name].topology, `${user.name}'s topology`).to.equal("mesh");
        expect(reports[user.name].joinError, `${user.name}'s join`).to.equal(null);
        for (const did of othersOf(user)) {
          const before = first[user.name].inbound[did];
          const now = reports[user.name].inbound[did];
          expect(now.audio, `${user.name}: audio packets from ${did}`).to.be.greaterThan(before.audio);
          expect(now.video, `${user.name}: video packets from ${did}`).to.be.greaterThan(before.video);
        }
      }

      if (evidenceDir) {
        // Wait for each remote's fake camera to be on screen before the screenshot.
        for (const user of users) {
          await user.page.waitForFunction(
            (count) => Array.from(document.querySelectorAll("video")).filter((v) => v.videoWidth > 0).length >= count,
            users.length - 1,
            { timeout: 15_000 },
          ).catch(() => {});
          await user.page.screenshot({ path: path.join(evidenceDir, `${user.name}.png`) });
        }
      }
      outcome = "passed";
    } catch (error) {
      outcome = `failed: ${error instanceof Error ? error.message : String(error)}`;
      throw error;
    } finally {
      if (evidenceDir) {
        const evidence = {
          outcome,
          chromium: executablePath,
          browserVersion: browser?.version() ?? null,
          users: users.map((user) => ({
            name: user.name,
            did: user.did,
            consoleErrors: user.consoleErrors,
            report: reports[user.name] ?? null,
          })),
        };
        writeFileSync(path.join(evidenceDir, "evidence.json"), JSON.stringify(evidence, null, 2));
      }
      for (const context of contexts) await context.close().catch(() => {});
      await browser?.close().catch(() => {});
      server?.close();
      await world?.stop().catch(() => {});
    }
  });
});
