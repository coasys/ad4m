/**
 * Real browsers for the cross-browser call suite: Chromium, Firefox and
 * WebKit through Playwright, each with fake camera and microphone, and the
 * users who call in them, each in a browser context of its own.
 *
 * Chrome, Edge and Brave are Chromium; Safari is WebKit. Pages come from a
 * loopback server (`servePage`), as an app served next to its executor
 * would: `http://127.0.0.1` is a secure context, so `getUserMedia` works.
 */

import { existsSync, readFileSync } from "node:fs";
import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { chromium, firefox, webkit, type Browser, type BrowserContext, type BrowserType, type Page } from "playwright-core";
import { installCallHarness, PAGE, type Inbound, type JoinArgs, type PageReport } from "./page.js";

export type BrowserKind = "chromium" | "firefox" | "webkit";
export const BROWSER_KINDS: BrowserKind[] = ["chromium", "firefox", "webkit"];

const TESTS_JS = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "..");
const SDK_BUNDLE = path.resolve(TESTS_JS, "..", "..", "core", "lib", "index.js");

const TYPES: Record<BrowserKind, BrowserType> = { chromium, firefox, webkit };

/** A browser that started, or why it did not. */
export type Launched = { kind: BrowserKind; browser: Browser } | { kind: BrowserKind; browser: null; reason: string };

/**
 * Start one browser with fake media devices. A browser that cannot start on
 * this host is returned with the reason, so its tests can say why they skip;
 * with `SFU_BROWSERS_REQUIRED` set (CI) it throws instead.
 *
 * - Chromium: `CHROMIUM_PATH` if set, else Playwright's build.
 * - Firefox: fake devices and no permission prompt by preference. Host
 *   candidates are not hidden behind mDNS names, as for a user who granted
 *   the camera (Firefox only waives the obfuscation for a real grant, which
 *   `media.navigator.permission.disabled` is not).
 * - WebKit: Playwright's mock capture devices, granted per context. Where
 *   the host lacks WebKit's system libraries and they cannot be installed,
 *   `WEBKIT_LD_PRELOAD` (a `:`-separated list of library files, dependencies
 *   first) is preloaded into WebKit alone; Playwright's host check is then
 *   skipped, since the libraries it looks for are the ones preloaded.
 */
export async function launch(kind: BrowserKind): Promise<Launched> {
  try {
    const browser = await (async () => {
      switch (kind) {
        case "chromium":
          return chromium.launch({
            ...(process.env.CHROMIUM_PATH ? { executablePath: process.env.CHROMIUM_PATH } : {}),
            headless: true,
            args: [
              "--use-fake-device-for-media-stream",
              "--use-fake-ui-for-media-stream",
              "--autoplay-policy=no-user-gesture-required",
            ],
          });
        case "firefox":
          return firefox.launch({
            headless: true,
            firefoxUserPrefs: {
              "media.navigator.streams.fake": true,
              "media.navigator.permission.disabled": true,
              "media.autoplay.default": 0,
              "media.autoplay.block-webaudio": false,
              "media.peerconnection.ice.obfuscate_host_addresses": false,
            },
          });
        case "webkit": {
          const preload = process.env.WEBKIT_LD_PRELOAD;
          if (preload) process.env.PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = "1";
          return webkit.launch({ headless: true, ...(preload ? { env: { ...process.env, LD_PRELOAD: preload } } : {}) });
        }
      }
    })();
    return { kind, browser };
  } catch (error) {
    const reason = `${kind} cannot start on this host: ${String(error instanceof Error ? error.message : error)
      .split("\n").map((l) => l.replace(/[║╔╗╚╝═]/g, "").trim()).filter(Boolean).slice(0, 6).join(" ")}` +
      ` (install it and its system libraries: \`pnpm exec playwright-core install --with-deps ${kind}\` in tests/js)`;
    if (process.env.SFU_BROWSERS_REQUIRED) throw new Error(reason);
    return { kind, browser: null, reason };
  }
}

/** Serve the page and the SDK's ESM bundle on loopback. */
export async function servePage(): Promise<{ url: string; server: Server }> {
  if (!existsSync(SDK_BUNDLE)) throw new Error(`${SDK_BUNDLE} is missing: build core first (\`pnpm run build\` in core).`);
  const bundle = readFileSync(SDK_BUNDLE, "utf8");
  const server = createServer((request, response) => {
    const isBundle = request.url === "/ad4m.js";
    response.writeHead(200, { "content-type": isBundle ? "text/javascript" : "text/html" });
    response.end(isBundle ? bundle : PAGE);
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  return { url: `http://127.0.0.1:${(server.address() as AddressInfo).port}/index.html`, server };
}

/** A user calling from one browser tab. */
export interface BrowserUser {
  /** The user's name in the world, e.g. `firefox`. */
  name: string;
  kind: BrowserKind;
  did: string;
  context: BrowserContext;
  page: Page;
  consoleErrors: string[];
}

/** Open a tab for a user in its own browser context, with the call harness installed. */
export async function openTab(browser: Browser, kind: BrowserKind, name: string, did: string, pageUrl: string): Promise<BrowserUser> {
  const context = await browser.newContext(kind === "firefox" ? {} : { permissions: ["camera", "microphone"] });
  await context.addInitScript(installCallHarness);
  const page = await context.newPage();
  const consoleErrors: string[] = [];
  page.on("console", (message) => {
    // Without the user's token, which a failed socket's URL carries.
    if (message.type() === "error") consoleErrors.push(message.text().replace(/token=[^'"\s]+/g, "token=…"));
  });
  page.on("pageerror", (error) => consoleErrors.push(`pageerror: ${error.message}`));
  await page.goto(pageUrl);
  await page.waitForFunction(() => (window as any).ad4mReady === true);
  return { name, kind, did, context, page, consoleErrors };
}

export async function joinCall(user: BrowserUser, args: JoinArgs): Promise<void> {
  await user.page.evaluate((a) => (window as any).__call.join(a), args);
}

export async function report(user: BrowserUser): Promise<PageReport> {
  return user.page.evaluate(() => (window as any).__call.report());
}

/** Thresholds for "the media is real": a fake tone is far above 0.01; a fake camera's frame far above black. */
const AUDIBLE_PEAK = 0.01;
const NOT_BLACK_LUMA = 10;

/**
 * What is wrong with what `now` received from `did`, given what `before`
 * (an earlier report) had: the participant must be listed, its audio and
 * video packets and decoded frames must have grown since, its audio must
 * be audible, and its video, where the page can draw it, not black.
 */
export function receiveProblems(before: PageReport | undefined, now: PageReport, did: string): string[] {
  if (!now.participants.includes(did)) return [`does not list ${short(did)}`];
  const a: Inbound | undefined = before?.inbound[did];
  const b = now.inbound[did];
  const problems: string[] = [];
  if (!b) return [`has no inbound media from ${short(did)}`];
  if (!(b.audioPackets > (a?.audioPackets ?? 0))) problems.push(`audio packets from ${short(did)} not growing (${a?.audioPackets ?? 0} → ${b.audioPackets})`);
  if (!(b.videoPackets > (a?.videoPackets ?? 0))) problems.push(`video packets from ${short(did)} not growing (${a?.videoPackets ?? 0} → ${b.videoPackets})`);
  if (!(b.framesDecoded > (a?.framesDecoded ?? 0))) problems.push(`no new decoded frames from ${short(did)} (${a?.framesDecoded ?? 0} → ${b.framesDecoded})`);
  if (!(b.audioPeak > AUDIBLE_PEAK)) problems.push(`audio from ${short(did)} is silent (peak ${b.audioPeak.toFixed(4)}, ${b.audioMeter})`);
  if (b.luma !== null && !(b.luma > NOT_BLACK_LUMA)) problems.push(`video from ${short(did)} is black (luma ${b.luma.toFixed(1)})`);
  return problems;
}

export function short(did: string): string {
  return did.slice(-8);
}
