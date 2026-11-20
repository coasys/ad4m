import { appendFileSync, createWriteStream, mkdirSync, readFileSync, writeFileSync, type WriteStream } from "node:fs";
import type { Server } from "node:http";
import { createSocket, type Socket } from "node:dgram";
import os from "node:os";
import path from "node:path";
import { type Browser, type BrowserContext, type CDPSession, type Page } from "playwright-core";
import { launch, outboundIp, servePage, short, type BrowserKind } from "../helpers/sfu/browser/browsers.js";
import { startWorld, type World } from "../helpers/sfu/e2e/world.js";

/**
 * A long call between five Chromium tabs, each in a browser of its own (its own WebRTC
 * stack), stressed in phases, to catch one participant's receive side dying while its
 * send side keeps working (Nico, 2026-11-19: in a 5-person We call, one participant
 * stopped seeing and hearing the other four, while all four still saw and heard them).
 *
 * Local only. Every 5 s, for each ordered pair (receiver, sender), the receiver's view of
 * the sender is sampled twice:
 *  - app level: the inbound-rtp stats of the tracks in the sender's participant stream,
 *    as an app renders them (what the user sees and hears);
 *  - transport level: the inbound-rtp stats of every receiver on the connection(s), by mid,
 *    whether or not the SDK attached the track to a participant.
 * A pair whose counters stop growing for 15 s while another receiver still gets that sender
 * is dead; one receiver dead for all four senders while all four still get it is the
 * asymmetric failure, and its tab's console and the executor's log around it are dumped.
 *
 * Opt-in: no CI job runs it. Run it with `pnpm run test-main-sfu-soak5` (prepare, soak,
 * clean up), or `pnpm run test-sfu-soak5` once the test languages are prepared.
 *
 * SOAK_TOPOLOGY sfu|mesh (default sfu), SOAK_RUN_DIR (default <tmpdir>/sfu-soak5/<time>),
 * SOAK_SCALE (phase length multiplier, default 1: about 28 minutes), SOAK_SHAPED_BROWSER
 * (firefox puts the shaped seat u3 in Firefox), SOAK_RUST_LOG (the executor's RUST_LOG),
 * SOAK_PHASES=rebind (only the join; everyone but u3 turns camera and mic off for 30 s; then
 * two NAT rebindings of u3, about 95 s apart: about 5 minutes).
 *
 * The full run is a report: it fails only when the join fails, and its findings are in
 * summary.json and the logs. SOAK_PHASES=rebind is a regression test and fails unless, after
 * each rebinding, every pair receives again within RECOVER_MS (20 s, read on a 2 s sampling
 * grid), every pair receives at the end, and no dead pair or asymmetric event outlasts the
 * grace (GRACE_MS) the rebinding is allowed.
 *
 * u3's transport is also read every second (`probe-u3.jsonl`), so a receive gap is timed to
 * the second, and each tab logs the SHA-256 of the SDK bundle it loaded.
 */

const TOPOLOGY = (process.env.SOAK_TOPOLOGY ?? "sfu") as "sfu" | "mesh";
const SCALE = Number(process.env.SOAK_SCALE ?? "1");
const RUN_DIR = process.env.SOAK_RUN_DIR ?? path.join(os.tmpdir(), "sfu-soak5", new Date().toISOString().replace(/[:.]/g, "-"));
const NAMES = ["u1", "u2", "u3", "u4", "u5"];
const DEAD_MS = 15_000;
/** After an intended disruption ends, how long its pairs may stay quiet without counting as unexpected. */
const GRACE_MS = 45_000;
const ROOM = `soak5-${TOPOLOGY}`;
const ONLY_REBIND = process.env.SOAK_PHASES === "rebind";
/** Every pair is read this often: in SOAK_PHASES=rebind often enough to time RECOVER_MS to 2 s. */
const SAMPLE_MS = ONLY_REBIND ? 2_000 : 5_000;
/** In SOAK_PHASES=rebind, how soon after a rebinding every pair must receive again: the
 *  measured repairs take 7-8 s (client watchdog) and 37-38 s (server backstop, rebuild). */
const RECOVER_MS = 20_000;

/** What one tab reports about one remote participant (a sender). */
interface PairSample {
  did: string;
  participantId: string;
  /** Live tracks in the participant's stream: what the app can render. */
  tracks: number;
  a: { pkts: number; bytes: number; level: number };
  v: { pkts: number; bytes: number; frames: number; width: number };
  peak: number;
  luma: number | null;
  /** The state of the connection(s) carrying this participant's tracks. */
  pc: string[];
}
interface TabSample {
  state: string;
  topology: string | null;
  joined: boolean;
  pcs: { conn: string; ice: string; sig: string; recv: number; inbound: { mid: string | null; kind: string; pkts: number; bytes: number; frames: number }[]; sent: { kind: string; pkts: number }[] }[];
  pairs: PairSample[];
  errors: string[];
}

/**
 * The page side, installed as an init script: it must not reach anything outside its body.
 * The tab joins as the We app does (`topologyFor(sfuConfig.mode)`), and can leave, rejoin,
 * toggle its camera and microphone, pick a quality, hear of a network change, and report.
 */
function installSoak(): void {
  const w = window as any;
  const Native = w.RTCPeerConnection;
  const connections: RTCPeerConnection[] = [];
  // On the tab whose network is shaped, every remote UDP candidate goes through the test's
  // relay (`__soakRelay`, exposed to that tab alone): CDP's network emulation leaves WebRTC alone.
  const relayed = async (sdp: string): Promise<string> => {
    if (typeof w.__soakRelay !== "function") return sdp;
    const out: string[] = [];
    for (const line of sdp.split("\r\n")) {
      const m = line.match(/^(a=candidate:\S+ \d+ udp \d+ )(\S+) (\d+)( typ host.*)$/i);
      if (m) {
        const port = await w.__soakRelay(`${m[2]}:${m[3]}`);
        out.push(`${m[1]}${m[2]} ${port}${m[4]}`);
      } else out.push(line);
    }
    return out.join("\r\n");
  };
  w.RTCPeerConnection = class extends Native {
    constructor(...args: any[]) {
      super(...args);
      connections.push(this as unknown as RTCPeerConnection);
    }
    async setRemoteDescription(desc: any): Promise<void> {
      if (desc?.sdp) desc = { type: desc.type, sdp: await relayed(desc.sdp) };
      return super.setRemoteDescription(desc);
    }
    // The SFU runs full ICE and checks the candidates a client trickles: the shaped tab tells it
    // none, so the only path it learns is the one the tab's checks arrive on, through the relay.
    set onicecandidate(handler: any) {
      super.onicecandidate = typeof w.__soakRelay === "function" && handler
        ? (e: RTCPeerConnectionIceEvent) => { if (!e.candidate) handler(e); }
        : handler;
    }
    get onicecandidate(): any {
      return super.onicecandidate;
    }
    get localDescription(): any {
      const d = super.localDescription;
      if (!d || typeof w.__soakRelay !== "function") return d;
      return { type: d.type, sdp: d.sdp.split("\r\n").filter((l: string) => !l.startsWith("a=candidate:")).join("\r\n") };
    }
    async addIceCandidate(c: any): Promise<void> {
      if (c?.candidate) {
        const sdp = await relayed(`a=${c.candidate}`);
        c = { ...(c.toJSON?.() ?? c), candidate: sdp.slice(2) };
      }
      return super.addIceCandidate(c);
    }
  };
  let args: any = null;
  let client: any = null;
  let proxy: any = null;
  let sfuConfig: any = null;
  let session: any = null;
  let stream: MediaStream | null = null;
  let joinedOnce = false;
  const errors: string[] = [];
  let audio: AudioContext | null = null;
  const meters = new Map<string, { analyser: AnalyserNode; peaks: { at: number; value: number }[] }>();
  const tiles = new Map<string, HTMLVideoElement>();
  const samples = new Float32Array(2048);
  setInterval(() => {
    const now = performance.now();
    for (const entry of meters.values()) {
      entry.analyser.getFloatTimeDomainData(samples);
      let peak = 0;
      for (const s of samples) peak = Math.max(peak, Math.abs(s));
      entry.peaks.push({ at: now, value: peak });
      while (entry.peaks.length && entry.peaks[0].at < now - 3_000) entry.peaks.shift();
    }
  }, 100);
  const meter = (track: MediaStreamTrack) => {
    if (meters.has(track.id) || track.readyState === "ended") return;
    audio ??= new AudioContext();
    if (audio.state === "suspended") void audio.resume().catch(() => {});
    const analyser = audio.createAnalyser();
    audio.createMediaStreamSource(new MediaStream([track])).connect(analyser);
    meters.set(track.id, { analyser, peaks: [] });
  };
  const render = () => {
    const listed = new Set<string>();
    for (const p of session?.participants ?? []) {
      listed.add(p.participantId);
      let video = tiles.get(p.participantId);
      if (!video) {
        video = document.createElement("video");
        video.muted = true;
        video.autoplay = true;
        video.playsInline = true;
        video.width = 160;
        video.height = 120;
        document.body.append(video);
        tiles.set(p.participantId, video);
      }
      if (video.srcObject !== p.stream) {
        video.srcObject = p.stream;
        void video.play().catch(() => {});
      }
      for (const t of p.stream.getAudioTracks()) meter(t);
    }
    for (const [id, video] of tiles) {
      if (listed.has(id)) continue;
      video.srcObject = null;
      video.remove();
      tiles.delete(id);
    }
  };
  setInterval(render, 500);
  const canvas = document.createElement("canvas");
  canvas.width = 32;
  canvas.height = 24;
  const luma = (id: string): number | null => {
    const video = tiles.get(id);
    if (!video || video.videoWidth === 0) return null;
    const c = canvas.getContext("2d", { willReadFrequently: true })!;
    c.drawImage(video, 0, 0, canvas.width, canvas.height);
    const px = c.getImageData(0, 0, canvas.width, canvas.height).data;
    let sum = 0;
    for (let i = 0; i < px.length; i += 4) sum += 0.299 * px[i] + 0.587 * px[i + 1] + 0.114 * px[i + 2];
    return sum / (px.length / 4);
  };

  async function join(a?: any): Promise<void> {
    if (a) args = a;
    document.title = args.name;
    if (!client) {
      client = new w.ad4m.Ad4mClient(args.executorUrl, args.token);
      const perspective = (await client.perspective.all()).find((p: any) => p.sharedUrl === args.neighbourhoodUrl);
      if (!perspective) throw new Error(`no perspective for ${args.neighbourhoodUrl}`);
      proxy = perspective.getNeighbourhoodProxy();
    }
    // As the We app's call port does: the space's policy picks mesh or sfu, never "auto".
    sfuConfig = await proxy.sfuConfig(args.neighbourhoodUrl);
    const topology = !sfuConfig?.mode || sfuConfig.mode === "mesh" ? "mesh" : "sfu";
    session = await proxy.createSession(args.room, { neighbourhoodUrl: args.neighbourhoodUrl, topology, sfuConfig });
    const s = session;
    s.on("error", (e: any) => { errors.push(`${new Date().toISOString()} ${String(e?.message ?? e)}`); console.error("[soak] session error", String(e?.message ?? e)); });
    for (const ev of ["participant-joined", "participant-left", "stream-added", "stream-removed", "topology-changed", "state-changed", "replaced"]) {
      s.on(ev, (x: any) => {
        if (s !== session) return;
        console.log(`[soak] event ${ev} ${x?.agentDid ? x.agentDid.slice(-8) + "/" + x.participantId : typeof x === "string" ? x : x?.id ?? ""}`);
        render();
      });
    }
    stream ??= await navigator.mediaDevices.getUserMedia({ audio: true, video: { width: 320, height: 240 } });
    await s.join(stream);
    joinedOnce = true;
  }
  async function leave(): Promise<void> {
    const s = session;
    session = null;
    render();
    await s?.leave();
    await s?.destroy?.();
  }
  async function setTrack(kind: "audio" | "video", on: boolean): Promise<void> {
    const track = kind === "audio" ? stream!.getAudioTracks()[0] : stream!.getVideoTracks()[0];
    await session.replaceTrack(kind, on ? track : null);
  }
  async function quality(pref: string): Promise<void> {
    await session.setQualityPreference(pref);
  }
  function networkChange(): void {
    window.dispatchEvent(new Event("online"));
  }
  let hidden = false;
  Object.defineProperty(document, "visibilityState", { configurable: true, get: () => (hidden ? "hidden" : "visible") });
  Object.defineProperty(document, "hidden", { configurable: true, get: () => hidden });
  function setHidden(h: boolean): void {
    hidden = h;
    document.dispatchEvent(new Event("visibilitychange"));
  }

  async function sample(): Promise<any> {
    const live = connections.filter((pc) => pc.connectionState !== "closed");
    const pcs: any[] = [];
    for (const pc of live) {
      const st = await pc.getStats();
      const inbound: any[] = [];
      const sent: any[] = [];
      const midOf = new Map<string, string | null>();
      for (const t of pc.getTransceivers()) if (t.receiver.track) midOf.set(t.receiver.track.id, t.mid);
      st.forEach((s: any) => {
        if (s.type === "inbound-rtp") inbound.push({ mid: s.mid ?? midOf.get(s.trackIdentifier) ?? null, kind: s.kind, pkts: s.packetsReceived ?? 0, bytes: s.bytesReceived ?? 0, frames: s.framesDecoded ?? 0 });
        if (s.type === "outbound-rtp") sent.push({ kind: s.kind, rid: s.rid, pkts: s.packetsSent ?? 0 });
      });
      pcs.push({ conn: pc.connectionState, ice: pc.iceConnectionState, sig: pc.signalingState, recv: pc.getReceivers().filter((r) => r.track?.readyState === "live").length, inbound, sent });
    }
    const pairs: any[] = [];
    const now = performance.now();
    for (const p of session?.participants ?? []) {
      const ids = new Set(p.stream.getTracks().filter((t: MediaStreamTrack) => t.readyState === "live").map((t: MediaStreamTrack) => t.id));
      const pair: any = { did: p.agentDid, participantId: p.participantId, tracks: ids.size, a: { pkts: 0, bytes: 0, level: 0 }, v: { pkts: 0, bytes: 0, frames: 0, width: 0 }, peak: 0, luma: luma(p.participantId), pc: [] };
      for (const pc of live) {
        let carries = false;
        for (const receiver of pc.getReceivers()) {
          if (!receiver.track || !ids.has(receiver.track.id)) continue;
          carries = true;
          const st = await receiver.getStats();
          st.forEach((s: any) => {
            if (s.type !== "inbound-rtp") return;
            if (s.kind === "audio") {
              pair.a.pkts += s.packetsReceived ?? 0;
              pair.a.bytes += s.bytesReceived ?? 0;
              pair.a.level = Math.max(pair.a.level, s.audioLevel ?? 0);
            } else {
              pair.v.pkts += s.packetsReceived ?? 0;
              pair.v.bytes += s.bytesReceived ?? 0;
              pair.v.frames += s.framesDecoded ?? 0;
              pair.v.width = Math.max(pair.v.width, s.frameWidth ?? 0);
            }
          });
        }
        if (carries) pair.pc.push(`${pc.connectionState}/${pc.iceConnectionState}`);
      }
      for (const t of p.stream.getAudioTracks()) for (const { at, value } of meters.get(t.id)?.peaks ?? []) if (at >= now - 3_000) pair.peak = Math.max(pair.peak, value);
      pairs.push(pair);
    }
    return { state: session?.getState?.() ?? "none", topology: session ? (sfuConfig?.mode === "mesh" ? "mesh" : "sfu") : null, joined: joinedOnce, pcs, pairs, errors: errors.slice(-5) };
  }
  function descriptions(): any[] {
    return connections.filter((pc) => pc.connectionState !== "closed").map((pc) => ({
      local: pc.localDescription?.sdp ?? null, remote: pc.remoteDescription?.sdp ?? null,
      transceivers: pc.getTransceivers().map((t) => ({ mid: t.mid, dir: t.direction, cur: t.currentDirection, kind: t.receiver.track?.kind, track: t.receiver.track?.id, state: t.receiver.track?.readyState, muted: t.receiver.track?.muted })),
    }));
  }
  function participants(): any[] {
    return (session?.participants ?? []).map((p: any) => ({ did: p.agentDid, id: p.participantId, tracks: p.stream.getTracks().map((t: any) => `${t.kind}:${t.id}:${t.readyState}`) }));
  }
  /** RTP packets in (all mids) and out of the tab's live connections, and how many connections it ever made. */
  async function transport(): Promise<any> {
    const live = connections.filter((pc) => pc.connectionState !== "closed");
    let inbound = 0;
    let outbound = 0;
    for (const pc of live) {
      (await pc.getStats()).forEach((s: any) => {
        if (s.type === "inbound-rtp") inbound += s.packetsReceived ?? 0;
        if (s.type === "outbound-rtp") outbound += s.packetsSent ?? 0;
      });
    }
    return { made: connections.length, live: live.map((pc) => `${pc.connectionState}/${pc.iceConnectionState}`), inbound, outbound };
  }
  /** The SHA-256 of the SDK bundle this page was served. */
  async function bundleHash(): Promise<string> {
    const digest = await crypto.subtle.digest("SHA-256", await (await fetch("/ad4m.js")).arrayBuffer());
    return Array.from(new Uint8Array(digest), (b) => b.toString(16).padStart(2, "0")).join("");
  }
  w.__soak = { join, leave, setTrack, quality, networkChange, setHidden, sample, descriptions, participants, transport, bundleHash };
}

/**
 * A UDP relay between one tab and the remote ends it calls, with loss, delay and outage:
 * the network trouble of one client, without root (no `tc`). One front socket per remote
 * address, on the remote's own IP; the tab is told that socket's port in place of the remote's.
 */
class Relay {
  loss = 0;
  delayMs = 0;
  down = false;
  passed = 0;
  dropped = 0;
  private fronts = new Map<string, Promise<number>>();
  private sockets: Socket[] = [];
  private rebinders: (() => Promise<number>)[] = [];
  portFor(target: string): Promise<number> {
    let port = this.fronts.get(target);
    if (!port) {
      port = this.open(target);
      this.fronts.set(target, port);
    }
    return port;
  }
  set(c: { loss?: number; delayMs?: number; down?: boolean }): void {
    this.loss = c.loss ?? 0;
    this.delayMs = c.delayMs ?? 0;
    this.down = c.down ?? false;
  }
  private pass(send: () => void): void {
    if (this.down || Math.random() * 100 < this.loss) {
      this.dropped++;
      return;
    }
    this.passed++;
    if (this.delayMs > 0) setTimeout(send, this.delayMs + Math.random() * this.delayMs * 0.2);
    else send();
  }
  private async open(target: string): Promise<number> {
    const at = target.lastIndexOf(":");
    const host = target.slice(0, at);
    const port = Number(target.slice(at + 1));
    const front = createSocket("udp4");
    this.sockets.push(front);
    let client: { address: string; port: number } | null = null;
    // The outward socket is the NAT's public port: `rebind` swaps it for a new one, as a NAT
    // that lost its mapping does. Packets to the old port are gone; the tab sees no change.
    let back: Socket;
    const openBack = async () => {
      const next = createSocket("udp4");
      this.sockets.push(next);
      next.on("message", (msg) => {
        const to = client;
        if (to && next === back) this.pass(() => front.send(msg, to.port, to.address));
      });
      await new Promise<void>((resolve) => next.bind(0, resolve));
      const old = back;
      back = next;
      old?.close();
      return next.address().port;
    };
    await openBack();
    this.rebinders.push(openBack);
    front.on("message", (msg, from) => {
      client = { address: from.address, port: from.port };
      this.pass(() => back.send(msg, port, host));
    });
    await new Promise<void>((resolve) => front.bind(0, host, resolve));
    return front.address().port;
  }
  /** Every outward port changes, as at a NAT rebinding; returns the new ports. */
  async rebind(): Promise<number[]> {
    return Promise.all(this.rebinders.map((r) => r()));
  }
  targets(): number {
    return this.fronts.size;
  }
  close(): void {
    for (const s of this.sockets) {
      try {
        s.close();
      } catch {
        // closed at a rebind
      }
    }
  }
}

interface Tab {
  name: string;
  did: string;
  browser: Browser;
  context: BrowserContext;
  page: Page;
  cdp: CDPSession;
  consoleFile: string;
  consoleLines: string[];
}

describe(`SFU soak: five Chromium tabs in one ${TOPOLOGY} call`, function () {
  let world: World | null = null;
  let server: Server | null = null;
  const tabs: Tab[] = [];
  let jsonl: WriteStream;
  let executorLog: WriteStream;
  let pageUrl = "";
  const relay = new Relay();
  /** The tab whose network the third phase shapes. */
  const SHAPED = "u3";
  const originalLog = console.log;
  const say = (line: string) => {
    const text = `${new Date().toISOString()} ${line}`;
    process.stdout.write(`[soak] ${text}\n`);
    appendFileSync(path.join(RUN_DIR, "soak.log"), text + "\n");
  };

  before(async function () {
    this.timeout(600_000);
    mkdirSync(RUN_DIR, { recursive: true });
    executorLog = createWriteStream(path.join(RUN_DIR, "executor.log"));
    // The executor's output reaches this process's console.log (spawnExecutor): keep it, with its time.
    console.log = (...parts: any[]) => {
      const at = new Date().toISOString();
      for (const line of parts.map(String).join(" ").split("\n")) if (line.trim()) executorLog.write(`${at} ${line}\n`);
    };
    jsonl = createWriteStream(path.join(RUN_DIR, "samples.jsonl"));
    say(`run dir ${RUN_DIR}, topology ${TOPOLOGY}, scale ${SCALE}`);
    const sfuIp = process.env.SFU_BROWSERS_SFU_ADDR || (await outboundIp()) || "127.0.0.1";
    const served = await servePage();
    server = served.server;
    pageUrl = served.url;
    world = await startWorld({
      id: `soak5-${TOPOLOGY}-${Date.now() % 100000}`,
      executorEnv: {
        AD4M_SFU_BIND_ADDR: sfuIp,
        RUST_LOG: process.env.SOAK_RUST_LOG ?? "warn,rust_executor=info,rust_executor::sfu=debug,rust_executor::api::sfu_ws=debug",
      },
      users: NAMES.map((name) => ({ name })),
      neighbourhoods: [{ name: "atlas", owner: "u1", members: NAMES, call: { mode: TOPOLOGY === "mesh" ? "mesh" : "designated" } }],
    });
    say(`world up, SFU on ${sfuIp}; dids: ${NAMES.map((n) => `${n}=${short(world!.users[n].did)}`).join(" ")}`);
    for (const name of NAMES) {
      // The shaped seat may call from Firefox (SOAK_SHAPED_BROWSER=firefox): its ICE nominates differently.
      const kind: BrowserKind = name === SHAPED && process.env.SOAK_SHAPED_BROWSER === "firefox" ? "firefox" : "chromium";
      const launched = await launch(kind);
      if (!launched.browser) throw new Error(launched.reason);
      const browser = launched.browser;
      const context = await browser.newContext(kind === "firefox" ? {} : { permissions: ["camera", "microphone"] });
      if (name === SHAPED && TOPOLOGY === "sfu") await context.exposeFunction("__soakRelay", (target: string) => relay.portFor(target));
      await context.addInitScript(installSoak);
      const page = await context.newPage();
      const cdp = kind === "chromium" ? await context.newCDPSession(page) : (null as unknown as CDPSession);
      const consoleFile = path.join(RUN_DIR, `console-${name}.log`);
      const tab: Tab = { name, did: world.users[name].did, browser, context, page, cdp, consoleFile, consoleLines: [] };
      const record = (line: string) => {
        const text = `${new Date().toISOString()} ${line.replace(/token=[^'"\s&]+/g, "token=…")}`;
        tab.consoleLines.push(text);
        if (tab.consoleLines.length > 5000) tab.consoleLines.splice(0, 1000);
        appendFileSync(consoleFile, text + "\n");
      };
      page.on("console", (m) => record(`${m.type()}: ${m.text()}`));
      page.on("pageerror", (e) => record(`pageerror: ${e.message}`));
      page.on("crash", () => record("PAGE CRASHED"));
      await page.goto(pageUrl);
      await page.waitForFunction(() => (window as any).ad4mReady === true);
      say(`${name} (${kind}) loaded SDK bundle sha256 ${await page.evaluate(() => (window as any).__soak.bundleHash())}`);
      tabs.push(tab);
    }
  });

  after(async function () {
    this.timeout(180_000);
    for (const tab of tabs) {
      await Promise.race([tab.page.evaluate(() => (window as any).__soak.leave()).catch(() => {}), new Promise((r) => setTimeout(r, 10_000))]);
      await tab.browser.close().catch(() => {});
    }
    server?.close();
    relay.close();
    await world?.stop().catch(() => {});
    jsonl?.end();
    executorLog?.end();
    console.log = originalLog;
  });

  it("stays in the call through the phases; every receiver keeps getting every sender", async function () {
    const phaseMs = (s: number) => Math.round(s * 1000 * SCALE);
    this.timeout(phaseMs(1700) + 30 * 60_000);
    const w = world!;
    const url = w.url("atlas");
    const joinArgs = (name: string) => ({ executorUrl: `http://127.0.0.1:${w.users[name].port}`, token: w.users[name].token, neighbourhoodUrl: url, room: ROOM, name });
    const byDid = new Map(tabs.map((t) => [t.did, t]));
    const ev = (t: Tab, fn: string, arg?: any, timeout = 60_000) =>
      Promise.race([
        t.page.evaluate(([f, a]) => (window as any).__soak[f as string](a), [fn, arg] as const),
        new Promise((_, reject) => setTimeout(() => reject(new Error(`${t.name}.${fn} timed out`)), timeout)),
      ]);

    // ── state for the detector ──────────────────────────────────────
    let phase = "join";
    const startedAt = Date.now();
    /** Windows in which a tab's pairs are expected to stall (it left, its network is cut, it is frozen). */
    const disrupted = new Map<string, number>(); // name -> until (ms); Infinity while ongoing
    const disrupt = (name: string) => disrupted.set(name, Infinity);
    const settle = (name: string) => disrupted.set(name, Date.now() + GRACE_MS);
    const isDisrupted = (name: string, at: number) => (disrupted.get(name) ?? 0) > at;
    const prev = new Map<string, number>(); // `${r}>${s}` -> app-level packet sum
    const lastGrowth = new Map<string, number>();
    const transportPrev = new Map<string, number>(); // `${r}` -> transport packet sum
    const transportGrowth = new Map<string, number>();
    const deadOpen = new Map<string, { since: number; expected: boolean; phase: string }>();
    const deadEvents: any[] = [];
    const asymOpen = new Map<string, number>();
    const asymEvents: any[] = [];
    const actions: any[] = [];
    const act = (what: string) => {
      actions.push({ t: new Date().toISOString(), phase, what });
      say(`[${phase}] ${what}`);
    };

    async function sampleAll(): Promise<void> {
      const at = Date.now();
      const iso = new Date(at).toISOString();
      const results = await Promise.all(tabs.map((t) => (ev(t, "sample", undefined, 4_000) as Promise<TabSample>).catch((e) => ({ error: String(e?.message ?? e) }))));
      const view = new Map<string, TabSample | null>();
      tabs.forEach((t, i) => {
        const r: any = results[i];
        view.set(t.name, r.error ? null : r);
        const transport = r.error ? null : (r as TabSample).pcs.reduce((n, pc) => n + pc.inbound.reduce((m, x) => m + x.pkts, 0), 0);
        jsonl.write(JSON.stringify({ t: iso, type: "tab", phase, tab: t.name, ...(r.error ? { error: r.error } : { state: r.state, joined: r.joined, pcs: r.pcs.map((pc: any) => ({ conn: pc.conn, ice: pc.ice, sig: pc.sig, recv: pc.recv, inMids: pc.inbound.length, inPkts: pc.inbound.reduce((m: number, x: any) => m + x.pkts, 0), sent: pc.sent })), participants: r.pairs.length, errors: r.errors }) }) + "\n");
        if (transport !== null) {
          if (transportPrev.get(t.name) !== transport) transportGrowth.set(t.name, at);
          transportPrev.set(t.name, transport);
        }
      });
      // Pair counters.
      for (const r of tabs) {
        const s = view.get(r.name);
        if (!s) continue; // no reading (frozen or hung): nothing learnt
        for (const snd of tabs) {
          if (snd === r) continue;
          const key = `${r.name}>${snd.name}`;
          const p = s.pairs.filter((x) => x.did === snd.did);
          const sum = p.reduce((n, x) => n + x.a.pkts + x.v.pkts, 0);
          const pair = p[p.length - 1];
          jsonl.write(JSON.stringify({
            t: iso, type: "pair", phase, r: r.name, s: snd.name, listed: p.length,
            ...(pair ? { pid: pair.participantId, tracks: pair.tracks, aPkts: pair.a.pkts, aBytes: pair.a.bytes, aLevel: pair.a.level, vPkts: pair.v.pkts, vBytes: pair.v.bytes, frames: pair.v.frames, width: pair.v.width, peak: Number(pair.peak.toFixed(4)), luma: pair.luma === null ? null : Math.round(pair.luma), pc: pair.pc } : {}),
            rState: s.state, rPc: s.pcs.map((pc) => `${pc.conn}/${pc.ice}`),
          }) + "\n");
          if (!lastGrowth.has(key) || (sum > 0 && prev.get(key) !== sum)) lastGrowth.set(key, at);
          prev.set(key, sum);
        }
      }
      // Dead pairs and the asymmetric pattern.
      const receiving = (r: string, s: string) => at - (lastGrowth.get(`${r}>${s}`) ?? at) < DEAD_MS;
      for (const r of tabs) {
        if (!view.get(r.name)) continue;
        const deadFrom: string[] = [];
        for (const snd of tabs) {
          if (snd === r) continue;
          const key = `${r.name}>${snd.name}`;
          const othersGetIt = tabs.some((o) => o !== r && o !== snd && receiving(o.name, snd.name));
          const dead = !receiving(r.name, snd.name) && othersGetIt;
          const expected = isDisrupted(r.name, at) || isDisrupted(snd.name, at);
          const open = deadOpen.get(key);
          if (dead) {
            deadFrom.push(snd.name);
            if (!open) {
              deadOpen.set(key, { since: lastGrowth.get(key)!, expected, phase });
              say(`DEAD ${key} (no growth since ${new Date(lastGrowth.get(key)!).toISOString()}, expected=${expected})`);
            } else if (!expected) open.expected = false;
          } else if (open) {
            deadOpen.delete(key);
            const e = { pair: key, from: new Date(open.since).toISOString(), to: iso, seconds: Math.round((at - open.since) / 1000), expected: open.expected, phase: open.phase, endPhase: phase };
            deadEvents.push(e);
            jsonl.write(JSON.stringify({ t: iso, type: "dead", ...e }) + "\n");
            say(`ALIVE again ${key} after ${e.seconds} s`);
          }
        }
        const others = tabs.filter((o) => o !== r);
        const asym = deadFrom.length === others.length && others.every((o) => receiving(o.name, r.name));
        if (asym && !asymOpen.has(r.name)) {
          asymOpen.set(r.name, at);
          const expected = isDisrupted(r.name, at) || others.some((o) => isDisrupted(o.name, at));
          const e = { t: iso, receiver: r.name, phase, expected, transportGrowing: at - (transportGrowth.get(r.name) ?? 0) < DEAD_MS, lastActions: actions.slice(-6) };
          asymEvents.push(e);
          jsonl.write(JSON.stringify({ type: "asym", ...e }) + "\n");
          say(`ASYMMETRIC: ${r.name} receives none of the 4 while all 4 receive it (expected=${expected}, transport still growing=${e.transportGrowing})`);
          void dumpAsym(r, at, e).catch((err) => say(`dump failed: ${err}`));
        } else if (!asym && asymOpen.has(r.name)) {
          const since = asymOpen.get(r.name)!;
          asymOpen.delete(r.name);
          say(`asymmetric state of ${r.name} ended after ${Math.round((at - since) / 1000)} s`);
          jsonl.write(JSON.stringify({ t: iso, type: "asym-end", receiver: r.name, seconds: Math.round((at - since) / 1000) }) + "\n");
        }
      }
    }

    async function dumpAsym(r: Tab, at: number, e: any): Promise<void> {
      const stamp = new Date(at).toISOString().replace(/[:.]/g, "-");
      const dir = path.join(RUN_DIR, `asym-${r.name}-${stamp}`);
      mkdirSync(dir, { recursive: true });
      writeFileSync(path.join(dir, "event.json"), JSON.stringify(e, null, 1));
      writeFileSync(path.join(dir, "console.log"), r.consoleLines.join("\n"));
      const state = await Promise.all([ev(r, "descriptions", undefined, 10_000), ev(r, "participants", undefined, 10_000), ev(r, "sample", undefined, 10_000)]).catch((err) => String(err));
      writeFileSync(path.join(dir, "page-state.json"), JSON.stringify(state, null, 1));
      // The executor log 120 s before to 60 s after.
      await new Promise((resolve) => setTimeout(resolve, 60_000));
      const lines = readFileSync(path.join(RUN_DIR, "executor.log"), "utf8").split("\n");
      const inWindow = lines.filter((l) => {
        const t = Date.parse(l.slice(0, 24));
        return t >= at - 120_000 && t <= at + 60_000;
      });
      writeFileSync(path.join(dir, "executor-window.log"), inWindow.join("\n"));
      writeFileSync(path.join(dir, "executor-sfu-window.log"), inWindow.filter((l) => /sfu/i.test(l)).join("\n"));
      say(`dumped ${dir}`);
    }

    // ── the sampler runs through every phase ────────────────────────
    let sampling = true;
    const sampler = (async () => {
      while (sampling) {
        const next = Date.now() + SAMPLE_MS;
        await sampleAll().catch((err) => say(`sample failed: ${err}`));
        await new Promise((resolve) => setTimeout(resolve, Math.max(0, next - Date.now())));
      }
    })();
    const wait = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));
    const runFor = async (ms: number, step: (i: number) => Promise<void>, every: number) => {
      const end = Date.now() + ms;
      for (let i = 0; Date.now() < end; i++) {
        const next = Date.now() + every;
        await step(i).catch((err) => say(`[${phase}] step ${i} failed: ${err?.message ?? err}`));
        await wait(Math.max(0, Math.min(next, end) - Date.now()));
      }
    };
    const leaveRejoin = async (t: Tab, away = 5_000) => {
      disrupt(t.name);
      act(`${t.name} leaves`);
      await ev(t, "leave");
      await wait(away);
      act(`${t.name} rejoins`);
      await ev(t, "join", undefined, 90_000);
      settle(t.name);
    };
    /** Shape the shaped tab's media (relay; SFU only) and, with `offline`, cut its signalling too. */
    const net = async (t: Tab, c: { loss?: number; delayMs?: number; down?: boolean; offline?: boolean }) => {
      say(`relay so far: ${relay.passed} packets passed, ${relay.dropped} dropped, ${relay.targets()} target(s)`);
      relay.set(c);
      await t.context.setOffline(c.offline ?? false);
    };

    let stopProbe = () => {};
    try {
      // ── join ──────────────────────────────────────────────────────
      for (const t of tabs) {
        act(`${t.name} joins`);
        await ev(t, "join", joinArgs(t.name), 90_000);
      }
      const deadline = Date.now() + 120_000;
      for (;;) {
        await wait(SAMPLE_MS);
        const now = Date.now();
        const missing = tabs.flatMap((r) => tabs.filter((s) => s !== r).map((s) => `${r.name}>${s.name}`))
          .filter((key) => (prev.get(key) ?? 0) === 0 || now - (lastGrowth.get(key) ?? 0) > 2 * SAMPLE_MS + 1_000);
        if (missing.length === 0) break;
        if (now > deadline) throw new Error(`not every pair receives after joining: ${missing.join(", ")}`);
      }
      // Media is real: audible and not black, everywhere.
      const check = await Promise.all(tabs.map((t) => ev(t, "sample") as Promise<TabSample>));
      const problems = check.flatMap((s, i) => s.pairs.filter((p) => !(p.peak > 0.01) || (p.luma !== null && !(p.luma > 10))).map((p) => `${tabs[i].name}<${byDid.get(p.did)?.name}: peak ${p.peak.toFixed(3)} luma ${p.luma}`));
      say(problems.length ? `media check: ${problems.join("; ")}` : "media check: every pair audible and not black");

      // u3's receive and send, every second, for timing a gap to the second.
      const probed = tabs.find((t) => t.name === SHAPED)!;
      const probeFile = createWriteStream(path.join(RUN_DIR, `probe-${SHAPED}.jsonl`));
      const probe = setInterval(() => {
        void ev(probed, "transport", undefined, 5_000)
          .then((r) => probeFile.write(JSON.stringify({ t: new Date().toISOString(), phase, ...(r as object) }) + "\n"))
          .catch(() => {});
      }, 1_000);
      stopProbe = () => { clearInterval(probe); probeFile.end(); };

      if (ONLY_REBIND) {
        if (TOPOLOGY !== "sfu") throw new Error("SOAK_PHASES=rebind needs the SFU: the relay carries only its media");
        phase = "1-quiet";
        act("quiet");
        await wait(30_000);
        // Nobody else sends: u3 gets no media, and must not take that for a dead path.
        phase = "2-silent";
        const others = tabs.filter((t) => t !== probed);
        for (const t of others) disrupt(t.name);
        act(`everyone but ${probed.name} turns camera and mic off for 30 s`);
        const tracks = (on: boolean) =>
          Promise.all(others.map((t) => t.page.evaluate(async (o) => { const soak = (window as any).__soak; await soak.setTrack("audio", o); await soak.setTrack("video", o); }, on)));
        await tracks(false);
        await wait(30_000);
        await tracks(true);
        act("camera and mic back on");
        for (const t of others) settle(t.name);
        await wait(20_000);
        phase = "3-network";
        const failures: string[] = [];
        const pairKeys = () => tabs.flatMap((r) => tabs.filter((x) => x !== r).map((x) => `${r.name}>${x.name}`));
        const rebindings = [
          async () => { act(`${probed.name}: NAT rebinding, no loss: new public port ${await relay.rebind()}`); },
          async () => {
            act(`${probed.name}: 3 s blip, then NAT rebinding`); await net(probed, { down: true }); await wait(3_000);
            await net(probed, {}); act(`${probed.name}: new public port ${await relay.rebind()}`);
          },
        ];
        for (const [i, rebind] of rebindings.entries()) {
          disrupt(probed.name);
          await rebind();
          const at = Date.now();
          // Every pair receives again within RECOVER_MS, read on the sampling grid: a pair
          // counts once a sample after that bound saw it grow.
          await wait(RECOVER_MS + SAMPLE_MS + 1_000);
          const late = pairKeys().filter((key) => (lastGrowth.get(key) ?? 0) < at + RECOVER_MS - 1_000);
          if (late.length) failures.push(`rebinding ${i + 1}: not receiving ${RECOVER_MS / 1000} s after it: ${late.join(", ")}`);
          say(late.length ? `RECOVERY FAILED after rebinding ${i + 1}: ${late.join(", ")}` : `rebinding ${i + 1}: every pair receives again within ${RECOVER_MS / 1000} s`);
          // Then u3 settles and its grace runs out with a dead window to spare: a pair still
          // dead by then, or dead again, is an unexpected event.
          settle(probed.name);
          await wait(GRACE_MS + DEAD_MS + SAMPLE_MS);
        }
        phase = "3b-quiet";
        const now = Date.now();
        const silent = pairKeys().filter((key) => now - (lastGrowth.get(key) ?? 0) > 2 * SAMPLE_MS + 1_000);
        if (silent.length) failures.push(`at the end, not receiving: ${silent.join(", ")}`);
        const unexpectedDead = [...deadEvents, ...[...deadOpen].map(([pair, o]) => ({ pair, ...o, from: new Date(o.since).toISOString(), to: "still dead" }))].filter((e: any) => !e.expected);
        if (unexpectedDead.length) failures.push(`unexpected dead pairs: ${unexpectedDead.map((e: any) => `${e.pair} from ${e.from} to ${e.to}`).join("; ")}`);
        const unexpectedAsym = asymEvents.filter((e) => !e.expected);
        if (unexpectedAsym.length) failures.push(`unexpected asymmetric events: ${unexpectedAsym.map((e) => `${e.receiver} at ${e.t}`).join("; ")}`);
        if (asymOpen.size) failures.push(`still asymmetric at the end: ${[...asymOpen.keys()].join(", ")}`);
        act(failures.length ? `FAILED: ${failures.join(" | ")}` : "every pair recovered after each rebinding; no unexpected dead or asymmetric event");
        if (failures.length) throw new Error(`receive recovery failed:\n${failures.join("\n")}`);
        return;
      }

      // ── 1. quiet baseline ─────────────────────────────────────────
      phase = "1-quiet";
      act("quiet baseline");
      await wait(phaseMs(240));

      // ── 2. churn ─────────────────────────────────────────────────
      phase = "2-churn";
      const prefs = ["low", "medium", "high", "auto"];
      await runFor(phaseMs(300), async (i) => {
        const t = tabs[i % 5];
        const setTrack = (kind: string, on: boolean) => t.page.evaluate(([k, o]) => (window as any).__soak.setTrack(k, o), [kind, on] as const);
        switch (i % 4) {
          case 0:
            await leaveRejoin(t);
            break;
          case 1:
            act(`${t.name} camera off 8 s`);
            await setTrack("video", false);
            await wait(8_000);
            await setTrack("video", true);
            break;
          case 2:
            act(`${t.name} mic off 8 s`);
            await setTrack("audio", false);
            await wait(8_000);
            await setTrack("audio", true);
            break;
          case 3:
            act(`${t.name} quality low → medium → high → auto`);
            for (const p of prefs) {
              await ev(t, "quality", p).catch((e) => say(`quality ${p}: ${e}`));
              await wait(2_000);
            }
            break;
        }
      }, 20_000);
      phase = "2b-quiet";
      act("quiet");
      await wait(phaseMs(120));

      // ── 3. network trouble on one client (u3) ────────────────────
      phase = "3-network";
      const victim = tabs.find((t) => t.name === SHAPED)!;
      disrupt(victim.name);
      const nphase = phaseMs(300) / 5;
      if (TOPOLOGY === "mesh") act("mesh: media is not relayed, so only the signalling outages below shape this tab");
      act(`${victim.name}: 10% loss, 200 ms delay`); await net(victim, { loss: 10, delayMs: 200 }); await wait(nphase);
      act(`${victim.name}: 30% loss, 50 ms delay`); await net(victim, { loss: 30, delayMs: 50 }); await wait(nphase);
      act(`${victim.name}: full outage 20 s (media and signalling)`); await net(victim, { down: true, offline: true }); await wait(20_000);
      act(`${victim.name}: back`); await net(victim, {}); await wait(nphase);
      act(`${victim.name}: 800 ms delay`); await net(victim, { delayMs: 800 }); await wait(nphase);
      act(`${victim.name}: media outage 8 s (shorter than the SFU's 10 s drop), signalling up`); await net(victim, { down: true }); await wait(8_000);
      act(`${victim.name}: back`); await net(victim, {}); await wait(nphase / 2);
      act(`${victim.name}: signalling outage 15 s, media up`); await net(victim, { offline: true }); await wait(15_000);
      act(`${victim.name}: back`); await net(victim, {}); await wait(nphase / 2);
      if (TOPOLOGY === "sfu") {
        act(`${victim.name}: NAT rebinding, no loss: new public port ${await relay.rebind()}`); await wait(nphase);
        act(`${victim.name}: 3 s blip, then NAT rebinding`); await net(victim, { down: true }); await wait(3_000);
        await net(victim, {}); act(`${victim.name}: new public port ${await relay.rebind()}`); await wait(nphase);
      }
      act(`${victim.name}: clear`); await net(victim, {});
      settle(victim.name);
      phase = "3b-quiet";
      await wait(phaseMs(120));

      // ── 4. a tab in the background ───────────────────────────────
      phase = "4-background";
      const bg = tabs[3];
      disrupt(bg.name);
      act(`${bg.name}: hidden (visibilitychange), CPU throttled 20x for 60 s`);
      await ev(bg, "setHidden", true);
      await bg.cdp.send("Emulation.setCPUThrottlingRate", { rate: 20 });
      await wait(60_000);
      await bg.cdp.send("Emulation.setCPUThrottlingRate", { rate: 1 });
      await ev(bg, "setHidden", false);
      act(`${bg.name}: visible again`);
      await wait(30_000);
      act(`${bg.name}: page frozen (lifecycle) for 60 s`);
      await bg.cdp.send("Page.enable");
      await bg.cdp.send("Page.setWebLifecycleState", { state: "frozen" });
      await wait(60_000);
      await bg.cdp.send("Page.setWebLifecycleState", { state: "active" });
      act(`${bg.name}: active again`);
      settle(bg.name);
      await wait(Math.max(0, phaseMs(180) - 150_000));
      phase = "4b-quiet";
      await wait(phaseMs(120));

      // ── 5. two renegotiate at once (glare) ───────────────────────
      phase = "5-glare";
      const pairsOf = [[0, 1], [1, 4], [2, 3], [0, 4]];
      await runFor(phaseMs(180), async (i) => {
        const [a, b] = pairsOf[i % pairsOf.length].map((k) => tabs[k]);
        switch (i % 3) {
          case 0:
            act(`${a.name} + ${b.name}: network change (ICE restart) at once`);
            await Promise.all([ev(a, "networkChange"), ev(b, "networkChange")]);
            break;
          case 1:
            act(`${a.name} + ${b.name}: leave and rejoin at once`);
            await Promise.all([leaveRejoin(a, 1_000), leaveRejoin(b, 1_000)]);
            break;
          case 2:
            act(`${a.name} rejoins while ${b.name} restarts ICE and changes quality`);
            await Promise.all([leaveRejoin(a, 500), ev(b, "networkChange"), ev(b, "quality", "low"), wait(3_000).then(() => ev(b, "quality", "auto"))]);
            break;
        }
      }, 25_000);
      phase = "5b-quiet";
      act("quiet (final)");
      await wait(phaseMs(120));
    } finally {
      stopProbe();
      sampling = false;
      await sampler;
      for (const [key, open] of deadOpen) deadEvents.push({ pair: key, from: new Date(open.since).toISOString(), to: "end", seconds: Math.round((Date.now() - open.since) / 1000), expected: open.expected, phase: open.phase, endPhase: "end" });
      const summary = { topology: TOPOLOGY, scale: SCALE, minutes: Math.round((Date.now() - startedAt) / 60_000), deadEvents, asymEvents, actions };
      writeFileSync(path.join(RUN_DIR, "summary.json"), JSON.stringify(summary, null, 1));
      say(`done: ${summary.minutes} min, ${deadEvents.length} dead-pair events (${deadEvents.filter((e) => !e.expected).length} unexpected), ${asymEvents.length} asymmetric (${asymEvents.filter((e) => !e.expected).length} unexpected)`);
    }
  });
});
