/**
 * T21: Users who share an executor find and hear each other in a call.
 *
 * The users of one multi-user executor share its single instance of a
 * neighbourhood's link language, and a link language holds one online status
 * per node. Before coasys/ad4m#1431, `onlineAgents` gave co-located users an
 * empty status and each `setOnlineStatus` overwrote the node's status for the
 * others on it. A mesh call's roster comes from those statuses, so two users
 * on one executor calling `createSession(room)` with default options never saw
 * each other. The executor now keeps each hosted agent's own signed status and
 * publishes them as a bundle that receivers unpack per agent.
 *
 * The other wind-tunnel scenarios cannot catch this: their executors run no
 * languages, and their neighbourhoods are synthetic URLs made members through
 * `sfu.ensureMembership`, so no link language and no online status is ever on
 * the path. T21 stands up two executors with the prepared test languages
 * (Holochain p-diff-sync, as `test-multi-user-with-setup.sh` does), shares real
 * neighbourhoods between managed users, and calls through the SDK `Session`
 * on `@roamhq/wrtc` with synthetic audio:
 *
 *   co-located: alice + bob on node 1 → rosters list each other → audio both
 *     ways → `sfuServesAllMembers` is true → bob leaves → alice sees him go.
 *   mixed: alice + bob on node 1, carol on node 2 → every roster lists the
 *     other two → `sfuServesAllMembers` is false → audio on every pair.
 *
 * The mixed audio check also guards a second bug it found: the executor signed
 * an unsigned signal over each link's `status`, which p-diff-sync does not
 * carry, so every signal from another node failed verification there and the
 * mesh (which drops unverified signalling) never connected across nodes.
 *
 * Needs the test languages prepared in Holochain mode
 * (`pnpm run prepare-test` in tests/js) and fails if they are not.
 * `EXECUTOR_PATH` selects the executor binary, as for the rest of the harness.
 *
 * Asserts:
 *   - Co-located: each roster lists the other within 30 s; the session runs
 *     mesh; audio frames arrive both ways; `sfuServesAllMembers` is true.
 *   - Leave: once bob leaves, alice's session reports `participant-left` for him.
 *   - Mixed: every roster lists the other two within 180 s;
 *     `sfuServesAllMembers` is false; audio frames arrive on every pair.
 */

import { ChildProcess, fork } from "node:child_process";
import { randomUUID } from "node:crypto";
import { existsSync, readFileSync } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { Ad4mClient, Perspective, type MeshParticipant, type Session } from "@coasys/ad4m";
import { Scenario, ScenarioContext, ScenarioResult } from "../scenario.js";
import { specPeerConnection, syntheticAudio } from "./t20-mesh-manager-lossy.js";
import {
  baseUrl, executorBinary, gracefulShutdown, pollUntil, runHcLocalServices, sleep, startExecutor,
} from "../../../utils/utils.js";
import { deregisterPorts, getFreePorts, registerPorts } from "../../ports.js";

const require_ = createRequire(import.meta.url);
let wrtc: any;
try {
  wrtc = require_("@roamhq/wrtc");
} catch (_e) {
  wrtc = null;
}

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const TESTS_JS = path.resolve(__dirname, "..", "..", "..");
const REPO_ROOT = path.resolve(TESTS_JS, "..", "..");
const BOOTSTRAP_SEED = path.join(TESTS_JS, "bootstrapSeed.json");
const DIFF_SYNC_HASH = path.join(TESTS_JS, "scripts", "perspective-diff-sync-hash");
const TST_TMP = path.join(TESTS_JS, "tst-tmp");

const COLOCATED_ROSTER_TIMEOUT_MS = 30_000;
const MIXED_ROSTER_TIMEOUT_MS = 180_000;
const LEAVE_TIMEOUT_MS = 30_000;
const COLOCATED_AUDIO_TIMEOUT_MS = 60_000;
const MIXED_AUDIO_TIMEOUT_MS = 120_000;
/** Audio frames (10 ms each) a pair must deliver to count as flowing. */
const MIN_FRAMES = 50;

/**
 * How many times a run may be lost to the binding crashing before the
 * scenario fails. An assertion failure is never retried. See T20.
 */
const BINDING_CRASH_RETRIES = 1;

/** A managed user the child process logs in as. */
interface CallUser {
  name: string;
  port: number;
  token: string;
  did: string;
}

/** What the parent hands the child: who calls in which neighbourhood. */
export interface T21Config {
  branch: string;
  room: string;
  colocated: { neighbourhoodUrl: string; users: CallUser[] };
  mixed: { neighbourhoodUrl: string; users: CallUser[] };
}

export const t21ColocatedCall: Scenario = {
  id: "t21",
  name: "Co-located users in a call",
  description: "Two users on one executor, then two plus a remote user, call through real neighbourhoods with default session options",
  managesOwnEnvironment: true,

  async run(ctx: ScenarioContext): Promise<ScenarioResult> {
    const diffSyncHash = checkPreparedLanguages();
    if (!wrtc?.nonstandard) {
      throw new Error("T21 needs @roamhq/wrtc (npm install --include=optional).");
    }
    const executor = executorBinary();
    if (!existsSync(executor)) {
      throw new Error(`T21: executor binary not found at ${executor}. Build it or set EXECUTOR_PATH.`);
    }

    const env = await startEnvironment(diffSyncHash);
    try {
      // The media runs in a child, as in T20: `@roamhq/wrtc` occasionally
      // segfaults in its own native teardown, and in-process that would end
      // the mocha run. The executors stay here, so a crash never orphans them.
      const child = fileURLToPath(new URL("./t21-child.ts", import.meta.url));
      const crashes: string[] = [];
      for (let attempt = 0; attempt <= BINDING_CRASH_RETRIES; attempt += 1) {
        // A crashed run leaves its presence links behind; a fresh room ignores them.
        const config: T21Config = { ...env.config, branch: ctx.branch, room: `t21-${attempt}` };
        const outcome = await new Promise<{ result?: ScenarioResult; signal?: string | null; code?: number | null }>((resolve) => {
          const proc = fork(child, [JSON.stringify(config)], { execArgv: process.execArgv, stdio: "inherit" });
          let result: ScenarioResult | undefined;
          proc.on("message", (message) => { result = message as ScenarioResult; });
          proc.on("exit", (code, signal) => resolve({ result, signal, code }));
        });
        if (outcome.result) {
          outcome.result.metrics["executor"] = executor;
          if (crashes.length) outcome.result.metrics["binding_crashes"] = crashes;
          console.log(`[t21] ${outcome.result.summary}`);
          return outcome.result;
        }
        if (!outcome.signal) throw new Error(`T21: the call process exited with code ${outcome.code} and no result`);
        crashes.push(outcome.signal);
        console.warn(`[t21] run ${attempt + 1} lost to the libwebrtc binding (${crashes.at(-1)})`);
      }
      throw new Error(`T21 never completed: the libwebrtc binding crashed every run (${crashes.join(", ")})`);
    } finally {
      await env.stop();
    }
  },
};

/**
 * The prepared test languages this scenario needs, or an error saying how to
 * prepare them. Returns the p-diff-sync template address.
 */
function checkPreparedLanguages(): string {
  const prepare = "Prepare them first: `pnpm run prepare-test` in tests/js (Holochain mode, as test-multi-user-with-setup.sh does).";
  const seed = JSON.parse(readFileSync(BOOTSTRAP_SEED, "utf8"));
  if (!seed.languageLanguageBundle) {
    throw new Error(`T21: ${BOOTSTRAP_SEED} has no language-language bundle, so the test languages are not prepared. ${prepare}`);
  }
  const publisher = path.join(TST_TMP, "agents", "p", "ad4m", "agent.json");
  if (!existsSync(publisher) || !seed.trustedAgents?.includes(JSON.parse(readFileSync(publisher, "utf8")).did)) {
    throw new Error(`T21: the agent that published the test languages is not a trusted agent in ${BOOTSTRAP_SEED}. ${prepare}`);
  }
  const hash = existsSync(DIFF_SYNC_HASH) ? readFileSync(DIFF_SYNC_HASH, "utf8").trim() : "";
  if (!hash || !existsSync(path.join(TST_TMP, "languages", `meta-${hash}.json`))) {
    throw new Error(`T21: the p-diff-sync template "${hash}" is not published in ${TST_TMP}/languages. ${prepare}`);
  }
  // Prepared in local mode, "perspective-diff-sync" is the local link
  // language, which syncs nothing between executors.
  const prepared = path.join(TST_TMP, "languages", "perspective-diff-sync", "build", "bundle.js");
  const local = path.join(REPO_ROOT, "bootstrap-languages", "local", "link-language.js");
  if (existsSync(local) && existsSync(prepared) && readFileSync(prepared).equals(readFileSync(local))) {
    throw new Error(`T21: the test languages were prepared in local mode (no Holochain), so node 2 cannot reach node 1. ${prepare}`);
  }
  return hash;
}

/** Two executors with Holochain, three managed users, two shared neighbourhoods. */
async function startEnvironment(diffSyncHash: string): Promise<{
  config: Omit<T21Config, "branch" | "room">;
  stop: () => Promise<void>;
}> {
  const ports = await getFreePorts(6);
  registerPorts(ports);
  const procs: ChildProcess[] = [];
  let services: ChildProcess | null = null;
  const stop = async () => {
    for (const proc of procs) await gracefulShutdown(proc, "t21 executor");
    await gracefulShutdown(services, "t21 local services");
    deregisterPorts(ports);
  };

  try {
    const local = await runHcLocalServices();
    services = local.process;
    const nodes: { port: number; admin: Ad4mClient }[] = [];
    for (const n of [0, 1]) {
      const [port, hcAdmin, hcApp] = ports.slice(n * 3, n * 3 + 3);
      procs.push(await startExecutor(
        path.join(TST_TMP, "agents", `t21-node${n + 1}`), BOOTSTRAP_SEED,
        // Multi-user mode needs an admin credential.
        port, hcAdmin, hcApp, false, "t21-admin", local.proxyUrl!, local.bootstrapUrl!,
      ));
      const admin = new Ad4mClient(baseUrl(port), "t21-admin");
      await admin.agent.generate("passphrase");
      await admin.runtime.setMultiUserEnabled(true);
      nodes.push({ port, admin });
    }

    const user = async (node: { port: number; admin: Ad4mClient }, name: string) => {
      const email = `t21-${name}@example.com`;
      const created = await node.admin.agent.createUser(email, "password");
      if (!created.success) throw new Error(`T21: creating ${name} failed: ${created.error}`);
      await node.admin.runtime.setUserFreeAccess(email, true);
      const token = await node.admin.agent.loginUser(email, "password");
      const client = new Ad4mClient(baseUrl(node.port), token);
      const did = (await client.agent.status()).did!;
      return { client, user: { name, port: node.port, token, did } as CallUser };
    };
    const alice = await user(nodes[0], "alice");
    const bob = await user(nodes[0], "bob");
    const carol = await user(nodes[1], "carol");

    const colocatedUrl = await shareNeighbourhood(diffSyncHash, alice.client, [bob.client], "T21 co-located");
    const mixedUrl = await shareNeighbourhood(diffSyncHash, alice.client, [bob.client, carol.client], "T21 mixed");

    // As in the multi-user suite: nothing reports when K2 is ready for an
    // agent-info exchange, so repeat it a few times.
    for (let attempt = 1; attempt <= 5; attempt++) {
      try {
        const infos1 = await nodes[0].admin.runtime.hcAgentInfos();
        const infos2 = await nodes[1].admin.runtime.hcAgentInfos();
        await nodes[0].admin.runtime.hcAddAgentInfos(infos2);
        await nodes[1].admin.runtime.hcAddAgentInfos(infos1);
      } catch (error) {
        console.log(`[t21] agent info exchange attempt ${attempt} failed:`, error);
      }
      if (attempt < 5) await sleep(3000);
    }

    return {
      config: {
        colocated: { neighbourhoodUrl: colocatedUrl, users: [alice.user, bob.user] },
        mixed: { neighbourhoodUrl: mixedUrl, users: [alice.user, bob.user, carol.user] },
      },
      stop,
    };
  } catch (error) {
    await stop();
    throw error;
  }
}

/** `owner` publishes a neighbourhood on a fresh p-diff-sync clone; `joiners` join it. */
async function shareNeighbourhood(
  diffSyncHash: string, owner: Ad4mClient, joiners: Ad4mClient[], name: string,
): Promise<string> {
  const perspective = await owner.perspective.add(name);
  const language = await owner.languages.applyTemplateAndPublish(diffSyncHash, JSON.stringify({ uid: randomUUID(), name }));
  const url = await owner.neighbourhood.publishFromPerspective(perspective.uuid, language.address, new Perspective([]));
  for (const joiner of joiners) await joiner.neighbourhood.joinFromUrl(url);
  const ready = ["LINK_LANGUAGE_INSTALLED_BUT_NOT_SYNCED", "SYNCED"];
  await pollUntil(async () => {
    for (const client of [owner, ...joiners]) {
      const state = (await client.perspective.all()).find((p) => p.sharedUrl === url)?.state;
      if (!ready.includes(state as string)) return false;
    }
    return true;
  }, { timeoutMs: 120_000, intervalMs: 1000, label: `link language of "${name}" installed for every member` });
  return url;
}

// ── The child: sessions and media ────────────────────────────────────────

interface Call {
  user: CallUser;
  client: Ad4mClient;
  perspectiveUuid: string;
  session: Session;
  topology: string | null;
  /** DIDs in this session's roster. */
  roster: Set<string>;
  /** DIDs this session saw leave. */
  left: Set<string>;
  participants: Map<string, MeshParticipant>;
  /** Audio frames received, per remote DID. */
  frames: Map<string, number>;
  sinks: any[];
  stopAudio: () => void;
  /** Mesh signalling this agent's executor delivered, per author: valid and unverified. */
  signals: Map<string, { valid: number; unverified: number }>;
  errors: string[];
  stopCounting: () => void;
}

/** Join `room` in `neighbourhoodUrl` as `user`, with default session options and a tone. */
async function joinWithAudio(user: CallUser, neighbourhoodUrl: string, room: string, toneHz: number): Promise<Call> {
  const client = new Ad4mClient(baseUrl(user.port), user.token);
  const perspective = (await client.perspective.all()).find((p) => p.sharedUrl === neighbourhoodUrl);
  if (!perspective) throw new Error(`${user.name} has no perspective for ${neighbourhoodUrl}`);
  const proxy = await perspective.getNeighbourhoodProxy();
  const signals = new Map<string, { valid: number; unverified: number }>();
  const countSignal = (signal: any) => {
    const link = signal?.data?.links?.[0];
    if (link?.data?.predicate !== "ad4m://session/rtc" || !link.author || link.author === user.did) return;
    const count = signals.get(link.author) ?? { valid: 0, unverified: 0 };
    if (signal.proof?.valid === true && link.proof?.valid === true) count.valid += 1;
    else count.unverified += 1;
    signals.set(link.author, count);
  };
  await proxy.addSignalHandler(countSignal);
  const session = await proxy.createSession(room);
  const audio = syntheticAudio(toneHz);
  const call: Call = {
    user, client, perspectiveUuid: perspective.uuid, session, topology: null,
    roster: new Set(), left: new Set(), participants: new Map(), frames: new Map(), sinks: [],
    stopAudio: audio.stop, signals, errors: [],
    stopCounting: () => proxy.removeSignalHandler(countSignal),
  };
  session.on("topology-changed", (topology: string) => { call.topology = topology; });
  session.on("error", (error: unknown) => { call.errors.push(error instanceof Error ? error.message : String(error)); });
  const seen = (p: MeshParticipant) => {
    call.roster.add(p.did);
    call.left.delete(p.did);
    if (p.stream) call.participants.set(p.did, p);
  };
  session.on("participant-joined", seen);
  session.on("participant-updated", seen);
  session.on("participant-left", (p: { did: string }) => {
    call.roster.delete(p.did);
    call.left.add(p.did);
    call.participants.delete(p.did);
  });
  try {
    await session.join(audio.stream);
  } catch (error) {
    audio.stop();
    throw error;
  }
  return call;
}

/** Count audio frames from each participant, on whatever audio tracks its stream holds now. */
function listen(call: Call): void {
  // A sink on a track whose connection closed can crash the binding (T20).
  const current = new Set([...call.participants.values()].flatMap((p) => p.stream.getAudioTracks()));
  call.sinks = call.sinks.filter((sink) => {
    if (current.has(sink.track)) return true;
    sink.stop();
    return false;
  });
  for (const participant of call.participants.values()) {
    for (const track of participant.stream.getAudioTracks()) {
      if (call.sinks.some((sink) => sink.track === track)) continue;
      const sink = new wrtc.nonstandard.RTCAudioSink(track);
      sink.track = track;
      sink.ondata = () => call.frames.set(participant.did, (call.frames.get(participant.did) ?? 0) + 1);
      call.sinks.push(sink);
    }
  }
}

function stopSinks(call: Call): void {
  for (const sink of call.sinks) sink.stop();
  call.sinks = [];
}

function rosterLists(calls: Call[]): boolean {
  return calls.every((call) => calls.every((other) => other === call || call.roster.has(other.user.did)));
}

function hearsEveryone(calls: Call[]): boolean {
  for (const call of calls) listen(call);
  return calls.every((call) => calls.every((other) =>
    other === call || (call.frames.get(other.user.did) ?? 0) >= MIN_FRAMES));
}

function rosters(calls: Call[]): string {
  const name = (did: string) => calls.find((c) => c.user.did === did)?.user.name ?? did;
  return calls.map((c) => `${c.user.name}=[${[...c.roster].map(name).join(",")}]`).join(" ");
}

function framesOf(calls: Call[]): Record<string, number> {
  const out: Record<string, number> = {};
  for (const call of calls) {
    for (const other of calls) {
      if (other !== call) out[`${other.user.name}->${call.user.name}`] = call.frames.get(other.user.did) ?? 0;
    }
  }
  return out;
}

/** Per call: each pair's connection state, the signalling received, and session errors. */
function meshState(calls: Call[]): Record<string, unknown> {
  const name = (did: string) => calls.find((c) => c.user.did === did)?.user.name ?? did;
  const out: Record<string, unknown> = {};
  for (const call of calls) {
    out[call.user.name] = {
      peers: Object.fromEntries([...call.session.peerStates()].map(([did, state]) => [name(did), state])),
      signals: Object.fromEntries([...call.signals].map(([did, count]) => [name(did), count])),
      errors: call.errors.slice(-5),
    };
  }
  return out;
}

async function waitFor(condition: () => boolean, timeoutMs: number, what: () => string): Promise<number> {
  const start = Date.now();
  while (!condition()) {
    if (Date.now() - start > timeoutMs) throw new Error(`timed out after ${timeoutMs} ms: ${what()}`);
    await sleep(250);
  }
  return Date.now() - start;
}

async function endCalls(calls: Call[]): Promise<void> {
  for (const call of calls) stopSinks(call);
  for (const call of calls) call.stopAudio();
  for (const call of calls) call.stopCounting();
  for (const call of calls) {
    try { await call.session.destroy(); } catch (error) { console.warn(`[t21] destroying ${call.user.name}'s session:`, error); }
  }
}

/** The scenario itself, run in the child (see `t21-child.ts`). */
export async function runColocatedCall(config: T21Config): Promise<ScenarioResult> {
  const startTime = Date.now();
  const samples: ScenarioResult["samples"] = [];
  const metrics: Record<string, unknown> = {};
  const failures: string[] = [];

  // The SDK's mesh builds its connections and streams from these globals.
  (globalThis as any).RTCPeerConnection = class {
    constructor(configuration: RTCConfiguration = {}) {
      return specPeerConnection(configuration);
    }
  };
  (globalThis as any).MediaStream = wrtc.MediaStream;

  /** Run one assertion: a failure is recorded and the scenario goes on. */
  const check = async (name: string, body: () => Promise<void>): Promise<boolean> => {
    const timestamp = Date.now();
    try {
      await body();
      samples.push({ name, durationMs: Date.now() - timestamp, timestamp });
      return true;
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      failures.push(`${name}: ${message}`);
      samples.push({ name, durationMs: Date.now() - timestamp, timestamp, error: message });
      console.error(`[t21] FAILED ${name}: ${message}`);
      return false;
    }
  };

  // ── Co-located, then leave ──
  let colocated: Call[] = [];
  await check("colocated.join", async () => {
    const [alice, bob] = config.colocated.users;
    colocated.push(await joinWithAudio(alice, config.colocated.neighbourhoodUrl, `${config.room}-colocated`, 440));
    colocated.push(await joinWithAudio(bob, config.colocated.neighbourhoodUrl, `${config.room}-colocated`, 660));
  });
  if (colocated.length === 2) {
    const [alice, bob] = colocated;
    const rosterOk = await check("colocated.roster", async () => {
      metrics["colocated_roster_ms"] = await waitFor(() => rosterLists(colocated), COLOCATED_ROSTER_TIMEOUT_MS,
        () => `each co-located user's roster should list the other; got ${rosters(colocated)}`);
    });
    metrics["colocated_rosters"] = rosters(colocated);
    await check("colocated.topology", async () => {
      metrics["colocated_topology"] = colocated.map((c) => c.topology);
      // A NAT'd executor's SFU is not public, so default options go mesh:
      // the path whose roster comes from online statuses.
      if (!colocated.every((c) => c.topology === "mesh")) {
        throw new Error(`expected mesh, got ${JSON.stringify(metrics["colocated_topology"])}`);
      }
    });
    if (rosterOk) {
      await check("colocated.audio", async () => {
        try {
          metrics["colocated_audio_ms"] = await waitFor(() => hearsEveryone(colocated), COLOCATED_AUDIO_TIMEOUT_MS,
            () => `audio should flow both ways; frames ${JSON.stringify(framesOf(colocated))}`);
        } finally {
          metrics["colocated_frames"] = framesOf(colocated);
        }
      });
    } else {
      failures.push("colocated.audio: not measured, the co-located call never formed");
    }
    await check("colocated.sfuServesAllMembers", async () => {
      const serves = await alice.client.neighbourhood.sfuServesAllMembers(alice.perspectiveUuid);
      metrics["colocated_sfu_serves_all_members"] = serves;
      if (serves !== true) throw new Error(`expected true for two users on one executor, got ${serves}`);
    });
    if (rosterOk) {
      await check("colocated.leave", async () => {
        for (const call of colocated) stopSinks(call);
        await bob.session.leave();
        metrics["leave_ms"] = await waitFor(() => alice.left.has(bob.user.did), LEAVE_TIMEOUT_MS,
          () => `alice's session should report participant-left for bob; left=[${[...alice.left].join(",")}]`);
      });
    } else {
      failures.push("colocated.leave: not run, the co-located call never formed");
    }
  }
  await endCalls(colocated);
  colocated = [];

  // ── Mixed: two on node 1, one on node 2 ──
  const mixed: Call[] = [];
  await check("mixed.join", async () => {
    const tones = [440, 660, 880];
    for (const [i, user] of config.mixed.users.entries()) {
      mixed.push(await joinWithAudio(user, config.mixed.neighbourhoodUrl, `${config.room}-mixed`, tones[i]));
    }
  });
  if (mixed.length === 3) {
    const rosterOk = await check("mixed.roster", async () => {
      metrics["mixed_roster_ms"] = await waitFor(() => rosterLists(mixed), MIXED_ROSTER_TIMEOUT_MS,
        () => `every roster should list the other two; got ${rosters(mixed)}`);
    });
    metrics["mixed_rosters"] = rosters(mixed);
    metrics["mixed_topology"] = mixed.map((c) => c.topology);
    await check("mixed.sfuServesAllMembers", async () => {
      const serves = await mixed[0].client.neighbourhood.sfuServesAllMembers(mixed[0].perspectiveUuid);
      metrics["mixed_sfu_serves_all_members"] = serves;
      if (serves !== false) throw new Error(`expected false with a member on node 2, got ${serves}`);
    });
    if (rosterOk) {
      await check("mixed.audio", async () => {
        try {
          metrics["mixed_audio_ms"] = await waitFor(() => hearsEveryone(mixed), MIXED_AUDIO_TIMEOUT_MS,
            () => `audio should flow on every pair; frames ${JSON.stringify(framesOf(mixed))}`);
        } finally {
          metrics["mixed_frames"] = framesOf(mixed);
          metrics["mixed_mesh"] = meshState(mixed);
        }
      });
    } else {
      failures.push("mixed.audio: not measured, the mixed call never formed");
    }
  }
  await endCalls(mixed);

  const passed = failures.length === 0;
  metrics["failures"] = failures;
  const endTime = Date.now();
  return {
    scenario: "t21",
    branch: config.branch,
    startTime,
    endTime,
    durationMs: endTime - startTime,
    passed,
    metrics,
    samples,
    summary: passed
      ? `co-located pair found each other in ${metrics["colocated_roster_ms"]} ms with audio both ways, leave seen in ${metrics["leave_ms"]} ms; ` +
        `mixed trio in ${metrics["mixed_roster_ms"]} ms with audio on every pair`
      : `FAILED: ${failures.join("; ")} — ${JSON.stringify(metrics)}`,
  };
}
