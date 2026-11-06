/**
 * The AI wind tunnel: every cell of providers x services, run for real through a fresh
 * executor, verified, measured, and written as one evidence file.
 *
 *   pnpm run ai-wind-tunnel -- --providers ollama,llama-server --services llm,embeddings --repeat 3
 *
 * Options:
 *   --providers a,b       ollama, llama-server, vllm, whisper-server, speaches, nemo-speech, sherpa-onnx (default: all)
 *   --services x,y        llm, completion, embeddings, stt, tts (default: all)
 *   --repeat N            repetitions per measurement (default 3)
 *   --concurrency 1,4     parallel streams for the LLM throughput (default 1,4)
 *   --llama-builds b,..   llama-server manifest builds or backends (default: the GPU ones)
 *   --models m,..         only these models (manifest modelId or provider model name)
 *   --attach              only use servers named by AI_WT_*_URL; start nothing
 *   --no-direct           skip the direct-to-provider calls (no executor overhead figure)
 *   --out FILE            evidence path (default ai-wind-tunnel/evidence/<class>/<date>-<id>.json)
 *   --no-write            print the evidence instead of writing it
 *   --dry-run             print the matrix and the machine's class, run nothing
 */

import { randomUUID } from "node:crypto";
import { mkdirSync, mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import type { ModelType } from "@coasys/ad4m";
import { completionCell, embeddingsCell, llmCell, sttCell, ttsCell, type CellEnv, type CellOutcome } from "./cells.js";
import { loadHardwareClasses, loadManifest, loadMatrixPlan } from "./data.js";
import { evidenceRelPath, validateEvidence } from "./evidence.js";
import { startExecutor, type WtExecutor } from "./executor.js";
import { collectFingerprint } from "./fingerprint.js";
import { classify, classInput, memoryFit } from "./hardware-class.js";
import { buildMatrix, parseList } from "./matrix.js";
import { REPO_ROOT } from "./paths.js";
import { MemorySampler, stopAllSpawned } from "./procs.js";
import { medianOf } from "./stats.js";
import { checkOffload } from "./verify.js";
import { ollama } from "./providers/ollama.js";
import { llamaServer } from "./providers/llama-server.js";
import { whisperServer } from "./providers/whisper-server.js";
import { nemoSpeech } from "./providers/nemo-speech.js";
import { sherpaOnnx } from "./providers/sherpa-onnx.js";
import { removeStartedContainers, speaches, vllm } from "./providers/docker.js";
import { SkipError, type Driver, type RunContext, type Served, type Session, type Variant } from "./providers/types.js";
import {
  EVIDENCE_SCHEMA_VERSION,
  PROVIDERS,
  SERVICES,
  type CellModel,
  type CellResult,
  type Evidence,
  type Metric,
  type PlannedCell,
  type ProviderId,
  type ProviderRun,
  type Service,
} from "./types.js";

const DRIVERS: Record<ProviderId, Driver> = { ollama, "llama-server": llamaServer, vllm, "whisper-server": whisperServer, speaches, "nemo-speech": nemoSpeech, "sherpa-onnx": sherpaOnnx };
const MODEL_TYPE: Record<Service, ModelType> = { llm: "LLM", completion: "LLM", embeddings: "EMBEDDING", stt: "TRANSCRIPTION", tts: "SPEECH" };
const CELL_FN: Record<Service, (env: CellEnv) => Promise<CellOutcome>> = { llm: llmCell, completion: completionCell, embeddings: embeddingsCell, stt: sttCell, tts: ttsCell };
const CELL_TIMEOUT_MS = Number(process.env.AI_WT_CELL_TIMEOUT_MS ?? 1_800_000);

function args(argv: string[]) {
  const get = (name: string) => {
    const i = argv.indexOf(`--${name}`);
    return i >= 0 ? argv[i + 1] : undefined;
  };
  const has = (name: string) => argv.includes(`--${name}`);
  return {
    providers: parseList(get("providers"), PROVIDERS, "providers"),
    services: parseList(get("services"), SERVICES, "services"),
    repeat: Math.max(1, Number(get("repeat") ?? 3)),
    concurrency: (get("concurrency") ?? "1,4").split(",").map(Number).filter((n) => n >= 1),
    llamaBuilds: get("llama-builds")?.split(","),
    models: get("models")?.split(","),
    attach: has("attach"),
    direct: !has("no-direct"),
    out: get("out"),
    write: !has("no-write"),
    dryRun: has("dry-run"),
  };
}

const slug = (s: string) => s.toLowerCase().replace(/[^a-z0-9.]+/g, "-").replace(/^-|-$/g, "");
const modelKey = (m?: CellModel) => m?.modelId ?? m?.remoteModel ?? "";

function withTimeout<T>(p: Promise<T>, ms: number, label: string): Promise<T> {
  let t: NodeJS.Timeout;
  return Promise.race([p, new Promise<T>((_, rej) => (t = setTimeout(() => rej(new Error(`${label} timed out after ${ms / 1000} s`)), ms)))]).finally(() => clearTimeout(t));
}

function table(cells: CellResult[]): string {
  const pick = (c: CellResult) => {
    const m = c.metrics;
    const f = (k: string, unit = "") => (medianOf(m[k]) !== null ? `${k}=${medianOf(m[k])}${unit}` : "");
    switch (c.service) {
      case "llm":
        return [f("ttftMs"), f("decodeTps"), f("prefillTps"), f("throughputTpsC4")].filter(Boolean).join(" ");
      case "completion":
        return f("latencyMs");
      case "embeddings":
        return [f("textsPerSec"), f("dimension")].filter(Boolean).join(" ");
      case "stt":
        return [f("werMean"), f("rtf")].filter(Boolean).join(" ");
      case "tts":
        return [f("rtf"), f("ttfbMs"), f("roundTripWer")].filter(Boolean).join(" ");
    }
  };
  return cells
    .map((c) => `${c.status.padEnd(11)} ${c.provider.padEnd(14)} ${(c.runtimeBuild ?? c.backend ?? "").padEnd(17)} ${c.service.padEnd(10)} ${modelKey(c.model).padEnd(38)} ${c.status === "pass" ? pick(c) : (c.reason ?? "").slice(0, 140)}`)
    .join("\n");
}

/** Providers first, then the executor; each at most once, also when a signal ends the run. */
const cleanups: (() => Promise<unknown>)[] = [];
async function cleanup(): Promise<void> {
  while (cleanups.length) await cleanups.shift()!().catch(() => {});
  await stopAllSpawned();
  await removeStartedContainers();
}
for (const sig of ["SIGINT", "SIGTERM"] as const) {
  process.once(sig, () => {
    console.error(`\n${sig}: stopping the servers and containers this run started`);
    cleanup().finally(() => process.exit(130));
  });
}

async function main() {
  const opts = args(process.argv.slice(2));
  const manifest = loadManifest();
  const plan = loadMatrixPlan();
  const classes = loadHardwareClasses();
  const fp = collectFingerprint();
  const hardwareClass = classify(classInput(fp), classes.rules);
  const planned = buildMatrix(plan, manifest, opts.providers, opts.services).filter(
    (c) => !opts.models || !c.model || opts.models.includes(modelKey(c.model)) || opts.models.includes(c.model.remoteModel),
  );
  const runId = randomUUID().slice(0, 8);
  const startedAt = new Date().toISOString();
  const runDir = mkdtempSync(path.join(os.tmpdir(), `ai-wt-run-${runId}-`));
  const log = (line: string) => console.log(`[ai-wt ${new Date().toISOString().slice(11, 19)}] ${line}`);
  log(`machine ${fp.id}: ${fp.cpu.model}, ${(fp.memoryBytes / 2 ** 30).toFixed(0)} GiB, ${fp.gpus.map((g) => g.name).join("; ") || "no GPU"}; backends ${fp.backends.join(",")}; class ${hardwareClass}`);
  log(`${planned.length} cells; logs in ${runDir}`);
  if (opts.dryRun) {
    for (const c of planned) console.log(`${c.provider.padEnd(14)} ${c.service.padEnd(10)} ${c.unsupported ? `unsupported: ${c.unsupported}` : modelKey(c.model)}`);
    return;
  }

  const ctx: RunContext = { runId, runDir, fp, manifest, plan, attach: opts.attach, llamaBuilds: opts.llamaBuilds, log };
  const results: CellResult[] = [];
  const providerRuns: Evidence["providers"] = {};
  const sttPassed: { modelName: string; label: string; wer: number }[] = [];
  let ex: WtExecutor | null = null;

  const record = (c: PlannedCell, status: CellResult["status"], reason: string, variant?: Variant, endpointKind?: CellResult["endpointKind"]) =>
    results.push({
      provider: c.provider,
      service: c.service,
      status,
      reason,
      ...(c.model ? { model: c.model } : {}),
      ...(variant?.runtimeBuild ? { runtimeBuild: variant.runtimeBuild } : {}),
      ...(variant?.backend ? { backend: variant.backend } : {}),
      ...(endpointKind ? { endpointKind } : {}),
      checks: [],
      metrics: {},
      durationMs: 0,
    });

  try {
    const runnable = planned.filter((c) => !c.unsupported);
    if (runnable.length) {
      ex = await startExecutor(log);
      const started = ex;
      cleanups.push(() => started.stop());
    }

    for (const provider of opts.providers) {
      const cells = planned.filter((c) => c.provider === provider);
      for (const c of cells.filter((c) => c.unsupported)) record(c, "unsupported", c.unsupported!);
      const todo = cells.filter((c) => !c.unsupported);
      if (!todo.length) continue;
      const p = await DRIVERS[provider].plan(ctx);
      if ("skip" in p) {
        log(`${provider}: skipped: ${p.skip}`);
        providerRuns[provider] = { status: "skipped", reason: p.skip };
        for (const c of todo) record(c, "skipped", p.skip);
        continue;
      }
      const run: ProviderRun = { status: "available", builds: p.variants.map((v) => v.runtimeBuild).filter((b): b is string => !!b) };
      providerRuns[provider] = run;

      for (const variant of p.variants) {
        let session: Session;
        try {
          session = await DRIVERS[provider].open(ctx, variant);
        } catch (e: any) {
          const skip = e instanceof SkipError;
          log(`${provider} ${variant.label}: ${skip ? "skipped" : "failed to start"}: ${e.message}`);
          for (const c of todo) record(c, skip ? "skipped" : "fail", `${provider} did not start: ${e.message}`, variant);
          continue;
        }
        const opened = session;
        cleanups.unshift(() => opened.close().catch((e) => log(`closing ${provider} failed: ${e.message}`)));
        Object.assign(run, { mode: session.mode, version: session.version ?? run.version, ...(session.image ? { image: session.image } : {}) });
        const kind = session.kind;

        // One model at a time, every service it serves while it is loaded.
        const byModel = new Map<string, PlannedCell[]>();
        for (const c of todo) byModel.set(modelKey(c.model), [...(byModel.get(modelKey(c.model)) ?? []), c]);
        for (const group of byModel.values()) {
          const model = group[0].model!;
          let served: Served;
          let pullMs: number | null = null;
          const t0 = Date.now();
          try {
            served = await session.serve(model, group[0].service);
          } catch (e: any) {
            const skip = e instanceof SkipError;
            log(`${provider} ${variant.label} ${modelKey(model)}: ${e.message}`);
            for (const c of group) record(c, skip ? "skipped" : "fail", `serving the model failed: ${e.message}`, variant, kind);
            continue;
          }
          const endpointName = `wt-${provider}-${slug(variant.label)}-${served.baseUrl.split(":").pop()}`;
          let endpointId: string;
          let endpointHealth: string | undefined;
          try {
            const ep = await ex!.endpoint(endpointName, served.kind ?? kind, served.baseUrl);
            endpointId = ep.id;
            endpointHealth = ep.health?.state;
            if (session.pullsThroughExecutor) {
              log(`${provider}: pulling ${served.remoteModel} through the executor`);
              const tp = Date.now();
              const progress: number[] = [];
              const off = ex!.client.on("model-status", (ev: any) => {
                if (ev.endpointId === endpointId && ev.remoteModel === served.remoteModel && typeof ev.status?.progress === "number") progress.push(ev.status.progress);
              });
              try {
                await ex!.client.ai.pullModel(endpointId, served.remoteModel, { timeoutMs: 3_600_000 });
              } finally {
                off();
              }
              pullMs = Date.now() - tp;
              log(`${provider}: pulled ${served.remoteModel} in ${(pullMs / 1000).toFixed(1)} s (${progress.length} progress events)`);
            }
          } catch (e: any) {
            log(`${provider} ${modelKey(model)}: registering failed: ${e.message}`);
            for (const c of group) record(c, "fail", `registering with the executor failed: ${e.message}`, variant, served.kind ?? kind);
            await served.release().catch(() => {});
            continue;
          }

          for (const c of group) {
            const started = Date.now();
            const sampler = new MemorySampler(served.pids).start();
            let outcome: CellOutcome;
            let error: string | undefined;
            try {
              const name = `wt-${provider}-${slug(variant.label)}-${slug(served.remoteModel)}-${MODEL_TYPE[c.service].toLowerCase()}`;
              const modelId = await ex!.model(name, MODEL_TYPE[c.service], endpointId, served.remoteModel);
              const best = [...sttPassed].sort((a, b) => a.wer - b.wer)[0];
              const env: CellEnv = {
                ex: ex!,
                modelName: name,
                modelId,
                remoteModel: served.remoteModel,
                kind: served.kind ?? kind,
                directV1: opts.direct ? `${served.baseUrl}/v1` : null,
                repeat: opts.repeat,
                concurrency: opts.concurrency,
                voice: model.voice,
                sttReference: best ? { modelName: best.modelName, label: best.label } : undefined,
                log,
              };
              log(`${provider} ${variant.label} ${c.service} ${modelKey(model)}: running`);
              outcome = await withTimeout(CELL_FN[c.service](env), CELL_TIMEOUT_MS, `${c.service} cell`);
              if (c.service === "stt" && outcome.checks.length && outcome.checks.every((k) => k.pass)) {
                sttPassed.push({ modelName: name, label: `${provider}/${served.remoteModel}`, wer: typeof outcome.metrics.werMean === "number" ? outcome.metrics.werMean : 1 });
              }
            } catch (e: any) {
              error = String(e?.message ?? e);
              outcome = { checks: [], metrics: {} };
            }
            const peak = sampler.stop();
            const metrics: Record<string, Metric> = { ...outcome.metrics, peakRssBytes: peak.peakRssBytes, peakGpuBytes: peak.peakGpuBytes };
            if (served.readyMs !== undefined) metrics.serverReadyMs = served.readyMs;
            if (served.adapter) metrics.adapter = served.adapter;
            if (pullMs !== null) metrics.pullMs = pullMs;
            // How the executor saw the endpoint when it was added (a server that lists no models is not REACHABLE).
            if (endpointHealth) metrics.endpointHealth = endpointHealth;
            const offload = checkOffload(served.backend?.() ?? variant.backend, peak.peakGpuBytes, model.sizeBytes);
            if (offload && !error) outcome.checks.push(offload);
            const failed = outcome.checks.filter((k) => !k.pass);
            const status = error || failed.length || outcome.checks.length === 0 ? "fail" : "pass";
            const modelBytes = model.memoryBytes ?? model.sizeBytes;
            const result: CellResult = {
              provider,
              service: c.service,
              status,
              ...(status === "fail" ? { reason: error ?? failed.map((k) => `${k.name}: ${k.detail}`).join("; ").slice(0, 1000) } : {}),
              model,
              ...(variant.runtimeBuild ? { runtimeBuild: variant.runtimeBuild } : {}),
              backend: served.backend?.() ?? variant.backend,
              endpointKind: served.kind ?? kind,
              ...(served.flags ? { serverFlags: served.flags } : {}),
              checks: outcome.checks,
              metrics,
              ...(modelBytes ? { fit: memoryFit(modelBytes, fp.memoryBytes, fp.gpuBudgetBytes) } : {}),
              durationMs: Date.now() - started + (c === group[0] ? started - t0 : 0),
            };
            results.push(result);
            log(`${provider} ${variant.label} ${c.service} ${modelKey(model)}: ${status}${result.reason ? ` (${result.reason.slice(0, 300)})` : ""}`);
          }
          await served.release().catch(() => {});
          // vLLM knows its version once a model serves.
          if (session.version) run.version = session.version;
        }
      }
    }
  } finally {
    await cleanup();
  }

  const evidence: Evidence = {
    schemaVersion: EVIDENCE_SCHEMA_VERSION,
    runId,
    startedAt,
    finishedAt: new Date().toISOString(),
    hardwareClass,
    fingerprint: fp,
    executor: {
      commit: ex?.commit ?? "not started",
      binary: path.basename(ex?.binary ?? "ad4m-executor"),
      ...(ex?.version ? { version: ex.version } : {}),
      ...(ex ? { harnessCommit: ex.harnessCommit } : {}),
    },
    options: { repeat: opts.repeat, concurrency: opts.concurrency, providers: opts.providers, services: opts.services, direct: opts.direct },
    providers: providerRuns,
    cells: results,
  };
  const errors = validateEvidence(evidence);
  console.log(`\n${table(results)}\n`);
  if (errors.length) {
    console.error(`the evidence does not validate:\n  ${errors.join("\n  ")}`);
    process.exitCode = 2;
  }
  const json = JSON.stringify(evidence, null, 2) + "\n";
  if (!opts.write) {
    console.log(json);
    return;
  }
  const out = opts.out ? path.resolve(opts.out) : path.join(REPO_ROOT, evidenceRelPath(hardwareClass, startedAt, fp.id));
  mkdirSync(path.dirname(out), { recursive: true });
  writeFileSync(out, json);
  log(`evidence: ${path.relative(REPO_ROOT, out)}; regenerate recommendations with pnpm run ai-wind-tunnel-recommend`);
}

main()
  .then(() => process.exit(process.exitCode ?? 0))
  .catch((e) => {
    console.error(e);
    process.exit(1);
  });
