/**
 * Providers that run in containers the run starts and removes: speaches (OpenAI-shaped
 * STT and TTS) and vLLM (NVIDIA through the official image, ROCm through
 * AI_WT_VLLM_ROCM_IMAGE). Model weights live in the cache's Hugging Face directory, so a
 * rerun does not download them again. Only containers named `ai-wt-<run id>-...` are removed.
 */

import { existsSync, mkdirSync, statSync, writeFileSync } from "node:fs";
import path from "node:path";
import { cacheDir } from "../paths.js";
import { assertNotReserved, docker, dockerCli, freePort, sleep, waitForHttp } from "../procs.js";
import { envUrl, SkipError, type Driver, type RunContext, type Session } from "./types.js";

async function imagePresent(image: string): Promise<boolean> {
  try {
    await docker(["image", "inspect", image, "--format", "{{.Id}}"]);
    return true;
  } catch {
    return false;
  }
}

async function containerPid(name: string): Promise<number | null> {
  try {
    const pid = Number(await docker(["inspect", "-f", "{{.State.Pid}}", name]));
    return pid > 0 ? pid : null;
  } catch {
    return null;
  }
}

/** Containers this run started and has not removed yet: a start that fails or a signal removes them too. */
const startedContainers = new Set<string>();

async function removeContainer(name: string, ctx: RunContext): Promise<void> {
  try {
    await docker(["rm", "-f", name], 60_000);
    startedContainers.delete(name);
    ctx.log(`removed container ${name}`);
  } catch {}
}

/** Remove every container this run started that is still there. */
export async function removeStartedContainers(): Promise<void> {
  for (const name of [...startedContainers]) await docker(["rm", "-f", name], 60_000).catch(() => {});
  startedContainers.clear();
}

async function saveLogs(name: string, ctx: RunContext): Promise<void> {
  try {
    writeFileSync(path.join(ctx.runDir, `${name}.log`), await docker(["logs", name], 60_000, true).catch((e) => String(e?.stderr ?? e)));
  } catch {}
}

function hfCache(): string {
  const dir = path.join(cacheDir(), "hf");
  mkdirSync(dir, { recursive: true });
  return dir;
}

/**
 * A hard memory cap for every container the run starts: AI_WT_CONTAINER_MEMORY (a docker
 * size such as `8g`) becomes `--memory` and `--memory-swap` alike, so the container can
 * neither grow past it nor swap. Unset: no cap.
 */
export function containerMemoryArgs(cap = process.env.AI_WT_CONTAINER_MEMORY): string[] {
  if (!cap) return [];
  if (!/^\d+(\.\d+)?[bkmg]?$/i.test(cap)) throw new Error(`AI_WT_CONTAINER_MEMORY '${cap}' is not a docker memory size (e.g. 8g)`);
  return ["--memory", cap, "--memory-swap", cap];
}

export const speaches: Driver = {
  id: "speaches",
  async plan(ctx) {
    if (envUrl("AI_WT_SPEACHES_URL")) return { variants: [{ label: "attach" }] };
    if (ctx.attach) return { skip: "--attach without AI_WT_SPEACHES_URL" };
    if (!(await dockerCli())) return { skip: "docker is not available (nor AI_WT_SPEACHES_URL)" };
    const cuda = ctx.fp.backends.includes("cuda");
    return { variants: [{ label: cuda ? "cuda" : "cpu", backend: cuda ? "cuda" : "cpu" }] };
  },
  async open(ctx, variant): Promise<Session> {
    const attached = envUrl("AI_WT_SPEACHES_URL");
    let baseUrl: string;
    let name: string | null = null;
    let image: string | undefined;
    if (attached) {
      assertNotReserved(attached);
      baseUrl = attached;
    } else {
      image = process.env.AI_WT_SPEACHES_IMAGE ?? (variant.backend === "cuda" ? "ghcr.io/speaches-ai/speaches:latest-cuda" : ctx.plan.providers.speaches.image ?? "ghcr.io/speaches-ai/speaches:latest-cpu");
      if (!(await imagePresent(image))) {
        ctx.log(`pulling ${image}`);
        await docker(["pull", image], 1_800_000);
      }
      const port = await freePort();
      name = `ai-wt-${ctx.runId}-speaches`;
      const args = ["run", "-d", "--name", name, "-p", `127.0.0.1:${port}:8000`, "-v", `${hfCache()}:/home/ubuntu/.cache/huggingface/hub`, ...containerMemoryArgs()];
      if (variant.backend === "cuda") args.push("--gpus", "all");
      await docker([...args, image]);
      startedContainers.add(name);
      baseUrl = `http://127.0.0.1:${port}`;
      try {
        await waitForHttp(`${baseUrl}/health`, { timeoutMs: 180_000, label: "speaches" });
      } catch (e) {
        await saveLogs(name, ctx);
        await removeContainer(name, ctx);
        throw e;
      }
    }
    const version = image ? await docker(["image", "inspect", image, "--format", "{{.Created}}"]).then((c) => `image created ${c.slice(0, 10)}`).catch(() => undefined) : undefined;
    ctx.log(`speaches at ${baseUrl}`);
    return {
      kind: "OPEN_AI_AUDIO",
      mode: attached ? "attach" : "docker",
      version,
      image,
      async serve(model) {
        // speaches serves a model once it is downloaded into its cache.
        const listed: any = await fetch(`${baseUrl}/v1/models`).then((r) => r.json()).catch(() => ({}));
        if (!(listed?.data ?? []).some((m: any) => m.id === model.remoteModel)) {
          ctx.log(`speaches: downloading ${model.remoteModel}`);
          const res = await fetch(`${baseUrl}/v1/models/${model.remoteModel}`, { method: "POST" });
          if (!res.ok && res.status !== 409) throw new Error(`speaches could not download ${model.remoteModel}: HTTP ${res.status} ${await res.text()}`);
        }
        const pid = name ? await containerPid(name) : null;
        return {
          baseUrl,
          remoteModel: model.remoteModel,
          pids: () => (pid ? [pid] : []),
          backend: () => variant.backend,
          // Unloaded on release, it loads the model again when a later request names it.
          staysUp: true,
          async release() {
            // Unload from the container this run started, so the next model's memory peak is its own.
            if (name) await fetch(`${baseUrl}/api/ps/${model.remoteModel}`, { method: "DELETE" }).catch(() => {});
          },
        };
      },
      async close() {
        if (name) {
          await saveLogs(name, ctx);
          await removeContainer(name, ctx);
        }
      },
    };
  },
};

/** The ROCm vLLM image for this machine: AI_WT_VLLM_ROCM_IMAGE, else none (no official image targets consumer and APU gfx parts). */
function rocmImage(): string | undefined {
  return process.env.AI_WT_VLLM_ROCM_IMAGE;
}

export const vllm: Driver = {
  id: "vllm",
  async plan(ctx) {
    if (envUrl("AI_WT_VLLM_URL")) return { variants: [{ label: "attach" }] };
    if (ctx.attach) return { skip: "--attach without AI_WT_VLLM_URL" };
    const cuda = ctx.fp.backends.includes("cuda");
    const rocm = ctx.fp.backends.includes("rocm");
    if (!cuda && !rocm) return { skip: "vLLM needs an NVIDIA (CUDA) or AMD (ROCm) GPU; this machine has neither" };
    if (!(await dockerCli())) return { skip: "vLLM runs from its docker image and docker is not available" };
    if (!cuda && !rocmImage()) {
      return { skip: "no ROCm vLLM image for this GPU: the official rocm/vllm images target Instinct (gfx9xx) parts; set AI_WT_VLLM_ROCM_IMAGE to an image built for it" };
    }
    return { variants: [{ label: cuda ? "cuda" : "rocm", backend: cuda ? "cuda" : "rocm" }] };
  },
  async open(ctx, variant): Promise<Session> {
    const attached = envUrl("AI_WT_VLLM_URL");
    if (attached) {
      assertNotReserved(attached);
      return {
        kind: "VLLM",
        mode: "attach",
        async serve(model) {
          return { baseUrl: attached, remoteModel: model.remoteModel, pids: () => [], async release() {} };
        },
        async close() {},
      };
    }
    const image = variant.backend === "cuda" ? process.env.AI_WT_VLLM_IMAGE ?? "vllm/vllm-openai:latest" : rocmImage()!;
    if (!(await imagePresent(image))) {
      ctx.log(`pulling ${image}`);
      await docker(["pull", image], 3_600_000);
    }
    const containers = new Map<string, { name: string; baseUrl: string; readyMs: number; pid: number | null; flags: string[] }>();
    let n = 0;
    let version: string | undefined;
    const session: Session = {
      kind: "VLLM",
      mode: "docker",
      image,
      async serve(model) {
        let c = containers.get(model.remoteModel);
        if (!c) {
          const port = await freePort();
          const name = `ai-wt-${ctx.runId}-vllm-${n++}`;
          // As this user, so the weights it downloads into the cache stay this user's files.
          // The image has no passwd entry for the uid: USER and LOGNAME stand in for it.
          const user =
            typeof process.getuid === "function"
              ? ["--user", `${process.getuid()}:${process.getgid!()}`, "-e", "HOME=/tmp", "-e", "USER=vllm", "-e", "LOGNAME=vllm", "-e", "TORCHINDUCTOR_CACHE_DIR=/tmp/inductor"]
              : [];
          const args = ["run", "-d", "--name", name, "-p", `127.0.0.1:${port}:8000`, "-v", `${hfCache()}:/hf`, "-e", "HF_HUB_CACHE=/hf", "--ipc=host", ...containerMemoryArgs(), ...user];
          if (variant.backend === "cuda") args.push("--gpus", "all", image);
          else {
            args.push("--device", "/dev/kfd", "--device", "/dev/dri", "--security-opt", "seccomp=unconfined");
            const gids = new Set(["/dev/kfd", "/dev/dri/renderD128"].filter(existsSync).map((d) => String(statSync(d).gid)));
            for (const g of gids) args.push("--group-add", g);
            args.push("--entrypoint", "vllm", image);
          }
          const flags = ["--max-model-len", "8192", "--gpu-memory-utilization", process.env.AI_WT_VLLM_GPU_UTIL ?? "0.15"];
          const serveArgs = variant.backend === "cuda" ? [] : ["serve"];
          serveArgs.push(model.remoteModel, "--host", "0.0.0.0", "--port", "8000", "--served-model-name", model.remoteModel, ...flags);
          const started = Date.now();
          ctx.log(`vLLM: starting ${name} (${image}) for ${model.remoteModel}`);
          await docker([...args, ...serveArgs]);
          startedContainers.add(name);
          const baseUrl = `http://127.0.0.1:${port}`;
          // The container's state, polled: a container that exits ends the wait.
          let running = true;
          const watch = setInterval(() => {
            docker(["inspect", "-f", "{{.State.Running}}", name]).then((s) => (running = s === "true"), () => (running = false));
          }, 3000);
          try {
            await waitForHttp(`${baseUrl}/health`, {
              timeoutMs: 1_200_000,
              label: `vLLM ${model.remoteModel}`,
              alive: () => running,
            });
          } catch (e: any) {
            await saveLogs(name, ctx);
            const state = await docker(["inspect", "-f", "{{.State.Status}} exit={{.State.ExitCode}}", name]).catch(() => "gone");
            await removeContainer(name, ctx);
            // A GPU stack that does not start here is this machine's limit, not a failed cell.
            throw new SkipError(`vLLM (${image}) did not start ${model.remoteModel} on ${variant.backend}: ${e.message}; container ${state}; log in ${name}.log`);
          } finally {
            clearInterval(watch);
          }
          version ??= await fetch(`${baseUrl}/version`).then((r) => r.json()).then((j: any) => j.version).catch(() => undefined);
          session.version = version;
          c = { name, baseUrl, readyMs: Date.now() - started, pid: await containerPid(name), flags };
          containers.set(model.remoteModel, c);
          await sleep(500);
        }
        const cc = c;
        return { baseUrl: cc.baseUrl, remoteModel: model.remoteModel, readyMs: cc.readyMs, pids: () => (cc.pid ? [cc.pid] : []), backend: () => variant.backend, flags: cc.flags, async release() {} };
      },
      async close() {
        for (const c of containers.values()) {
          await saveLogs(c.name, ctx);
          await removeContainer(c.name, ctx);
        }
      },
    };
    return session;
  },
};
