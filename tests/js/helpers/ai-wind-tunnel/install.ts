/**
 * Runtimes and model files from the manifest, downloaded into the cache and checked
 * against the manifest's sha256, as the executor's local AI setup will install them.
 */

import { execFile } from "node:child_process";
import { createHash } from "node:crypto";
import { createReadStream, createWriteStream, existsSync, mkdirSync, readFileSync, renameSync, statSync, writeFileSync } from "node:fs";
import path from "node:path";
import { Readable } from "node:stream";
import { pipeline } from "node:stream/promises";
import { promisify } from "node:util";
import type { Manifest, RuntimeArchive } from "./data.js";
import { cacheDir } from "./paths.js";

const execFileP = promisify(execFile);

async function sha256File(file: string): Promise<string> {
  const h = createHash("sha256");
  await pipeline(createReadStream(file), h);
  return h.digest("hex");
}

/**
 * `url` into `file`: with curl where there is one (Node's fetch reads a multi-gigabyte body
 * an order of magnitude slower), else with fetch.
 */
async function download(url: string, file: string): Promise<void> {
  try {
    await execFileP("curl", ["-fsSL", "--retry", "3", "-o", file, url], { timeout: 4 * 3600_000 });
    return;
  } catch (e: any) {
    if (e?.code !== "ENOENT") throw new Error(`download of ${url} failed: ${e?.stderr || e?.message}`);
  }
  const res = await fetch(url, { redirect: "follow" });
  if (!res.ok || !res.body) throw new Error(`download of ${url} failed: HTTP ${res.status}`);
  await pipeline(Readable.fromWeb(res.body as any), createWriteStream(file));
}

/**
 * `dest`, downloaded from `src.url` unless a verified copy is there. A file whose size and
 * hash differ from the manifest is refused; a verified file gets a `.sha256` marker so the
 * next run does not hash it again.
 */
export async function fetchVerified(src: RuntimeArchive, dest: string, log: (s: string) => void): Promise<string> {
  const marker = `${dest}.sha256`;
  if (existsSync(dest) && statSync(dest).size === src.sizeBytes) {
    if (existsSync(marker) && readFileSync(marker, "utf8").trim() === src.sha256) return dest;
    if ((await sha256File(dest)) === src.sha256) {
      writeFileSync(marker, src.sha256);
      return dest;
    }
  }
  mkdirSync(path.dirname(dest), { recursive: true });
  log(`downloading ${src.url} (${(src.sizeBytes / 1e6).toFixed(0)} MB)`);
  const part = `${dest}.part`;
  await download(src.url, part);
  const got = await sha256File(part);
  const size = statSync(part).size;
  if (got !== src.sha256 || size !== src.sizeBytes) {
    throw new Error(`${src.url}: sha256 ${got} / ${size} bytes, the manifest says ${src.sha256} / ${src.sizeBytes}`);
  }
  renameSync(part, dest);
  writeFileSync(marker, src.sha256);
  return dest;
}

async function extract(archive: string, into: string): Promise<void> {
  mkdirSync(into, { recursive: true });
  if (archive.endsWith(".zip")) {
    // bsdtar (macOS, Windows 10+) reads zip; Linux has unzip.
    if (process.platform === "linux") await execFileP("unzip", ["-o", "-q", archive, "-d", into]);
    else await execFileP("tar", ["-xf", archive, "-C", into]);
  } else {
    await execFileP("tar", ["-xzf", archive, "-C", into]);
  }
}

/** The server binary of runtime `id`, build `build` (`linux-x64-rocm`), installed into the cache. */
export async function installRuntime(manifest: Manifest, id: string, build: string, log: (s: string) => void): Promise<{ binary: string; version: string }> {
  const rt = manifest.runtimes[id];
  if (!rt) throw new Error(`the manifest has no runtime '${id}'`);
  const b = rt.builds[build];
  if (!b) throw new Error(`runtime '${id}' has no build '${build}' (has: ${Object.keys(rt.builds).join(", ")})`);
  const root = path.join(cacheDir(), "runtimes", id, rt.version, build);
  const binary = path.join(root, "x", b.binary);
  const done = path.join(root, "installed");
  if (!existsSync(done) || !existsSync(binary)) {
    for (const a of [b, ...(b.extras ?? [])]) {
      const file = await fetchVerified(a, path.join(root, path.basename(new URL(a.url).pathname)), log);
      await extract(file, path.join(root, "x"));
    }
    if (!existsSync(binary)) throw new Error(`${b.url} has no ${b.binary}`);
    writeFileSync(done, new Date().toISOString());
  }
  return { binary, version: rt.version };
}

/** The file of manifest model `modelId`, downloaded into the cache. */
export async function installModel(manifest: Manifest, modelId: string, log: (s: string) => void): Promise<string> {
  const m = manifest.models[modelId];
  if (!m) throw new Error(`the manifest has no model '${modelId}'`);
  return fetchVerified(m, path.join(cacheDir(), "models", path.basename(new URL(m.url).pathname)), log);
}
