import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

export const TESTS_JS = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");
export const REPO_ROOT = path.resolve(TESTS_JS, "..", "..");
/** The wind tunnel's checked-in data: matrix, hardware classes, evidence, recommendations. */
export const DATA_DIR = path.join(REPO_ROOT, "ai-wind-tunnel");
export const EVIDENCE_DIR = path.join(DATA_DIR, "evidence");
export const FIXTURES_DIR = path.join(DATA_DIR, "fixtures");
export const MANIFEST_PATH = path.join(REPO_ROOT, "rust-executor", "src", "ai_service", "local", "manifest.json");
export const BOOTSTRAP_SEED = path.join(TESTS_JS, "bootstrapSeed.json");

/** Downloads (runtimes, GGUF files, Ollama and speaches models), kept between runs. */
export function cacheDir(): string {
  return process.env.AI_WT_CACHE || path.join(os.homedir(), ".cache", "ad4m-ai-wind-tunnel");
}

/** Repo-relative path with forward slashes, as evidence links are written. */
export function repoRelative(p: string): string {
  return path.relative(REPO_ROOT, p).split(path.sep).join("/");
}
