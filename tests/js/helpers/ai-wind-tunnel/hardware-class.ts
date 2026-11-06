/**
 * Hardware classes, from the rules in ai-wind-tunnel/hardware-classes.json. Pure: the
 * executor's local AI setup ports this function to Rust and runs it against the same
 * fixtures (ai-wind-tunnel/hardware-class-fixtures.json).
 */

import type { ClassRule } from "./data.js";
import type { Fingerprint, MemoryFit } from "./types.js";

const GIB = 1024 ** 3;

/** What classification reads from a machine. */
export interface ClassInput {
  os: string;
  arch: string;
  memoryBytes: number;
  backends: string[];
  gpus: { vendor: string; vramBytes: number; unified: boolean }[];
}

export function classInput(fp: Fingerprint): ClassInput {
  return {
    os: fp.os,
    arch: fp.arch,
    memoryBytes: fp.memoryBytes,
    backends: fp.backends,
    gpus: fp.gpus.map((g) => ({ vendor: g.vendor, vramBytes: g.vramBytes, unified: g.unified })),
  };
}

function matches(rule: ClassRule, m: ClassInput): boolean {
  const r = rule.match;
  if (r.os !== undefined && r.os !== m.os) return false;
  if (r.arch !== undefined && r.arch !== m.arch) return false;
  if (r.backendAny !== undefined && !r.backendAny.some((b) => m.backends.includes(b))) return false;
  if (r.minMemoryGb !== undefined && m.memoryBytes / GIB < r.minMemoryGb) return false;
  if (r.gpuVendor !== undefined || r.unifiedMemory !== undefined || r.minVramGb !== undefined) {
    const gpus = m.gpus.filter(
      (g) =>
        (r.gpuVendor === undefined || g.vendor === r.gpuVendor) &&
        (r.unifiedMemory === undefined || g.unified === r.unifiedMemory) &&
        (r.minVramGb === undefined || g.vramBytes / GIB >= r.minVramGb),
    );
    if (gpus.length === 0) return false;
  }
  return true;
}

/** The class of the first rule that matches; throws when none does (the rules end in a catch-all). */
export function classify(m: ClassInput, rules: ClassRule[]): string {
  const rule = rules.find((r) => matches(r, m));
  if (!rule) throw new Error("no hardware class rule matches (the rules must end in a catch-all)");
  return rule.class;
}

/**
 * The classes to look in for a recommendation, nearest first: the class itself, the
 * smaller tiers of its family (largest first), then the cpu family at the same or a
 * smaller tier. What runs on less memory runs on more; a GPU class can always run what a
 * CPU class of its memory runs.
 */
export function fallbackClasses(cls: string, rules: ClassRule[]): string[] {
  const own = rules.find((r) => r.class === cls);
  if (!own) return [cls];
  const smaller = (family: string, tier: number) =>
    rules.filter((r) => r.family === family && r.tierGb <= tier).sort((a, b) => b.tierGb - a.tierGb).map((r) => r.class);
  const chain = [cls, ...smaller(own.family, own.tierGb).filter((c) => c !== cls)];
  if (own.family !== "cpu") {
    // A GPU class's tier is its VRAM for a discrete card and its RAM otherwise; either way
    // the machine has at least that much RAM to give a CPU runtime.
    chain.push(...smaller("cpu", own.tierGb));
  }
  return [...new Set(chain)];
}

/**
 * Whether a model of `modelBytes` fits: model memory <= min(GPU budget - 1 GiB, RAM - 4 GiB).
 * Without a GPU budget (no GPU backend) only RAM counts.
 */
export function memoryFit(modelBytes: number, ramBytes: number, gpuBudgetBytes: number | null): MemoryFit {
  const ramRoom = ramBytes - 4 * GIB;
  const budget = gpuBudgetBytes === null ? ramRoom : Math.min(gpuBudgetBytes - 1 * GIB, ramRoom);
  return { fits: modelBytes <= budget, modelBytes, budgetBytes: Math.max(0, budget), gpuBudgetBytes, ramBytes };
}

/** The fit of a cell that ran on `backend`: a CPU cell holds the model in RAM, so the GPU budget does not limit it. */
export function cellFit(modelBytes: number, ramBytes: number, gpuBudgetBytes: number | null, backend: string | undefined): MemoryFit {
  return memoryFit(modelBytes, ramBytes, backend === "cpu" ? null : gpuBudgetBytes);
}
