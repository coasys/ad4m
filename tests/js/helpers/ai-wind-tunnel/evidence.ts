/** Evidence files: where they go and whether one is well formed. Pure, but for the file reads at the bottom. */

import { existsSync, readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { EVIDENCE_DIR, repoRelative } from "./paths.js";
import { EVIDENCE_SCHEMA_VERSION, PROVIDERS, SERVICES, type Evidence } from "./types.js";

const STATUSES = ["pass", "fail", "unsupported", "skipped"];

/** `ai-wind-tunnel/evidence/<class>/<yyyy-mm-dd>-<fingerprint id>.json`, relative to the repo root. */
export function evidenceRelPath(hardwareClass: string, startedAt: string, fingerprintId: string): string {
  if (!/^[a-z0-9][a-z0-9.-]*$/.test(hardwareClass)) throw new Error(`hardware class '${hardwareClass}' is not a safe path segment`);
  return `ai-wind-tunnel/evidence/${hardwareClass}/${startedAt.slice(0, 10)}-${fingerprintId}.json`;
}

function isStat(v: any): boolean {
  return v && typeof v === "object" && ["n", "median", "p95", "min", "max", "mean"].every((k) => typeof v[k] === "number");
}

/** Every way `e` departs from schema v1; empty when it is valid. */
export function validateEvidence(e: any): string[] {
  const errors: string[] = [];
  const need = (cond: boolean, msg: string) => {
    if (!cond) errors.push(msg);
  };
  if (!e || typeof e !== "object") return ["evidence is not an object"];
  need(e.schemaVersion === EVIDENCE_SCHEMA_VERSION, `schemaVersion must be ${EVIDENCE_SCHEMA_VERSION}`);
  for (const k of ["runId", "startedAt", "finishedAt", "hardwareClass"]) need(typeof e[k] === "string" && e[k].length > 0, `${k} must be a non-empty string`);
  need(!Number.isNaN(Date.parse(e.startedAt)), "startedAt must be an ISO date");
  const fp = e.fingerprint;
  need(fp && typeof fp === "object", "fingerprint is required");
  if (fp) {
    need(["linux", "macos", "windows", "other"].includes(fp.os), "fingerprint.os is invalid");
    need(["x64", "arm64", "other"].includes(fp.arch), "fingerprint.arch is invalid");
    need(typeof fp.memoryBytes === "number" && fp.memoryBytes > 0, "fingerprint.memoryBytes must be positive");
    need(Array.isArray(fp.gpus), "fingerprint.gpus must be an array");
    need(Array.isArray(fp.backends) && fp.backends.includes("cpu"), "fingerprint.backends must include cpu");
    need(typeof fp.id === "string" && /^[0-9a-f]{8}$/.test(fp.id), "fingerprint.id must be 8 hex characters");
    need(fp.cpu && typeof fp.cpu.model === "string" && typeof fp.cpu.threads === "number", "fingerprint.cpu is incomplete");
  }
  need(e.executor && typeof e.executor.commit === "string", "executor.commit is required");
  need(e.options && typeof e.options.repeat === "number" && e.options.repeat >= 1, "options.repeat must be >= 1");
  need(Array.isArray(e.cells), "cells must be an array");
  (e.cells ?? []).forEach((c: any, i: number) => {
    const at = `cells[${i}]`;
    need(PROVIDERS.includes(c.provider), `${at}.provider '${c.provider}' is unknown`);
    need(SERVICES.includes(c.service), `${at}.service '${c.service}' is unknown`);
    need(STATUSES.includes(c.status), `${at}.status '${c.status}' is invalid`);
    need(Array.isArray(c.checks), `${at}.checks must be an array`);
    need(c.metrics && typeof c.metrics === "object", `${at}.metrics must be an object`);
    need(typeof c.durationMs === "number", `${at}.durationMs must be a number`);
    if (c.status === "unsupported" || c.status === "skipped" || c.status === "fail") {
      need(typeof c.reason === "string" && c.reason.length > 0, `${at} is ${c.status} without a reason`);
    }
    if (c.status === "pass") {
      need(c.checks.length > 0 && c.checks.every((k: any) => k.pass === true), `${at} passes with a failed or missing check`);
      need(c.model && typeof c.model.remoteModel === "string", `${at} passes without a model`);
    }
    for (const [k, v] of Object.entries(c.metrics ?? {})) {
      need(v === null || ["number", "string", "boolean"].includes(typeof v) || isStat(v), `${at}.metrics.${k} is neither a value nor a stat`);
    }
  });
  return errors;
}

export interface LoadedEvidence {
  /** Repo-relative path, as recommendations cite it. */
  path: string;
  evidence: Evidence;
}

/** Every evidence file under `dir`, validated; throws naming the first invalid file. */
export function loadAllEvidence(dir = EVIDENCE_DIR): LoadedEvidence[] {
  if (!existsSync(dir)) return [];
  const files: string[] = [];
  const walk = (d: string) => {
    for (const f of readdirSync(d).sort()) {
      const p = path.join(d, f);
      if (statSync(p).isDirectory()) walk(p);
      else if (f.endsWith(".json")) files.push(p);
    }
  };
  walk(dir);
  return files.map((f) => {
    const evidence = JSON.parse(readFileSync(f, "utf8"));
    const errors = validateEvidence(evidence);
    if (errors.length) throw new Error(`${repoRelative(f)} is not valid evidence:\n  ${errors.join("\n  ")}`);
    const rel = repoRelative(f);
    if (path.basename(path.dirname(f)) !== evidence.hardwareClass) {
      throw new Error(`${rel} records class '${evidence.hardwareClass}' but sits in another class's directory`);
    }
    return { path: rel, evidence };
  });
}
