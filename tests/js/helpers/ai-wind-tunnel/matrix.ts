/** Building the matrix of cells from the plan and the manifest. Pure. */

import type { Manifest, MatrixPlan } from "./data.js";
import type { CellModel, PlannedCell, ProviderId, Service } from "./types.js";

/**
 * The model a plan entry names. A `modelId` takes size, licence and memory from the
 * manifest, and its remote name is the id itself (the runtime serves the file under the
 * name the run gives it). Throws on a `modelId` the manifest lacks, so a typo in the plan
 * fails the run before anything starts.
 */
export function resolveModel(
  entry: { modelId?: string; remoteModel?: string; sizeBytes?: number; license?: string; params?: string; source?: string; voice?: string },
  manifest: Manifest,
): CellModel {
  if (entry.modelId) {
    const m = manifest.models[entry.modelId];
    if (!m) throw new Error(`matrix.json names modelId '${entry.modelId}', which the manifest does not list`);
    return {
      modelId: entry.modelId,
      remoteModel: entry.remoteModel ?? entry.modelId,
      // The download: the model file and the files beside it (a projector, a decoder).
      sizeBytes: m.sizeBytes + Object.values(m.files ?? {}).reduce((a, f) => a + f.sizeBytes, 0),
      memoryBytes: m.memoryBytes,
      license: m.license,
      params: m.params,
      source: m.url,
      ...(entry.voice ? { voice: entry.voice } : {}),
    };
  }
  if (!entry.remoteModel) throw new Error("a matrix.json model needs a modelId or a remoteModel");
  const model: CellModel = { remoteModel: entry.remoteModel };
  for (const k of ["sizeBytes", "license", "params", "source", "voice"] as const) {
    if (entry[k] !== undefined) (model as any)[k] = entry[k];
  }
  // Without a measured footprint, the download is the floor of what serving takes.
  if (model.sizeBytes !== undefined) model.memoryBytes = model.sizeBytes;
  return model;
}

/**
 * Every cell for the chosen providers and services, in run order (providers as given,
 * services in the plan's order). A service a provider cannot serve is one `unsupported`
 * cell; a service it can serve but the plan lists no model for yields no cell.
 */
export function buildMatrix(
  plan: MatrixPlan,
  manifest: Manifest,
  providers: ProviderId[],
  services: Service[],
): PlannedCell[] {
  const cells: PlannedCell[] = [];
  for (const provider of providers) {
    const p = plan.providers[provider];
    if (!p) throw new Error(`matrix.json has no provider '${provider}'`);
    for (const service of plan.services.filter((s) => services.includes(s))) {
      const reason = p.unsupported?.[service];
      if (reason) {
        cells.push({ provider, service, unsupported: reason });
        continue;
      }
      for (const entry of p.models[service] ?? []) {
        cells.push({ provider, service, model: resolveModel(entry, manifest) });
      }
    }
  }
  return cells;
}

/** `--providers a,b` / `--services x,y`: the listed ids, each checked against the known ones. */
export function parseList<T extends string>(value: string | undefined, known: readonly T[], label: string): T[] {
  if (!value) return [...known];
  const items = value.split(",").map((s) => s.trim()).filter(Boolean);
  const unknown = items.filter((i) => !(known as readonly string[]).includes(i));
  if (unknown.length) throw new Error(`unknown ${label}: ${unknown.join(", ")} (known: ${known.join(", ")})`);
  return items as T[];
}
