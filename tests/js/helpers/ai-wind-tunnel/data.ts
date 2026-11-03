/** The checked-in data the wind tunnel reads: the runtime/model manifest, the matrix plan and the hardware classes. */

import { readFileSync } from "node:fs";
import path from "node:path";
import { DATA_DIR, MANIFEST_PATH } from "./paths.js";
import type { EndpointKindId, ProviderId, Service } from "./types.js";

export interface RuntimeArchive {
  url: string;
  sha256: string;
  sizeBytes: number;
}

export interface RuntimeBuild extends RuntimeArchive {
  /** Path of the server binary inside the archive. */
  binary: string;
  /** Archives unpacked beside the build (CUDA runtime libraries). */
  extras?: RuntimeArchive[];
}

export interface Runtime {
  provider: "llama-server" | "whisper-server";
  version: string;
  license?: string;
  source?: string;
  /** By `<os>-<arch>-<backend>`: linux-x64-rocm, macos-arm64-metal, windows-x64-cuda... */
  builds: Record<string, RuntimeBuild>;
}

export interface ManifestModel {
  service: "llm" | "embeddings" | "stt" | "tts";
  provider: string;
  url: string;
  sha256: string;
  sizeBytes: number;
  license: string;
  params?: string;
  /** Resident memory when served (weights, context, buffers). */
  memoryBytes: number;
}

export interface Manifest {
  schemaVersion: 1;
  runtimes: Record<string, Runtime>;
  models: Record<string, ManifestModel>;
}

export interface PlanModel {
  modelId?: string;
  remoteModel?: string;
  sizeBytes?: number;
  license?: string;
  params?: string;
  source?: string;
  voice?: string;
}

export interface MatrixPlan {
  schemaVersion: 1;
  services: Service[];
  providers: Record<
    ProviderId,
    {
      endpointKind: EndpointKindId;
      image?: string;
      models: Partial<Record<Service, PlanModel[]>>;
      unsupported?: Partial<Record<Service, string>>;
    }
  >;
}

export interface ClassRule {
  class: string;
  family: string;
  tierGb: number;
  match: {
    os?: string;
    arch?: string;
    gpuVendor?: string;
    unifiedMemory?: boolean;
    backendAny?: string[];
    minMemoryGb?: number;
    minVramGb?: number;
  };
}

export interface HardwareClasses {
  schemaVersion: 1;
  rules: ClassRule[];
}

function readJson<T>(file: string): T {
  return JSON.parse(readFileSync(file, "utf8")) as T;
}

export const loadManifest = (): Manifest => readJson(MANIFEST_PATH);
export const loadMatrixPlan = (): MatrixPlan => readJson(path.join(DATA_DIR, "matrix.json"));
export const loadHardwareClasses = (): HardwareClasses => readJson(path.join(DATA_DIR, "hardware-classes.json"));
