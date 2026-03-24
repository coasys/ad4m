// lib/types.ts — Shared types for the AD4M test harness

/** Result returned by every action */
export interface ActionResult {
  ok: boolean;
  data?: Record<string, unknown>;
  error?: string;
  duration_ms: number;
}

/** Parameter definition for action metadata */
export interface ParamDef {
  type: string;
  description: string;
  default?: unknown;
  required?: boolean;
}

/** Action interface — every action module default-exports this */
export interface Action {
  name: string;
  description: string;
  params: Record<string, ParamDef>;
  run(params: Record<string, unknown>, ctx: HarnessContext): Promise<ActionResult>;
}

/** Resource — tagged union for tracked entities */
export type Resource =
  | ExecutorResource
  | BrowserResource
  | NeighbourhoodResource
  | FluxServerResource
  | GenericResource;

export interface ExecutorResource {
  kind: 'executor';
  id: string;
  pid: number;
  port: number;
  host: string;
  dataDir?: string;
  did?: string;
  jwt?: string;
  binaryPath?: string;
  [key: string]: unknown;
}

export interface BrowserResource {
  kind: 'browser';
  id: string;
  url: string;
  executorId: string;
  [key: string]: unknown;
}

export interface NeighbourhoodResource {
  kind: 'neighbourhood';
  id: string;
  url: string;
  perspectiveUuid: string;
  executorId: string;
  [key: string]: unknown;
}

export interface FluxServerResource {
  kind: 'flux-server';
  id: string;
  pid: number;
  url: string;
  [key: string]: unknown;
}

export interface GenericResource {
  kind: string;
  id: string;
  [key: string]: unknown;
}

/** Session data persisted to session.json */
export interface SessionData {
  resources: Record<string, Resource>;
  state: Record<string, unknown>;
  counters: Record<string, number>;
  portCounter: number;
}

/** HarnessContext interface */
export interface HarnessContext {
  resources: Map<string, Resource>;
  state: Map<string, unknown>;

  executor(id: string): ExecutorResource | undefined;
  executors(): ExecutorResource[];
  browser(id: string): BrowserResource | undefined;
  browsers(): BrowserResource[];
  neighbourhood(id: string): NeighbourhoodResource | undefined;
  neighbourhoods(): NeighbourhoodResource[];

  nextId(prefix: string): string;
  nextPort(base?: number): number;

  run(action: string, params: Record<string, unknown>): Promise<ActionResult>;

  save(): Promise<void>;
  toJSON(): SessionData;
}

/** Options for process spawning */
export interface SpawnOptions {
  cwd?: string;
  env?: Record<string, string>;
  shell?: boolean;
  detached?: boolean;
}

/** Options for retry/polling */
export interface RetryOptions {
  maxAttempts?: number;
  delayMs?: number;
  backoffFactor?: number;
  maxDelayMs?: number;
  timeoutMs?: number;
}

/** SSH connection options */
export interface SSHOptions {
  host: string;
  user?: string;
  port?: number;
  keyPath?: string;
}

/** Command runner interface — shared by local and SSH */
export interface CommandRunner {
  host: string;
  exec(cmd: string, opts?: { cwd?: string; timeout?: number }): Promise<{ stdout: string; stderr: string; code: number }>;
  spawn(cmd: string, opts?: SpawnOptions): Promise<number>; // returns PID
}
