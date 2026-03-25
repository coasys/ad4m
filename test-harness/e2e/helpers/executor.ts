// e2e/helpers/executor.ts — Executor lifecycle helpers for E2E tests

import { ChildProcess, spawn } from 'node:child_process';
import { mkdirSync, rmSync } from 'node:fs';
import { ExecutorAPI } from '../../lib/api.js';
import { waitForPort, sleep } from '../../lib/retry.js';

export interface ExecutorInstance {
  id: string;
  port: number;
  did: string;
  api: ExecutorAPI;
  adminCredential: string;
  process: ChildProcess;
  dataDir: string;
  stdout: string;
}

let portCounter = 12100;

function nextPort(): number {
  return portCounter++;
}

export function resetPortCounter(base = 12100): void {
  portCounter = base;
}

export interface StartExecutorOpts {
  port?: number;
  holochain?: boolean;
  adminCredential?: string;
  binaryPath?: string;
}

/**
 * Start an AD4M executor, init its data dir, generate agent, return handle.
 */
export async function startExecutor(opts: StartExecutorOpts = {}): Promise<ExecutorInstance> {
  const port = opts.port ?? nextPort();
  const adminCredential = opts.adminCredential ?? 'harness-admin';
  const binaryPath = opts.binaryPath
    ?? process.env.AD4M_BINARY
    ?? `${(process.env.AD4M_REPO ?? process.cwd().replace(/\/test-harness(\/e2e)?$/, ''))}/target/release/ad4m-executor`;
  const holochain = opts.holochain ?? true;
  const id = `e2e-exec-${port}`;
  const dataDir = `/tmp/ad4m-e2e-${id}-${Date.now()}`;

  mkdirSync(dataDir, { recursive: true });

  // Init
  const initProc = spawn(binaryPath, ['init', '--data-path', dataDir], {
    stdio: ['pipe', 'pipe', 'pipe'],
  });
  await new Promise<void>((resolve, reject) => {
    initProc.on('close', (code) => (code === 0 ? resolve() : reject(new Error(`init exited ${code}`))));
    initProc.on('error', reject);
  });

  // Start
  const args = [
    'run',
    '--app-data-path', dataDir,
    '--gql-port', String(port),
    '--admin-credential', adminCredential,
    '--run-dapp-server', 'false',
    '--language-language-only', 'false',
    '--connect-holochain', String(holochain),
  ];

  let stdoutBuffer = '';
  const proc = spawn(binaryPath, args, {
    stdio: ['pipe', 'pipe', 'pipe'],
    env: { ...process.env },
  });

  proc.stdout?.on('data', (chunk: Buffer) => {
    stdoutBuffer += chunk.toString();
  });
  proc.stderr?.on('data', (chunk: Buffer) => {
    stdoutBuffer += chunk.toString();
  });

  // Wait for GraphQL port
  await waitForPort('localhost', port, 60_000);

  const endpoint = `http://localhost:${port}/graphql`;
  const api = new ExecutorAPI(endpoint, adminCredential);

  // Generate agent
  let did = '';
  try {
    const result = await api.query(`mutation { agentGenerate(passphrase: "test-passphrase") { did } }`);
    did = (result.data?.agentGenerate as { did: string })?.did ?? '';
  } catch {
    // Agent may already exist
    const result = await api.query(`{ agent { did } }`);
    did = (result.data?.agent as { did: string })?.did ?? '';
  }

  return {
    id,
    port,
    did,
    api,
    adminCredential,
    process: proc,
    dataDir,
    get stdout() { return stdoutBuffer; },
  } as ExecutorInstance;
}

/**
 * Start N executors.
 */
export async function startExecutors(n: number, opts: StartExecutorOpts = {}): Promise<ExecutorInstance[]> {
  const executors: ExecutorInstance[] = [];
  for (let i = 0; i < n; i++) {
    executors.push(await startExecutor(opts));
  }
  return executors;
}

/**
 * Stop an executor and clean up its data dir.
 */
export async function stopExecutor(executor: ExecutorInstance): Promise<void> {
  try {
    executor.process.kill('SIGTERM');
    await new Promise<void>((resolve) => {
      const timeout = setTimeout(() => {
        executor.process.kill('SIGKILL');
        resolve();
      }, 5000);
      executor.process.on('close', () => {
        clearTimeout(timeout);
        resolve();
      });
    });
  } catch {
    // already dead
  }
  try {
    rmSync(executor.dataDir, { recursive: true, force: true });
  } catch {
    // best-effort cleanup
  }
}

/**
 * Stop all executors.
 */
export async function stopAll(executors: ExecutorInstance[]): Promise<void> {
  await Promise.all(executors.map(stopExecutor));
}
