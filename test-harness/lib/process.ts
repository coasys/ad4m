// lib/process.ts — Process lifecycle manager

import { spawn, execSync, ChildProcess } from 'node:child_process';
import { waitForPort, sleep } from './retry.js';
import type { SpawnOptions, CommandRunner } from './types.js';

/** Tracked processes for cleanup */
const trackedProcesses: Map<number, { proc: ChildProcess; label: string }> = new Map();

/** Local command runner */
export class LocalRunner implements CommandRunner {
  host = 'localhost';

  async exec(cmd: string, opts?: { cwd?: string; timeout?: number }): Promise<{ stdout: string; stderr: string; code: number }> {
    return new Promise((resolve) => {
      try {
        const stdout = execSync(cmd, {
          cwd: opts?.cwd,
          timeout: opts?.timeout ?? 120000,
          encoding: 'utf-8',
          stdio: ['pipe', 'pipe', 'pipe'],
          maxBuffer: 50 * 1024 * 1024,
        });
        resolve({ stdout, stderr: '', code: 0 });
      } catch (err: unknown) {
        const e = err as { stdout?: string; stderr?: string; status?: number };
        resolve({
          stdout: e.stdout ?? '',
          stderr: e.stderr ?? '',
          code: e.status ?? 1,
        });
      }
    });
  }

  async spawn(cmd: string, opts?: SpawnOptions): Promise<number> {
    const proc = spawn(cmd, {
      cwd: opts?.cwd,
      env: { ...process.env, ...opts?.env },
      shell: opts?.shell ?? true,
      detached: opts?.detached ?? true,
      stdio: ['ignore', 'pipe', 'pipe'],
    });

    if (!proc.pid) throw new Error(`Failed to spawn: ${cmd}`);

    // Track for cleanup
    trackedProcesses.set(proc.pid, { proc, label: cmd.slice(0, 80) });

    // Unref so it doesn't prevent exit
    proc.unref();

    return proc.pid;
  }
}

/** Spawn a process and wait for a port to be ready */
export async function spawnAndWait(
  cmd: string,
  port: number,
  opts?: SpawnOptions & { host?: string; timeoutMs?: number }
): Promise<{ pid: number }> {
  const runner = new LocalRunner();
  const pid = await runner.spawn(cmd, opts);
  const host = opts?.host ?? 'localhost';
  const ready = await waitForPort(host, port, opts?.timeoutMs ?? 30000);
  if (!ready) {
    killProcess(pid);
    throw new Error(`Process started (PID ${pid}) but port ${port} never became ready`);
  }
  return { pid };
}

/** Kill a tracked process */
export function killProcess(pid: number): boolean {
  try {
    process.kill(pid, 'SIGTERM');
    trackedProcesses.delete(pid);
    return true;
  } catch {
    trackedProcesses.delete(pid);
    return false;
  }
}

/** Kill all tracked processes */
export function killAllTracked(): { killed: string[]; failed: string[] } {
  const killed: string[] = [];
  const failed: string[] = [];

  for (const [pid, { label }] of trackedProcesses) {
    try {
      process.kill(pid, 'SIGTERM');
      killed.push(`${pid} (${label})`);
    } catch {
      failed.push(`${pid} (${label})`);
    }
  }
  trackedProcesses.clear();
  return { killed, failed };
}

/** Check if a PID is alive */
export function isProcessAlive(pid: number): boolean {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

/** Get all tracked process PIDs */
export function getTrackedPids(): number[] {
  return [...trackedProcesses.keys()];
}
