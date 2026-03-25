// e2e/helpers/flux-server.ts — Flux dev server lifecycle helpers

import { ChildProcess, spawn } from 'node:child_process';
import { waitForPort } from '../../lib/retry.js';

export interface FluxServer {
  url: string;
  port: number;
  process: ChildProcess;
}

let fluxPortCounter = 3030 + Math.floor(Math.random() * 900);

export function resetFluxPortCounter(base = 3030): void {
  fluxPortCounter = base;
}

/**
 * Start a Flux Vite dev server on the given port.
 */
export async function startFluxServer(opts?: { port?: number }): Promise<FluxServer> {
  const port = opts?.port ?? fluxPortCounter++;
  const fluxDir = process.env.FLUX_PATH
    ?? '/tmp/flux-sfu-harness/app';

  const proc = spawn('npx', ['vite', '--port', String(port), '--strictPort', '--host'], {
    cwd: fluxDir,
    stdio: ['pipe', 'pipe', 'pipe'],
    env: { ...process.env },
  });

  let output = '';
  proc.stdout?.on('data', (chunk: Buffer) => { output += chunk.toString(); });
  proc.stderr?.on('data', (chunk: Buffer) => { output += chunk.toString(); });

  // Wait for Vite to start
  await new Promise<void>((resolve, reject) => {
    const timeout = setTimeout(() => reject(new Error(`Flux server timeout on port ${port}. Output: ${output.slice(-500)}`)), 60_000);
    const handler = (chunk: Buffer) => {
      const text = chunk.toString();
      if (text.includes('Local:') || text.includes('ready in') || text.includes('VITE')) {
        clearTimeout(timeout);
        proc.stdout?.off('data', handler);
        proc.stderr?.off('data', handler);
        // Give it a moment to fully bind
        setTimeout(resolve, 1000);
      }
    };
    proc.stdout?.on('data', handler);
    proc.stderr?.on('data', handler);
    proc.on('error', (err) => { clearTimeout(timeout); reject(err); });
  });

  return { url: `https://localhost:${port}`, port, process: proc };
}

/**
 * Stop a Flux server.
 */
export async function stopFluxServer(server: FluxServer): Promise<void> {
  try {
    server.process.kill('SIGTERM');
    await new Promise<void>((resolve) => {
      const timeout = setTimeout(() => {
        server.process.kill('SIGKILL');
        resolve();
      }, 5000);
      server.process.on('close', () => {
        clearTimeout(timeout);
        resolve();
      });
    });
  } catch {
    // already dead
  }
}
