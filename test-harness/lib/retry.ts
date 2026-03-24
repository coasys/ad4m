// lib/retry.ts — Backoff, timeout, condition polling

import type { RetryOptions } from './types.js';

const DEFAULTS: Required<RetryOptions> = {
  maxAttempts: 20,
  delayMs: 500,
  backoffFactor: 1.5,
  maxDelayMs: 10000,
  timeoutMs: 60000,
};

/** Retry an async function with exponential backoff */
export async function retryWithBackoff<T>(
  fn: () => Promise<T>,
  opts?: RetryOptions
): Promise<T> {
  const o = { ...DEFAULTS, ...opts };
  const deadline = Date.now() + o.timeoutMs;
  let delay = o.delayMs;
  let lastError: Error | undefined;

  for (let attempt = 0; attempt < o.maxAttempts; attempt++) {
    if (Date.now() > deadline) break;
    try {
      return await fn();
    } catch (err) {
      lastError = err instanceof Error ? err : new Error(String(err));
      const remaining = deadline - Date.now();
      if (remaining <= 0) break;
      await sleep(Math.min(delay, remaining));
      delay = Math.min(delay * o.backoffFactor, o.maxDelayMs);
    }
  }

  throw lastError ?? new Error('Retry timed out');
}

/** Poll a condition until it returns true */
export async function waitUntil(
  condition: () => Promise<boolean> | boolean,
  opts?: { timeoutMs?: number; intervalMs?: number }
): Promise<{ met: boolean; elapsed_ms: number }> {
  const timeoutMs = opts?.timeoutMs ?? 30000;
  const intervalMs = opts?.intervalMs ?? 500;
  const start = Date.now();
  const deadline = start + timeoutMs;

  while (Date.now() < deadline) {
    try {
      if (await condition()) {
        return { met: true, elapsed_ms: Date.now() - start };
      }
    } catch {
      // condition threw — keep polling
    }
    await sleep(Math.min(intervalMs, deadline - Date.now()));
  }

  return { met: false, elapsed_ms: Date.now() - start };
}

/** Wait for a TCP port to accept connections */
export async function waitForPort(
  host: string,
  port: number,
  timeoutMs: number = 30000
): Promise<boolean> {
  const net = await import('node:net');
  const result = await waitUntil(
    () =>
      new Promise<boolean>((resolve) => {
        const sock = net.createConnection({ host, port }, () => {
          sock.destroy();
          resolve(true);
        });
        sock.on('error', () => {
          sock.destroy();
          resolve(false);
        });
        sock.setTimeout(2000, () => {
          sock.destroy();
          resolve(false);
        });
      }),
    { timeoutMs, intervalMs: 500 }
  );
  return result.met;
}

export function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
