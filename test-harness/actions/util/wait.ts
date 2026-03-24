// actions/util/wait.ts — Wait for a condition (port, process, selector)

import type { Action } from '../../lib/types.js';
import { waitForPort, waitUntil } from '../../lib/retry.js';
import { isProcessAlive } from '../../lib/process.js';

const action: Action = {
  name: 'util/wait',
  description: 'Wait for a condition: port availability, process liveness, or browser selector',
  params: {
    condition: { type: 'string', description: 'Condition type: port | process | selector', required: true },
    host: { type: 'string', description: 'Host for port check', default: 'localhost' },
    port: { type: 'number', description: 'Port number (for port condition)' },
    pid: { type: 'number', description: 'Process ID (for process condition)' },
    browserId: { type: 'string', description: 'Browser resource ID (for selector condition)' },
    selector: { type: 'string', description: 'CSS selector (for selector condition)' },
    timeout: { type: 'number', description: 'Timeout in ms', default: 30000 },
  },

  async run(params, ctx) {
    const start = Date.now();
    const condition = params.condition as string;
    const timeout = (params.timeout as number) ?? 30000;

    try {
      switch (condition) {
        case 'port': {
          const host = (params.host as string) ?? 'localhost';
          const port = params.port as number;
          if (!port) return { ok: false, error: 'Port required for port condition', duration_ms: Date.now() - start };
          const met = await waitForPort(host, port, timeout);
          return { ok: met, data: { met, condition: 'port', host, port, elapsed_ms: Date.now() - start }, duration_ms: Date.now() - start };
        }

        case 'process': {
          const pid = params.pid as number;
          if (!pid) return { ok: false, error: 'PID required for process condition', duration_ms: Date.now() - start };
          const result = await waitUntil(() => !isProcessAlive(pid), { timeoutMs: timeout });
          return { ok: true, data: { met: result.met, condition: 'process', pid, processExited: result.met, elapsed_ms: result.elapsed_ms }, duration_ms: Date.now() - start };
        }

        case 'selector': {
          const browserId = params.browserId as string;
          const selector = params.selector as string;
          if (!browserId || !selector) {
            return { ok: false, error: 'browserId and selector required', duration_ms: Date.now() - start };
          }
          const browser = ctx.browser(browserId);
          if (!browser) return { ok: false, error: `Browser ${browserId} not found`, duration_ms: Date.now() - start };
          const driver = (browser as Record<string, unknown>)._driver as { waitForSelector?: (s: string, t: number) => Promise<void> } | undefined;
          if (!driver?.waitForSelector) return { ok: false, error: 'Browser driver not available', duration_ms: Date.now() - start };
          try {
            await driver.waitForSelector(selector, timeout);
            return { ok: true, data: { met: true, condition: 'selector', selector, elapsed_ms: Date.now() - start }, duration_ms: Date.now() - start };
          } catch {
            return { ok: false, data: { met: false, condition: 'selector', selector, elapsed_ms: Date.now() - start }, duration_ms: Date.now() - start };
          }
        }

        default:
          return { ok: false, error: `Unknown condition: ${condition}`, duration_ms: Date.now() - start };
      }
    } catch (err) {
      return {
        ok: false,
        error: `Wait failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
