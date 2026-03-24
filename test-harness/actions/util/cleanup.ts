// actions/util/cleanup.ts — Kill harness-managed processes and clean up session

import type { Action } from '../../lib/types.js';
import { killAllTracked, killProcess } from '../../lib/process.js';
import { promises as fs } from 'node:fs';
import path from 'node:path';

const action: Action = {
  name: 'util/cleanup',
  description: 'Kill harness-managed processes and optionally clean up session data. Default: current session. --all for all processes.',
  params: {
    all: { type: 'boolean', description: 'Kill ALL harness-managed processes (not just current session)', default: false },
    removeData: { type: 'boolean', description: 'Also remove executor data directories', default: false },
  },

  async run(params, ctx) {
    const start = Date.now();
    const all = (params.all as boolean) ?? false;
    const removeData = (params.removeData as boolean) ?? false;
    const killed: string[] = [];
    const removed: string[] = [];
    const errors: string[] = [];

    if (all) {
      // Kill all tracked processes (in-memory tracking)
      const result = killAllTracked();
      killed.push(...result.killed);
    }

    // Kill processes tracked in context (persisted session)
    for (const exec of ctx.executors()) {
      try {
        if (exec.host === 'local' || exec.host === 'localhost') {
          killProcess(exec.pid);
        }
        killed.push(`${exec.id} (PID ${exec.pid})`);

        // Remove data directory if requested
        if (removeData && exec.dataDir) {
          try {
            await fs.rm(exec.dataDir, { recursive: true, force: true });
            removed.push(exec.dataDir);
          } catch (err) {
            errors.push(`Failed to remove ${exec.dataDir}: ${err instanceof Error ? err.message : String(err)}`);
          }
        }

        ctx.resources.delete(exec.id);
      } catch (err) {
        errors.push(`Failed to stop ${exec.id}: ${err instanceof Error ? err.message : String(err)}`);
      }
    }

    // Close browser resources
    for (const browser of ctx.browsers()) {
      try {
        const driver = (browser as Record<string, unknown>)._driver as { close?: () => Promise<void> } | undefined;
        if (driver?.close) await driver.close();
        killed.push(`${browser.id} (browser)`);
        ctx.resources.delete(browser.id);
      } catch {
        // Best effort
      }
    }

    // Kill flux server resources
    const fluxServers = [...ctx.resources.values()].filter(r => r.kind === 'flux-server');
    for (const srv of fluxServers) {
      try {
        killProcess((srv as { pid: number }).pid);
        killed.push(`${srv.id} (PID ${(srv as { pid: number }).pid})`);
        ctx.resources.delete(srv.id);
      } catch {
        // Best effort
      }
    }

    // Remove session file if all cleaned up
    if (ctx.resources.size === 0) {
      try {
        const sessionPath = path.join(process.cwd(), 'session.json');
        await fs.unlink(sessionPath);
        removed.push('session.json');
      } catch {
        // File might not exist
      }
    }

    return {
      ok: errors.length === 0,
      data: { killed, removed, errors },
      duration_ms: Date.now() - start,
    };
  },
};

export default action;
