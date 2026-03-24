// actions/executor/init.ts — Initialize an AD4M executor data directory

import type { Action } from '../../lib/types.js';
import { LocalRunner } from '../../lib/process.js';
import { createRunner } from '../../lib/ssh.js';

const action: Action = {
  name: 'executor/init',
  description: 'Initialize an AD4M executor data directory',
  params: {
    dataDir: { type: 'string', description: 'App data directory path' },
    host: { type: 'string', description: '"local" or SSH target', default: 'local' },
    binaryPath: { type: 'string', description: 'Path to ad4m binary', default: 'ad4m' },
  },

  async run(params, ctx) {
    const start = Date.now();
    const id = ctx.nextId('exec');
    const host = (params.host as string) ?? 'local';
    const dataDir = (params.dataDir as string) ?? `/tmp/ad4m-harness-${id}`;
    const binary = (params.binaryPath as string) ?? 'ad4m-executor';
    const runner = host === 'local' ? new LocalRunner() : createRunner(host);

    const result = await runner.exec(`${binary} init --data-path "${dataDir}"`, { timeout: 30000 });

    if (result.code !== 0 && !result.stderr.includes('already')) {
      return { ok: false, error: `Init failed: ${result.stderr}`, duration_ms: Date.now() - start };
    }

    return {
      ok: true,
      data: { id, dataDir, host },
      duration_ms: Date.now() - start,
    };
  },
};

export default action;
