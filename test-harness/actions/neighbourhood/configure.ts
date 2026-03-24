// actions/neighbourhood/configure.ts — Configure neighbourhood settings

import type { Action } from '../../lib/types.js';
import { ExecutorAPI } from '../../lib/api.js';

const action: Action = {
  name: 'neighbourhood/configure',
  description: 'Configure neighbourhood settings (SFU mode, etc)',
  params: {
    neighbourhoodId: { type: 'string', description: 'Neighbourhood resource ID', required: true },
    executorId: { type: 'string', description: 'Executor resource ID', required: true },
    config: { type: 'object', description: 'Key-value configuration to apply' },
  },

  async run(params, ctx) {
    const start = Date.now();
    const nhId = params.neighbourhoodId as string;
    const execId = params.executorId as string;
    const config = (params.config as Record<string, unknown>) ?? {};
    const nh = ctx.neighbourhood(nhId);
    const exec = ctx.executor(execId);

    if (!nh) return { ok: false, error: `Neighbourhood ${nhId} not found`, duration_ms: Date.now() - start };
    if (!exec) return { ok: false, error: `Executor ${execId} not found`, duration_ms: Date.now() - start };

    const endpoint = `http://${exec.host === 'local' ? 'localhost' : exec.host}:${exec.port}/graphql`;
    const api = new ExecutorAPI(endpoint, exec.jwt ?? exec.adminCredential as string | undefined);

    try {
      const applied: string[] = [];

      // Apply SFU config if present
      if (config.sfuMode) {
        await api.sfuSetConfig(nh.url, config.sfuMode as string, {
          designatedPeer: config.designatedPeer as string | undefined,
        });
        applied.push('sfuMode');
      }

      // Store any additional config in the neighbourhood resource
      for (const [key, value] of Object.entries(config)) {
        (nh as Record<string, unknown>)[key] = value;
      }
      ctx.resources.set(nhId, nh);

      return {
        ok: true,
        data: { applied, neighbourhoodId: nhId },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `Configuration failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
