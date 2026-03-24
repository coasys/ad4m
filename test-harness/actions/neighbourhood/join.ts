// actions/neighbourhood/join.ts — Join an existing neighbourhood

import type { Action } from '../../lib/types.js';
import { ExecutorAPI } from '../../lib/api.js';

const action: Action = {
  name: 'neighbourhood/join',
  description: 'Join an existing neighbourhood by URL',
  params: {
    executorId: { type: 'string', description: 'Executor resource ID', required: true },
    url: { type: 'string', description: 'Neighbourhood URL', required: true },
  },

  async run(params, ctx) {
    const start = Date.now();
    const execId = params.executorId as string;
    const url = params.url as string;
    const exec = ctx.executor(execId);

    if (!exec) {
      return { ok: false, error: `Executor ${execId} not found`, duration_ms: Date.now() - start };
    }

    const endpoint = `http://${exec.host === 'local' ? 'localhost' : exec.host}:${exec.port}/graphql`;
    const api = new ExecutorAPI(endpoint, exec.jwt ?? exec.adminCredential as string | undefined);

    try {
      const result = await api.neighbourhoodJoin(url);
      const nhId = ctx.nextId('nh');

      ctx.resources.set(nhId, {
        kind: 'neighbourhood',
        id: nhId,
        url,
        perspectiveUuid: result.uuid,
        executorId: execId,
      });

      return {
        ok: true,
        data: { id: nhId, perspectiveUuid: result.uuid, url },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `Join failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
