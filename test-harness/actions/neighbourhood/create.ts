// actions/neighbourhood/create.ts — Create a neighbourhood

import type { Action } from '../../lib/types.js';
import { ExecutorAPI } from '../../lib/api.js';

const action: Action = {
  name: 'neighbourhood/create',
  description: 'Create a new neighbourhood from a perspective',
  params: {
    executorId: { type: 'string', description: 'Executor resource ID', required: true },
    name: { type: 'string', description: 'Neighbourhood name', required: true },
    linkLanguage: { type: 'string', description: 'Link language address (uses default if omitted)' },
  },

  async run(params, ctx) {
    const start = Date.now();
    const execId = params.executorId as string;
    const name = params.name as string;
    const exec = ctx.executor(execId);

    if (!exec) {
      return { ok: false, error: `Executor ${execId} not found`, duration_ms: Date.now() - start };
    }

    const endpoint = `http://${exec.host === 'local' ? 'localhost' : exec.host}:${exec.port}/graphql`;
    const api = new ExecutorAPI(endpoint, exec.jwt ?? exec.adminCredential as string | undefined);

    try {
      // Create perspective first
      const perspective = await api.perspectiveAdd(name);

      // Publish as neighbourhood
      const linkLanguage = (params.linkLanguage as string) ?? 'social-context';
      const nhUrl = await api.neighbourhoodPublish(perspective.uuid, linkLanguage, { name });

      const nhId = ctx.nextId('nh');
      ctx.resources.set(nhId, {
        kind: 'neighbourhood',
        id: nhId,
        url: nhUrl,
        perspectiveUuid: perspective.uuid,
        executorId: execId,
        name,
      });

      return {
        ok: true,
        data: { id: nhId, url: nhUrl, perspectiveUuid: perspective.uuid, name },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `Neighbourhood creation failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
