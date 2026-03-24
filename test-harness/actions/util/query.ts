// actions/util/query.ts — Execute arbitrary GraphQL query against an executor

import type { Action } from '../../lib/types.js';
import { ExecutorAPI } from '../../lib/api.js';

const action: Action = {
  name: 'util/query',
  description: 'Execute an arbitrary GraphQL query or mutation against an executor',
  params: {
    executorId: { type: 'string', description: 'Executor resource ID', required: true },
    query: { type: 'string', description: 'GraphQL query or mutation string', required: true },
    variables: { type: 'object', description: 'Query variables' },
  },

  async run(params, ctx) {
    const start = Date.now();
    const execId = params.executorId as string;
    const queryStr = params.query as string;
    const variables = (params.variables as Record<string, unknown>) ?? {};
    const exec = ctx.executor(execId);

    if (!exec) {
      return { ok: false, error: `Executor ${execId} not found`, duration_ms: Date.now() - start };
    }

    const endpoint = `http://${exec.host === 'local' ? 'localhost' : exec.host}:${exec.port}/graphql`;
    const api = new ExecutorAPI(endpoint, exec.jwt ?? exec.adminCredential as string | undefined);

    try {
      const result = await api.query(queryStr, variables);
      if (result.errors) {
        return {
          ok: false,
          data: { errors: result.errors },
          error: result.errors[0].message,
          duration_ms: Date.now() - start,
        };
      }

      return {
        ok: true,
        data: result.data ?? {},
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `Query failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
