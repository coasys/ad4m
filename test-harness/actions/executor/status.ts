// actions/executor/status.ts — Check executor status

import type { Action } from '../../lib/types.js';
import { ExecutorAPI } from '../../lib/api.js';
import { isProcessAlive } from '../../lib/process.js';

const action: Action = {
  name: 'executor/status',
  description: 'Check executor status via API and process state',
  params: {
    executorId: { type: 'string', description: 'Executor resource ID', required: true },
  },

  async run(params, ctx) {
    const start = Date.now();
    const id = params.executorId as string;
    const exec = ctx.executor(id);

    if (!exec) {
      return { ok: false, error: `Executor ${id} not found in context`, duration_ms: Date.now() - start };
    }

    const alive = exec.host === 'local' ? isProcessAlive(exec.pid) : true; // Can't check remote easily
    const endpoint = `http://${exec.host === 'local' ? 'localhost' : exec.host}:${exec.port}/graphql`;
    const api = new ExecutorAPI(endpoint, exec.adminCredential as string | undefined);

    try {
      const status = await api.agentStatus();
      const runtime = await api.runtimeInfo();

      return {
        ok: true,
        data: {
          id: exec.id,
          running: alive,
          pid: exec.pid,
          port: exec.port,
          host: exec.host,
          did: status.did,
          isInitialized: status.isInitialized,
          isUnlocked: status.isUnlocked,
          runtimeVersion: runtime.ad4mExecutorVersion,
          uptime_ms: Date.now() - start,
        },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: true,
        data: {
          id: exec.id,
          running: alive,
          pid: exec.pid,
          port: exec.port,
          host: exec.host,
          apiReachable: false,
          error: err instanceof Error ? err.message : String(err),
        },
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
