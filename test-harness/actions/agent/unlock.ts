// actions/agent/unlock.ts — Unlock an existing agent

import type { Action } from '../../lib/types.js';
import { ExecutorAPI } from '../../lib/api.js';

const action: Action = {
  name: 'agent/unlock',
  description: 'Unlock an existing agent on an executor',
  params: {
    executorId: { type: 'string', description: 'Executor resource ID', required: true },
    passphrase: { type: 'string', description: 'Agent passphrase', required: true },
  },

  async run(params, ctx) {
    const start = Date.now();
    const execId = params.executorId as string;
    const passphrase = params.passphrase as string;
    const exec = ctx.executor(execId);

    if (!exec) {
      return { ok: false, error: `Executor ${execId} not found`, duration_ms: Date.now() - start };
    }

    const endpoint = `http://${exec.host === 'local' ? 'localhost' : exec.host}:${exec.port}/graphql`;
    const api = new ExecutorAPI(endpoint, exec.adminCredential as string | undefined);

    try {
      const result = await api.agentUnlock(passphrase);

      exec.did = result.did;
      exec.passphrase = passphrase;
      ctx.resources.set(execId, exec);

      return {
        ok: true,
        data: { did: result.did, isUnlocked: result.isUnlocked, executorId: execId },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `Agent unlock failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
