// actions/agent/jwt.ts — Generate JWT for app authentication

import type { Action } from '../../lib/types.js';
import { ExecutorAPI } from '../../lib/api.js';

const action: Action = {
  name: 'agent/jwt',
  description: 'Generate a JWT token for app authentication',
  params: {
    executorId: { type: 'string', description: 'Executor resource ID', required: true },
    appName: { type: 'string', description: 'Application name', default: 'harness-app' },
    appDesc: { type: 'string', description: 'Application description', default: 'AD4M Test Harness' },
    appUrl: { type: 'string', description: 'Application URL', default: 'http://localhost' },
  },

  async run(params, ctx) {
    const start = Date.now();
    const execId = params.executorId as string;
    const appName = (params.appName as string) ?? 'harness-app';
    const appDesc = (params.appDesc as string) ?? 'AD4M Test Harness';
    const appUrl = (params.appUrl as string) ?? 'http://localhost';
    const exec = ctx.executor(execId);

    if (!exec) {
      return { ok: false, error: `Executor ${execId} not found`, duration_ms: Date.now() - start };
    }

    const endpoint = `http://${exec.host === 'local' ? 'localhost' : exec.host}:${exec.port}/graphql`;
    const api = new ExecutorAPI(endpoint, exec.adminCredential as string | undefined);

    try {
      // Request capability → permit → generate JWT
      const requestToken = await api.requestCapability(appName, appDesc, appUrl);
      const rand = await api.permitCapability(requestToken);
      const jwt = await api.generateJwt(requestToken, rand);

      // Store JWT on executor resource
      exec.jwt = jwt;
      ctx.resources.set(execId, exec);

      return {
        ok: true,
        data: { jwt, did: exec.did, executorId: execId },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `JWT generation failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
