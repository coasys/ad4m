// actions/webrtc/configure.ts — Configure SFU/WebRTC settings

import type { Action } from '../../lib/types.js';
import { ExecutorAPI } from '../../lib/api.js';

const action: Action = {
  name: 'webrtc/configure',
  description: 'Configure SFU mode and WebRTC settings for a neighbourhood',
  params: {
    neighbourhoodId: { type: 'string', description: 'Neighbourhood resource ID', required: true },
    executorId: { type: 'string', description: 'Executor resource ID', required: true },
    mode: { type: 'string', description: 'SFU mode: mesh | sfu | cascade', required: true },
    designatedPeer: { type: 'string', description: 'Designated peer DID for SFU mode' },
  },

  async run(params, ctx) {
    const start = Date.now();
    const nhId = params.neighbourhoodId as string;
    const execId = params.executorId as string;
    const mode = params.mode as string;
    const nh = ctx.neighbourhood(nhId);
    const exec = ctx.executor(execId);

    if (!nh) return { ok: false, error: `Neighbourhood ${nhId} not found`, duration_ms: Date.now() - start };
    if (!exec) return { ok: false, error: `Executor ${execId} not found`, duration_ms: Date.now() - start };

    const endpoint = `http://${exec.host === 'local' ? 'localhost' : exec.host}:${exec.port}/graphql`;
    const api = new ExecutorAPI(endpoint, exec.jwt ?? exec.adminCredential as string | undefined);

    try {
      await api.sfuSetConfig(nh.url, mode, {
        designatedPeer: params.designatedPeer as string | undefined,
      });

      // Update neighbourhood resource
      (nh as Record<string, unknown>).sfuMode = mode;
      if (params.designatedPeer) (nh as Record<string, unknown>).designatedPeer = params.designatedPeer;
      ctx.resources.set(nhId, nh);

      return {
        ok: true,
        data: { mode, applied: true, neighbourhoodId: nhId },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `SFU configure failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
