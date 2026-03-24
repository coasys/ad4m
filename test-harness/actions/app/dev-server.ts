// actions/app/dev-server.ts — Start a Flux/WE dev server

import type { Action } from '../../lib/types.js';
import { spawnAndWait } from '../../lib/process.js';

const action: Action = {
  name: 'app/dev-server',
  description: 'Start a Flux or WE development server',
  params: {
    path: { type: 'string', description: 'Path to app repo root', required: true },
    host: { type: 'string', description: 'Host to bind', default: 'localhost' },
    port: { type: 'number', description: 'Dev server port', default: 5173 },
    command: { type: 'string', description: 'Start command override' },
  },

  async run(params, ctx) {
    const start = Date.now();
    const appPath = params.path as string;
    const host = (params.host as string) ?? 'localhost';
    const port = (params.port as number) ?? ctx.nextPort(5173);
    const command = (params.command as string) ?? `pnpm dev --host ${host} --port ${port}`;

    try {
      const { pid } = await spawnAndWait(command, port, {
        cwd: appPath,
        timeoutMs: 60000,
        env: { HOST: host, PORT: String(port) },
      });

      const id = ctx.nextId('app');
      const url = `http://${host}:${port}`;

      ctx.resources.set(id, {
        kind: 'flux-server',
        id,
        pid,
        url,
        path: appPath,
      });

      return {
        ok: true,
        data: { id, pid, url, path: appPath },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `Dev server failed: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
