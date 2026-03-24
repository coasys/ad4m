// actions/executor/stop.ts — Stop executor instance(s)

import type { Action } from '../../lib/types.js';
import { killProcess } from '../../lib/process.js';

const action: Action = {
  name: 'executor/stop',
  description: 'Stop one or all executor instances',
  params: {
    executorId: { type: 'string', description: 'Executor resource ID (or "all")' },
  },

  async run(params, ctx) {
    const start = Date.now();
    const stopped: string[] = [];
    const failed: string[] = [];
    const targetId = params.executorId as string | undefined;

    const executors = targetId === 'all'
      ? ctx.executors()
      : targetId
        ? [ctx.executor(targetId)].filter(Boolean)
        : ctx.executors(); // default: stop all in current session

    for (const exec of executors) {
      if (!exec) continue;
      try {
        if (exec.host === 'local' || exec.host === 'localhost') {
          killProcess(exec.pid);
        } else {
          const { createRunner } = await import('../../lib/ssh.js');
          const runner = createRunner(exec.host);
          await runner.exec(`kill ${exec.pid}`);
        }
        ctx.resources.delete(exec.id);
        stopped.push(exec.id);
      } catch {
        failed.push(exec.id);
      }
    }

    return {
      ok: failed.length === 0,
      data: { stopped, failed },
      duration_ms: Date.now() - start,
    };
  },
};

export default action;
