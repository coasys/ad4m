// actions/executor/start.ts — Start an AD4M executor instance

import type { Action } from '../../lib/types.js';
import { LocalRunner, spawnAndWait } from '../../lib/process.js';
import { createRunner } from '../../lib/ssh.js';

const action: Action = {
  name: 'executor/start',
  description: 'Start an AD4M executor instance',
  params: {
    port: { type: 'number', description: 'GraphQL port', default: 12000 },
    host: { type: 'string', description: '"local" or SSH target', default: 'local' },
    dataDir: { type: 'string', description: 'App data directory' },
    holochain: { type: 'boolean', description: 'Enable Holochain', default: true },
    features: { type: 'string[]', description: 'Cargo features', default: ['sfu'] },
    binaryPath: { type: 'string', description: 'Path to ad4m binary' },
    adminCredential: { type: 'string', description: 'Admin credential', default: 'harness-admin' },
    runDappServer: { type: 'boolean', description: 'Run dapp server', default: false },
    languageLanguageOnly: { type: 'boolean', description: 'Language language only mode', default: false },
  },

  async run(params, ctx) {
    const start = Date.now();
    const id = ctx.nextId('exec');
    const port = (params.port as number) ?? ctx.nextPort(12000);
    const host = (params.host as string) ?? 'local';
    const dataDir = (params.dataDir as string) ?? `/tmp/ad4m-harness-${id}`;
    const adminCredential = (params.adminCredential as string) ?? 'harness-admin';
    const runDappServer = params.runDappServer ?? false;
    const languageLanguageOnly = params.languageLanguageOnly ?? false;

    // Determine binary path
    const repoRoot = process.env.AD4M_REPO ?? (host === 'local' ? process.cwd().replace(/\/test-harness$/, '') : '~/ad4m');
    const binaryPath = (params.binaryPath as string) ?? `${repoRoot}/target/release/ad4m`;

    // Build the command
    const cmd = [
      binaryPath,
      'executor', 'run',
      `--app-data-path "${dataDir}"`,
      `--gql-port ${port}`,
      `--admin-credential "${adminCredential}"`,
      `--run-dapp-server ${runDappServer}`,
      `--language-language-only ${languageLanguageOnly}`,
    ].join(' ');

    try {
      let pid: number;

      if (host === 'local') {
        // Init first
        await ctx.run('executor/init', { dataDir, host, binaryPath });
        const result = await spawnAndWait(cmd, port, { timeoutMs: 60000 });
        pid = result.pid;
      } else {
        const runner = createRunner(host);
        await runner.exec(`${binaryPath} executor init --data-path "${dataDir}"`);
        pid = await runner.spawn(cmd);
        // Wait for port on remote — we can't check directly, so use a GraphQL probe
        const { retryWithBackoff } = await import('../../lib/retry.js');
        await retryWithBackoff(async () => {
          const res = await runner.exec(`curl -s -o /dev/null -w '%{http_code}' http://localhost:${port}/graphql`);
          if (res.stdout.trim() !== '200' && res.stdout.trim() !== '400') throw new Error('Not ready');
          return true;
        }, { timeoutMs: 60000 });
      }

      const graphqlUrl = host === 'local'
        ? `http://localhost:${port}/graphql`
        : `http://${host}:${port}/graphql`;

      ctx.resources.set(id, {
        kind: 'executor',
        id,
        pid,
        port,
        host,
        dataDir,
        binaryPath,
        adminCredential,
      });

      return {
        ok: true,
        data: { id, pid, port, host, graphql: graphqlUrl },
        duration_ms: Date.now() - start,
      };
    } catch (err) {
      return {
        ok: false,
        error: `Failed to start executor: ${err instanceof Error ? err.message : String(err)}`,
        duration_ms: Date.now() - start,
      };
    }
  },
};

export default action;
