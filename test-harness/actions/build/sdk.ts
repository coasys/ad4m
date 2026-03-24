// actions/build/sdk.ts — Build AD4M core TS SDK + connect + hooks packages
//
// Build chain:
// 1. core (TS SDK) → pnpm exec tsc && pnpm run bundle
// 2. core buildSchema → regenerates schema.gql (needed by rust-client)
// 3. connect (ad4m-connect) → pnpm run build
//    CRITICAL: ad4m-connect's esbuild uses bundle:true with no external,
//    so it INLINES its own copy of @coasys/ad4m. You MUST rebuild connect
//    whenever core changes.
// 4. ad4m-hooks/helpers → pnpm run build
// 5. ad4m-hooks/vue → pnpm run build
// 6. ad4m-hooks/react → pnpm run build

import type { Action } from '../../lib/types.js';
import { LocalRunner } from '../../lib/process.js';
import { createRunner } from '../../lib/ssh.js';
import path from 'node:path';

const action: Action = {
  name: 'build/sdk',
  description: 'Build AD4M core TS SDK, ad4m-connect, and hooks packages in correct dependency order',
  params: {
    host: { type: 'string', description: '"local" or SSH target', default: 'local' },
    repoPath: { type: 'string', description: 'Path to AD4M repo root' },
    skipHooks: { type: 'boolean', description: 'Skip building hooks packages', default: false },
  },

  async run(params, ctx) {
    const start = Date.now();
    const host = (params.host as string) ?? 'local';
    const repoRoot = (params.repoPath as string) ??
      (host === 'local' ? path.resolve(process.cwd(), '..') : '~/ad4m');
    const skipHooks = (params.skipHooks as boolean) ?? false;
    const runner = host === 'local' ? new LocalRunner() : createRunner(host);
    const steps: Array<{ step: string; ok: boolean; duration_ms: number; error?: string }> = [];

    const runStep = async (step: string, cmd: string, cwd: string): Promise<boolean> => {
      const stepStart = Date.now();
      const result = await runner.exec(cmd, { cwd, timeout: 300000 });
      const ok = result.code === 0;
      steps.push({
        step,
        ok,
        duration_ms: Date.now() - stepStart,
        ...(ok ? {} : { error: result.stderr.slice(-500) }),
      });
      if (!ok) return false;
      return true;
    };

    // Step 1: Build core SDK
    // This compiles TypeScript and creates the bundle
    if (!await runStep(
      'core/tsc',
      'pnpm exec tsc',
      `${repoRoot}/core`
    )) {
      return { ok: false, data: { steps }, error: 'Core TSC failed', duration_ms: Date.now() - start };
    }

    if (!await runStep(
      'core/bundle',
      'pnpm run bundle',
      `${repoRoot}/core`
    )) {
      return { ok: false, data: { steps }, error: 'Core bundle failed', duration_ms: Date.now() - start };
    }

    // Step 2: Regenerate schema.gql
    // This is needed by rust-client and must be done after core builds
    if (!await runStep(
      'core/buildSchema',
      'pnpm run buildSchema',
      `${repoRoot}/core`
    )) {
      // Non-fatal — try copying directly
      await runner.exec(
        `cp "${repoRoot}/core/lib/src/schema.gql" "${repoRoot}/rust-client/schema.gql"`,
        { cwd: repoRoot }
      );
      steps[steps.length - 1].error = 'buildSchema failed, attempted direct copy';
    }

    // Also copy schema.gql to rust-client if it exists
    await runner.exec(
      `[ -f "${repoRoot}/core/lib/src/schema.gql" ] && cp "${repoRoot}/core/lib/src/schema.gql" "${repoRoot}/rust-client/schema.gql" || true`,
      { cwd: repoRoot }
    );

    // Step 3: Build ad4m-connect
    // CRITICAL: This bundles core via esbuild (bundle:true, no external).
    // The bundled copy of @coasys/ad4m inside connect WON'T pick up changes
    // from pnpm/yarn resolutions — you must rebuild connect after any core change.
    if (!await runStep(
      'connect/build',
      'pnpm run build',
      `${repoRoot}/connect`
    )) {
      return { ok: false, data: { steps }, error: 'ad4m-connect build failed', duration_ms: Date.now() - start };
    }

    // Step 4-6: Build hooks packages (optional)
    if (!skipHooks) {
      // Helpers first (vue and react depend on it)
      await runStep('hooks/helpers', 'pnpm run build', `${repoRoot}/ad4m-hooks/helpers`);
      await runStep('hooks/vue', 'pnpm run build', `${repoRoot}/ad4m-hooks/vue`);
      await runStep('hooks/react', 'pnpm run build', `${repoRoot}/ad4m-hooks/react`);
      // Hooks failures are non-fatal — warn but continue
    }

    return {
      ok: true,
      data: {
        steps,
        corePath: `${repoRoot}/core`,
        connectPath: `${repoRoot}/connect`,
      },
      duration_ms: Date.now() - start,
    };
  },
};

export default action;
