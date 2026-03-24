// actions/build/full-stack.ts — Orchestrate full AD4M + Flux build in correct order
//
// The full build sequence:
// 1. SDK: core → schema → connect → hooks
// 2. Executor: JS bundle → Deno snapshot → cargo build
// 3. App: link → install → clear caches → build
//
// This encodes the entire dependency chain so agents never have to reason about it.

import type { Action } from '../../lib/types.js';

const action: Action = {
  name: 'build/full-stack',
  description: 'Build everything: SDK → Executor → App, in correct dependency order',
  params: {
    host: { type: 'string', description: '"local" or SSH target', default: 'local' },
    repoPath: { type: 'string', description: 'Path to AD4M repo root' },
    appPath: { type: 'string', description: 'Path to Flux/WE repo root' },
    features: { type: 'string[]', description: 'Cargo features', default: ['sfu'] },
    skipExecutor: { type: 'boolean', description: 'Skip Rust executor build', default: false },
    skipApp: { type: 'boolean', description: 'Skip Flux/WE app build', default: false },
    skipSnapshot: { type: 'boolean', description: 'Skip Deno snapshot rebuild', default: false },
    packageManager: { type: 'string', description: 'App package manager: yarn | pnpm', default: 'yarn' },
  },

  async run(params, ctx) {
    const start = Date.now();
    const results: Array<{ action: string; ok: boolean; duration_ms: number; error?: string }> = [];

    // Step 1: Build SDK (core + connect + hooks)
    const sdkResult = await ctx.run('build/sdk', {
      host: params.host,
      repoPath: params.repoPath,
    });
    results.push({ action: 'build/sdk', ok: sdkResult.ok, duration_ms: sdkResult.duration_ms, error: sdkResult.error });

    if (!sdkResult.ok) {
      return {
        ok: false,
        data: { results },
        error: `SDK build failed: ${sdkResult.error}`,
        duration_ms: Date.now() - start,
      };
    }

    // Step 2: Build Executor (JS bundle + snapshot + Rust)
    if (!(params.skipExecutor as boolean)) {
      const execResult = await ctx.run('build/executor', {
        host: params.host,
        repoPath: params.repoPath,
        features: params.features,
        skipSnapshot: params.skipSnapshot,
      });
      results.push({ action: 'build/executor', ok: execResult.ok, duration_ms: execResult.duration_ms, error: execResult.error });

      if (!execResult.ok) {
        return {
          ok: false,
          data: { results },
          error: `Executor build failed: ${execResult.error}`,
          duration_ms: Date.now() - start,
        };
      }
    }

    // Step 3: Build App (link + install + clear caches + build)
    if (!(params.skipApp as boolean)) {
      const appResult = await ctx.run('build/app', {
        host: params.host,
        ad4mPath: params.repoPath,
        appPath: params.appPath,
        packageManager: params.packageManager,
      });
      results.push({ action: 'build/app', ok: appResult.ok, duration_ms: appResult.duration_ms, error: appResult.error });

      if (!appResult.ok) {
        return {
          ok: false,
          data: { results },
          error: `App build failed: ${appResult.error}`,
          duration_ms: Date.now() - start,
        };
      }
    }

    return {
      ok: true,
      data: { results },
      duration_ms: Date.now() - start,
    };
  },
};

export default action;
