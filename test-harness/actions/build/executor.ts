// actions/build/executor.ts — Build AD4M Rust executor with full dependency chain
//
// Build chain:
// 1. Ensure JS executor bundle is built: pnpm run build-core-executor (in rust-executor/)
// 2. Build CUSTOM_DENO_SNAPSHOT.bin: pnpm run build-deno-snapshot (in rust-executor/)
// 3. Build rust-executor + cli: cargo build --release -p ad4m --features sfu
//
// The Deno snapshot bundles the JS executor code into a binary blob that the Rust
// executor loads at startup. If you change any JS code in the executor pipeline,
// you MUST rebuild the snapshot.

import type { Action } from '../../lib/types.js';
import { LocalRunner } from '../../lib/process.js';
import { createRunner } from '../../lib/ssh.js';
import path from 'node:path';

const action: Action = {
  name: 'build/executor',
  description: 'Build AD4M Rust executor with features (sfu, etc). Includes JS bundle and Deno snapshot.',
  params: {
    host: { type: 'string', description: '"local" or SSH target', default: 'local' },
    repoPath: { type: 'string', description: 'Path to AD4M repo root' },
    features: { type: 'string[]', description: 'Cargo features', default: ['sfu'] },
    release: { type: 'boolean', description: 'Build in release mode', default: true },
    skipSnapshot: { type: 'boolean', description: 'Skip Deno snapshot rebuild (faster if JS unchanged)', default: false },
    skipJsBundle: { type: 'boolean', description: 'Skip JS executor bundle (faster if JS unchanged)', default: false },
  },

  async run(params, ctx) {
    const start = Date.now();
    const host = (params.host as string) ?? 'local';
    const repoRoot = (params.repoPath as string) ??
      (host === 'local' ? path.resolve(process.cwd(), '..') : '~/ad4m');
    const features = (params.features as string[]) ?? ['sfu'];
    const release = (params.release as boolean) ?? true;
    const skipSnapshot = (params.skipSnapshot as boolean) ?? false;
    const skipJsBundle = (params.skipJsBundle as boolean) ?? false;
    const runner = host === 'local' ? new LocalRunner() : createRunner(host);
    const steps: Array<{ step: string; ok: boolean; duration_ms: number; error?: string }> = [];

    const runStep = async (step: string, cmd: string, cwd: string, timeout?: number): Promise<boolean> => {
      const stepStart = Date.now();
      const result = await runner.exec(cmd, { cwd, timeout: timeout ?? 600000 });
      const ok = result.code === 0;
      steps.push({
        step,
        ok,
        duration_ms: Date.now() - stepStart,
        ...(ok ? {} : { error: result.stderr.slice(-500) }),
      });
      return ok;
    };

    // Step 1: Build JS executor bundle
    // This compiles the TypeScript executor code that gets embedded into the Rust binary
    if (!skipJsBundle) {
      if (!await runStep(
        'executor/js-bundle',
        'pnpm run build-core-executor',
        `${repoRoot}/rust-executor`,
        300000
      )) {
        return { ok: false, data: { steps }, error: 'JS executor bundle build failed', duration_ms: Date.now() - start };
      }
    }

    // Step 2: Build CUSTOM_DENO_SNAPSHOT.bin
    // This creates the Deno runtime snapshot that includes all JS executor code.
    // Takes ~2-5 minutes. Skip if only Rust code changed.
    if (!skipSnapshot) {
      if (!await runStep(
        'executor/deno-snapshot',
        'pnpm run build-deno-snapshot',
        `${repoRoot}/rust-executor`,
        600000
      )) {
        return { ok: false, data: { steps }, error: 'Deno snapshot build failed', duration_ms: Date.now() - start };
      }
    }

    // Step 3: Build Rust executor + CLI
    const featuresFlag = features.length > 0 ? `--features ${features.join(',')}` : '';
    const releaseFlag = release ? '--release' : '';
    const cargoCmd = `cargo build ${releaseFlag} -p ad4m ${featuresFlag}`;

    if (!await runStep(
      'executor/cargo-build',
      cargoCmd,
      repoRoot,
      900000 // 15 min for full Rust build
    )) {
      return { ok: false, data: { steps }, error: 'Cargo build failed', duration_ms: Date.now() - start };
    }

    const profile = release ? 'release' : 'debug';
    const binaryPath = `${repoRoot}/target/${profile}/ad4m`;

    return {
      ok: true,
      data: {
        steps,
        binaryPath,
        features,
        profile,
      },
      duration_ms: Date.now() - start,
    };
  },
};

export default action;
