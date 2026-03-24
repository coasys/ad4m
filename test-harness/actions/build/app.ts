// actions/build/app.ts — Build Flux/WE app with local AD4M package linking
//
// Flux linking (use link: not file: — file: copies, link: symlinks):
//   @coasys/ad4m         → link: to ad4m core/
//   @coasys/ad4m-connect → link: to ad4m connect/
//   @coasys/ad4m-vue-hooks    → link: to ad4m ad4m-hooks/vue/
//   @coasys/ad4m-react-hooks  → link: to ad4m ad4m-hooks/react/
//   @coasys/hooks-helpers      → link: to ad4m ad4m-hooks/helpers/
//
// After linking: yarn install + clear Vite caches
//   rm -rf app/node_modules/.vite node_modules/.cache .turbo
//
// GOTCHAS:
// - ad4m-connect bundles its own copy of core (esbuild bundle:true, no external)
//   → must rebuild connect whenever core changes
// - Vite pre-bundles deps in node_modules/.vite — stale after linking
// - Turborepo caches in node_modules/.cache and .turbo — must clear
// - Service workers can cache aggressively — may need manual unregister

import type { Action } from '../../lib/types.js';
import { LocalRunner } from '../../lib/process.js';
import { createRunner } from '../../lib/ssh.js';
import path from 'node:path';

const action: Action = {
  name: 'build/app',
  description: 'Build Flux/WE app with local AD4M package linking. Handles all cache clearing and dependency gotchas.',
  params: {
    host: { type: 'string', description: '"local" or SSH target', default: 'local' },
    appPath: { type: 'string', description: 'Path to Flux/WE repo root' },
    ad4mPath: { type: 'string', description: 'Path to AD4M repo root' },
    packageManager: { type: 'string', description: 'Package manager: yarn | pnpm', default: 'yarn' },
    skipLink: { type: 'boolean', description: 'Skip relinking (if already linked)', default: false },
  },

  async run(params, ctx) {
    const start = Date.now();
    const host = (params.host as string) ?? 'local';
    const ad4mRoot = (params.ad4mPath as string) ??
      (host === 'local' ? path.resolve(process.cwd(), '..') : '~/ad4m');
    const appRoot = (params.appPath as string) ??
      (host === 'local' ? path.resolve(ad4mRoot, '../flux') : '~/flux');
    const pkgMgr = (params.packageManager as string) ?? 'yarn';
    const skipLink = (params.skipLink as boolean) ?? false;
    const runner = host === 'local' ? new LocalRunner() : createRunner(host);
    const steps: Array<{ step: string; ok: boolean; duration_ms: number; error?: string }> = [];

    const runStep = async (step: string, cmd: string, cwd: string, timeout?: number): Promise<boolean> => {
      const stepStart = Date.now();
      const result = await runner.exec(cmd, { cwd, timeout: timeout ?? 300000 });
      const ok = result.code === 0;
      steps.push({
        step,
        ok,
        duration_ms: Date.now() - stepStart,
        ...(ok ? {} : { error: result.stderr.slice(-500) }),
      });
      return ok;
    };

    // Step 1: Update package.json resolutions/overrides to use link: paths
    if (!skipLink) {
      const linkMap: Record<string, string> = {
        '@coasys/ad4m': `link:${ad4mRoot}/core`,
        '@coasys/ad4m-connect': `link:${ad4mRoot}/connect`,
        '@coasys/ad4m-vue-hooks': `link:${ad4mRoot}/ad4m-hooks/vue`,
        '@coasys/ad4m-react-hooks': `link:${ad4mRoot}/ad4m-hooks/react`,
        '@coasys/hooks-helpers': `link:${ad4mRoot}/ad4m-hooks/helpers`,
      };

      if (pkgMgr === 'yarn') {
        // Yarn 1 uses "resolutions" in root package.json
        const jqFilter = Object.entries(linkMap)
          .map(([pkg, link]) => `.resolutions["${pkg}"] = "${link}"`)
          .join(' | ');

        await runStep(
          'app/update-resolutions',
          `cat package.json | node -e "
            const fs = require('fs');
            const pkg = JSON.parse(fs.readFileSync('/dev/stdin', 'utf8'));
            pkg.resolutions = pkg.resolutions || {};
            ${Object.entries(linkMap).map(([k, v]) => `pkg.resolutions['${k}'] = '${v}';`).join('\n            ')}
            fs.writeFileSync('package.json', JSON.stringify(pkg, null, 2) + '\\n');
          "`,
          appRoot
        );
      } else {
        // pnpm uses "pnpm.overrides"
        await runStep(
          'app/update-overrides',
          `cat package.json | node -e "
            const fs = require('fs');
            const pkg = JSON.parse(fs.readFileSync('/dev/stdin', 'utf8'));
            pkg.pnpm = pkg.pnpm || {};
            pkg.pnpm.overrides = pkg.pnpm.overrides || {};
            ${Object.entries(linkMap).map(([k, v]) => `pkg.pnpm.overrides['${k}'] = '${v}';`).join('\n            ')}
            fs.writeFileSync('package.json', JSON.stringify(pkg, null, 2) + '\\n');
          "`,
          appRoot
        );
      }
    }

    // Step 2: Install dependencies
    const installCmd = pkgMgr === 'yarn' ? 'yarn install' : 'pnpm install';
    await runStep('app/install', installCmd, appRoot, 120000);

    // Step 3: Clear ALL caches
    // - app/node_modules/.vite — Vite's pre-bundle cache (stale after linking)
    // - node_modules/.cache — Turborepo cache
    // - .turbo — Turborepo build outputs
    await runStep(
      'app/clear-caches',
      'rm -rf app/node_modules/.vite node_modules/.cache .turbo',
      appRoot
    );

    // Step 4: Build the app
    await runStep('app/build', `${pkgMgr} run build`, appRoot, 300000);

    return {
      ok: steps.every(s => s.ok),
      data: {
        steps,
        appPath: appRoot,
        ad4mPath: ad4mRoot,
        ready: steps.every(s => s.ok),
      },
      duration_ms: Date.now() - start,
    };
  },
};

export default action;
