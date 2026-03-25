// e2e/fixtures.ts — Playwright custom fixtures for AD4M SFU E2E tests

import { test as base, type Page, type BrowserContext } from '@playwright/test';
import {
  startExecutor,
  startExecutors,
  stopExecutor,
  stopAll,
  type ExecutorInstance,
  type StartExecutorOpts,
} from './helpers/executor.js';
import { injectAuth } from './helpers/auth.js';
import { injectWebRTCTracking } from './helpers/webrtc.js';
import { ExecutorAPI } from '../lib/api.js';
import { ChildProcess, spawn } from 'node:child_process';

export interface NeighbourhoodInfo {
  url: string;
  perspectiveUuid: string;
}

export interface FluxServerInfo {
  url: string;
  port: number;
  process: ChildProcess;
}

export interface AuthenticatedPageInfo {
  page: Page;
  executor: ExecutorInstance;
  fluxUrl: string;
}

// Custom fixture types
type SFUFixtures = {
  executor: ExecutorInstance;
  executorPair: ExecutorInstance[];
  executorQuad: ExecutorInstance[];
  executorOctet: ExecutorInstance[];
  neighbourhood: NeighbourhoodInfo;
  fluxServer: FluxServerInfo;
  authenticatedPage: AuthenticatedPageInfo;
};

export const test = base.extend<SFUFixtures>({
  // Single executor (no Holochain by default for SFU tests)
  executor: async ({}, use) => {
    const exec = await startExecutor({ holochain: false });
    await use(exec);
    await stopExecutor(exec);
  },

  // 2 executors
  executorPair: async ({}, use) => {
    const execs = await startExecutors(2, { holochain: false });
    await use(execs);
    await stopAll(execs);
  },

  // 4 executors
  executorQuad: async ({}, use) => {
    const execs = await startExecutors(4, { holochain: false });
    await use(execs);
    await stopAll(execs);
  },

  // 8 executors (for stress tests)
  executorOctet: async ({}, use) => {
    const execs = await startExecutors(8, { holochain: false });
    await use(execs);
    await stopAll(execs);
  },

  // Create neighbourhood on first executor of executorPair, join from second
  // NOTE: Requires Holochain — only works with holochain: true
  neighbourhood: async ({ executorPair }, use) => {
    const [creator, joiner] = executorPair;

    // Create perspective, then publish as neighbourhood
    const perspResult = await creator.api.query(`
      mutation { perspectiveAdd(name: "e2e-test-nh") { uuid } }
    `);
    const perspUuid = (perspResult.data?.perspectiveAdd as { uuid: string })?.uuid;

    // Publish as neighbourhood (requires a link language — this will likely fail without Holochain)
    const nhResult = await creator.api.query(`
      mutation {
        neighbourhoodPublishFromPerspective(
          perspectiveUUID: "${perspUuid}",
          linkLanguage: "social-context",
          meta: { links: [] }
        )
      }
    `);
    const nhUrl = nhResult.data?.neighbourhoodPublishFromPerspective as string;

    // Join from second executor
    await joiner.api.query(`
      mutation { neighbourhoodJoinFromUrl(url: "${nhUrl}") { uuid } }
    `);

    await use({ url: nhUrl, perspectiveUuid: perspUuid });
  },

  // Start a Vite dev server for Flux
  fluxServer: async ({}, use) => {
    const port = 5173 + Math.floor(Math.random() * 1000);
    const fluxDir = process.env.FLUX_PATH ?? process.env.FLUX_DIR ?? `${process.cwd().replace(/\/test-harness(\/e2e)?$/, '')}/flux`;

    const proc = spawn('npx', ['vite', '--port', String(port), '--strictPort'], {
      cwd: fluxDir,
      stdio: ['pipe', 'pipe', 'pipe'],
      env: { ...process.env },
    });

    // Wait for Vite to be ready
    await new Promise<void>((resolve, reject) => {
      const timeout = setTimeout(() => reject(new Error('Flux server start timeout')), 30_000);
      const handler = (chunk: Buffer) => {
        if (chunk.toString().includes('Local:') || chunk.toString().includes('ready')) {
          clearTimeout(timeout);
          proc.stdout?.off('data', handler);
          resolve();
        }
      };
      proc.stdout?.on('data', handler);
      proc.on('error', (err) => { clearTimeout(timeout); reject(err); });
    });

    const url = `http://localhost:${port}`;
    await use({ url, port, process: proc });

    proc.kill('SIGTERM');
  },

  // Authenticated page: opens browser at Flux URL with auth injected
  authenticatedPage: async ({ page, executor, fluxServer }, use) => {
    const executorUrl = `ws://localhost:${executor.port}/graphql`;

    // Inject WebRTC tracking
    await injectWebRTCTracking(page);

    // Inject auth credentials
    await injectAuth(page, fluxServer.url, executorUrl, executor.adminCredential);

    await use({ page, executor, fluxUrl: fluxServer.url });
  },
});

export { expect } from '@playwright/test';
