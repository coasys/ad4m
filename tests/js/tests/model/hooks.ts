/**
 * Mocha Root Hooks Plugin — model test suite shared executor
 *
 * Loaded via --require in the test-model script. Starts a single HC executor
 * before the first model test file runs and tears it down after the last,
 * replacing per-file startAgent/stop pairs with one HC startup for the
 * entire model suite (files → 1 holochain conductor boot instead of N).
 *
 * Each test file calls getSharedAgent() in its before() hook. If it returns
 * null the file was run in isolation (without --require hooks.ts) and it
 * falls back to starting its own executor — so individual files stay runnable
 * standalone for debugging.
 *
 * When adding a new test group that should share one executor, follow the
 * same pattern: add a hooks.ts alongside the files, export mochaHooks and
 * getSharedAgent(), and add --require <hooks-path> to the npm script.
 */

import fetch from "node-fetch";
import { startAgent } from "../../helpers/index.js";
import type { AgentHandle } from "../../helpers/executor.js";

// @ts-ignore
global.fetch = fetch;

let _sharedAgent: AgentHandle | null = null;

/**
 * Returns the AgentHandle started by the Root Hooks Plugin, or null when a
 * test file is run in isolation (without --require hooks.ts).
 */
export function getSharedAgent(): AgentHandle | null {
  return _sharedAgent;
}

/**
 * Root Hooks — called by Mocha once around the full test run.
 */
export const mochaHooks = {
  async beforeAll(this: Mocha.Context) {
    this.timeout(120_000);
    _sharedAgent = await startAgent("model-suite");
  },

  async afterAll() {
    if (_sharedAgent) {
      await _sharedAgent.stop();
      _sharedAgent = null;
    }
  },
};
