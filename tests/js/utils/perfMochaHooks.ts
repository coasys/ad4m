/**
 * Mocha root hooks: a timing log of every test, for the nightly performance
 * history (`scripts/perf/llm-e2e-results.mjs` reads it).
 *
 * Loaded with `--require utils/perfMochaHooks.ts` (test-model, test-integration).
 * Inert unless `PERF_MOCHA_LOG` names a file; then it appends JSON Lines:
 *
 *   {"event":"start","at":<epoch ms>}
 *   {"event":"test","title":"<full title>","state":"passed|failed|pending","duration_ms":N,"at":...}
 *   {"event":"end","at":<epoch ms>}
 *
 * `duration_ms` is Mocha's own time of the test body (no hooks). Writing never
 * fails a test.
 */

import { appendFileSync, mkdirSync } from "node:fs";
import { dirname } from "node:path";

function log(entry: Record<string, unknown>): void {
  const file = process.env.PERF_MOCHA_LOG;
  if (!file) return;
  try {
    mkdirSync(dirname(file), { recursive: true });
    appendFileSync(file, JSON.stringify({ ...entry, at: Date.now() }) + "\n");
  } catch (e) {
    console.warn(`perf log ${file} not written: ${(e as Error).message}`);
  }
}

export const mochaHooks = {
  beforeAll() {
    log({ event: "start" });
  },
  afterEach(this: Mocha.Context) {
    const t = this.currentTest;
    if (!t) return;
    log({ event: "test", title: t.fullTitle(), state: t.state ?? "pending", duration_ms: t.duration ?? null });
  },
  afterAll() {
    log({ event: "end" });
  },
};
