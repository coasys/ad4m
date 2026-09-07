/**
 * Re-export of the canonical WakerSubscriptionManager from core.
 *
 * There is deliberately no implementation here: the class lives in
 * `core/src/perspectives/WakerSubscriptionManager.ts` and is exported from
 * `@coasys/ad4m`, so the plugin, the executor's consumers and the integration
 * tests all run the same code. This module only exists so the plugin's own
 * imports (and its vitest suite) keep a stable local path; esbuild inlines the
 * core source into `dist/index.cjs` at build time.
 *
 * Do not add logic here — change core instead.
 */

export {
  WakerSubscriptionManager,
  hintFor,
} from "../../core/src/perspectives/WakerSubscriptionManager";
export type {
  MentionMessage,
  WakerLogger,
  WakerSubscription,
  WakerSubscriptionManagerOptions,
} from "../../core/src/perspectives/WakerSubscriptionManager";
