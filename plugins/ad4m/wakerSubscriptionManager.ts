/**
 * Re-export WakerSubscriptionManager from core.
 * The canonical implementation lives in core/src/perspectives/WakerSubscriptionManager.ts
 * so it can be imported by both the plugin and integration tests without ESM/CJS issues.
 */
export {
  WakerSubscriptionManager,
  type WakerSubscription,
  type WakerLogger,
  type WakerSubscriptionManagerOptions,
} from "../../core/src/perspectives/WakerSubscriptionManager";
