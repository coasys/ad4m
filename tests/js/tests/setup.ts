/**
 * Mocha global setup — loaded via --require in all test scripts.
 *
 * Polyfills global.fetch with node-fetch so test files don't each need to
 * import and assign it individually.
 *
 * Also installs a process-level unhandledRejection guard for WebSocket close
 * code 1006 (abnormal closure). When the executor is killed in after-all hooks,
 * all active GraphQL subscriptions close with 1006. graphql-ws fires a
 * Promise rejection for each active subscription; if any subscription's
 * zen-observable observer has already been cleaned up (a race that only
 * manifests on slower CI machines), the rejection escapes our per-subscription
 * error handlers. Without this guard, mocha attributes those stray rejections
 * to whichever after-all hook happens to be running — failing the run despite
 * all tests passing.
 */
import fetch from "node-fetch";

// @ts-ignore — node-fetch v3 type is close enough for runtime use
(global as any).fetch = fetch;

/**
 * Swallow unhandled Promise rejections that are simply WebSocket 1006 close
 * events. Re-throw everything else so mocha still catches real errors.
 */
function isSocketCloseError(reason: any): boolean {
  if (!reason) return false;
  if (typeof reason.code === "number" && reason.code === 1006) return true;
  const msg = String(reason?.message ?? reason);
  return msg.includes("Socket closed with event 1006");
}

process.on("unhandledRejection", (reason: any) => {
  if (isSocketCloseError(reason)) return; // expected on executor shutdown
  // Re-throw so mocha still catches genuine unhandled rejections as failures
  throw reason;
});
