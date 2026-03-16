/**
 * Polls `condition` up to `timeoutMs` milliseconds, resolving when it returns
 * (or resolves to) `true`.  Throws a descriptive error on timeout.
 *
 * Works with both sync and async condition functions.
 */
export async function waitUntil(
  condition: () => boolean | Promise<boolean>,
  timeoutMs = 6000,
  label = "condition",
): Promise<void> {
  const deadline = Date.now() + timeoutMs;
  while (true) {
    if (await condition()) return;
    if (Date.now() >= deadline) {
      throw new Error(
        `waitUntil timed out after ${timeoutMs}ms waiting for: ${label}`,
      );
    }
    await new Promise((r) => setTimeout(r, 100));
  }
}

/**
 * Clears all links in a perspective in a single removeLinks() batch call.
 * Call at the start of each test that needs a clean slate.
 */
export { wipePerspective } from "../utils/utils.js";
