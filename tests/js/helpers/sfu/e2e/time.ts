/** Waiting, for the e2e scenarios and their fault layer. */

/** How often a readiness loop looks again when no event signals it: root AGENTS.md asks for 100 ms or less. */
export const POLL_MS = 100;

export const sleep = (ms: number): Promise<void> => new Promise((resolve) => setTimeout(resolve, ms));

/**
 * `promise`, or a rejection when it has not settled within `ms`. A budget
 * already spent (`ms <= 0`) rejects at once, even if `promise` has settled:
 * the caller's bound has passed, and a late answer is not an answer in time.
 */
export async function within<T>(ms: number, promise: Promise<T>, what: string): Promise<T> {
  if (ms <= 0) {
    promise.catch(() => {});
    throw new Error(`${what}: no budget left (${Math.round(-ms)}ms past its bound)`);
  }
  let timer: ReturnType<typeof setTimeout>;
  const late = new Promise<never>((_, reject) => {
    timer = setTimeout(() => reject(new Error(`${what}: not within ${ms}ms`)), ms);
  });
  return Promise.race([promise, late]).finally(() => clearTimeout(timer));
}
