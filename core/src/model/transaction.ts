/** Atomic batch-transaction helper — see {@link runTransaction}. */

import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";

// ─────────────────────────────────────────────────────────────────────────────
// TransactionContext
// ─────────────────────────────────────────────────────────────────────────────

/**
 * An open batch transaction on a perspective.
 *
 * Obtained from the callback argument of {@link runTransaction}. Pass
 * `tx.batchId` to `save()`, `delete()` etc. to enlist those operations in the
 * transaction. Commit and abort are handled automatically by `runTransaction`.
 */
export interface TransactionContext {
  /** The underlying batch ID — pass to save/delete/add/remove calls. */
  readonly batchId: string;
  /** The perspective this transaction is open on. */
  readonly perspective: PerspectiveProxy;
}

// ─────────────────────────────────────────────────────────────────────────────
// runTransaction
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Commits on success, aborts (discards batch) and re-throws on failure.
 *
 * @example
 * ```typescript
 * await Ad4mModel.transaction(perspective, async (tx) => {
 *   await post.save(tx.batchId);
 *   await comment.save(tx.batchId);
 * });
 * ```
 */
export async function runTransaction<T>(
  perspective: PerspectiveProxy,
  callback: (tx: TransactionContext) => Promise<T>,
): Promise<T> {
  const batchId = await perspective.createBatch();
  const tx: TransactionContext = { batchId, perspective };

  try {
    const result = await callback(tx);
    await perspective.commitBatch(batchId);
    return result;
  } catch (err) {
    // PerspectiveProxy has no abortBatch — the uncommitted batch will be
    // discarded by the runtime on its next GC cycle.  Log at debug level
    // only: the re-thrown error already carries all actionable information,
    // and logging at warn would spam the console for intentional rollbacks.
    console.debug(
      `[Ad4mModel.transaction] callback threw — batch ${batchId} was NOT committed and will be discarded.`,
      err,
    );
    throw err;
  }
}
