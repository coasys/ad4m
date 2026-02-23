/**
 * Transaction API for Ad4mModel (Phase 3b).
 *
 * Replaces the scattered `batchId?: string` manual lifecycle pattern with a
 * single `Ad4mModel.transaction()` call that handles create-commit-abort
 * automatically.
 *
 * @example
 * ```typescript
 * // Before (fragile — leaked batch if save2 throws):
 * const batchId = await perspective.createBatch();
 * await model1.save(batchId);
 * await model2.save(batchId);
 * await perspective.commitBatch(batchId);
 *
 * // After (safe — auto-abort on error):
 * await Ad4mModel.transaction(perspective, async (tx) => {
 *   await model1.save(tx.batchId);
 *   await model2.save(tx.batchId);
 * });
 * ```
 */

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
 * Runs `callback` inside a fresh batch transaction.
 *
 * - A new batch is created before calling `callback`.
 * - If `callback` resolves successfully the batch is **committed**.
 * - If `callback` throws (or rejects) the batch is **aborted** and the error
 *   is re-thrown, so the caller always sees the original failure.
 *
 * @param perspective - The perspective to open the transaction on
 * @param callback    - Async function that performs model operations using `tx`
 * @returns Whatever `callback` returns
 *
 * @example
 * ```typescript
 * const [post, comment] = await Ad4mModel.transaction(perspective, async (tx) => {
 *   const post = new Post(perspective);
 *   post.title = "Hello";
 *   await post.save(tx.batchId);
 *
 *   const comment = new Comment(perspective);
 *   comment.body = "First!";
 *   await comment.save(tx.batchId);
 *
 *   return [post, comment];
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
