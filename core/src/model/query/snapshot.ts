/**
 * Dirty-tracking snapshot registry for Ad4mModel instances.
 *
 * A lightweight WeakMap-based store that records the schema-declared field
 * values of an instance immediately after hydration or save. `innerUpdate`
 * consults this snapshot to skip writing fields that haven't changed since the
 * last successful persist, preventing the "re-save duplicates relations" bug.
 *
 * ## Design notes
 *
 * - A **WeakMap** is used so snapshot entries are GC'd when the instance is
 *   collected — no manual cleanup needed.
 * - Arrays are normalised to `string[]` on capture (model instances → their ID
 *   URI) so the comparison is stable regardless of whether `include` hydration
 *   was used.
 * - There is **no snapshot** for a freshly constructed instance that has never
 *   been hydrated or saved. `isDirty` returns `true` in that case, preserving
 *   existing create-path behaviour (write everything).
 * - Only Tier-1 saves (no caller-provided `batchId`) trigger a post-save
 *   rehydration via `fetchInstanceData`, which re-captures the snapshot. Tier-2
 *   / Tier-3 (caller-managed batch / `transaction()`) skip rehydration by
 *   design; fields that were not changed may be re-sent on the next Tier-1
 *   save, which is idempotent and acceptable.
 */

// ─────────────────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────────────────

type SnapshotEntry = Record<string, any>;

// ─────────────────────────────────────────────────────────────────────────────
// Registry
// ─────────────────────────────────────────────────────────────────────────────

const snapshots = new WeakMap<object, SnapshotEntry>();

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Normalise a field value for snapshot storage and comparison.
 *
 * - Arrays whose items may be model instances (from `include` hydration) are
 *   reduced to their `id` URI strings so comparisons are stable.
 * - All other values are stored as-is.
 */
function normalizeValue(value: any): any {
  if (Array.isArray(value)) {
    return value.map((v) =>
      v && typeof v === "object" && typeof v.id === "string" ? v.id : v,
    );
  }
  return value;
}

// ─────────────────────────────────────────────────────────────────────────────
// Public API
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Records the current schema-field values of `instance` as its clean baseline.
 *
 * Call this after every hydration (bulk or single-instance path) and after
 * every successful internal-batch save (the latter comes for free because
 * `saveInstance` calls `fetchInstanceData` which ends with a `captureSnapshot`
 * call).
 *
 * @param instance  - The Ad4mModel instance to snapshot.
 * @param keys      - Schema-declared field names to record (@Property keys +
 *                    relation keys). Internal machinery (`_id`, `_perspective`,
 *                    dynamically-wired `addX`/`removeX`/`setX` methods) must
 *                    NOT be included.
 */
export function captureSnapshot(instance: object, keys: string[]): void {
  const entry: SnapshotEntry = {};
  for (const key of keys) {
    entry[key] = normalizeValue((instance as any)[key]);
  }
  snapshots.set(instance, entry);
}

/**
 * Returns the snapshot entry for `instance`, or `undefined` if none has been
 * captured yet (i.e. the instance has never been hydrated or saved via an
 * internal batch).
 */
export function readSnapshot(instance: object): SnapshotEntry | undefined {
  return snapshots.get(instance);
}

/**
 * Returns `true` if `currentValue` differs from the snapshot value for `key`.
 *
 * Always returns `true` (→ write the field) when:
 *   - No snapshot has been captured (create path / instance never hydrated).
 *   - The key was not present in the snapshot (new field added after hydration).
 *
 * Array comparison is order-insensitive: both sides are sorted before checking
 * element-wise equality, because a fresh hydration may return relation IDs in a
 * different order than they were originally written.
 */
export function isDirty(
  instance: object,
  key: string,
  currentValue: any,
): boolean {
  const snap = snapshots.get(instance);
  if (!snap) return true; // no snapshot → always write (create path)
  if (!(key in snap)) return true; // field absent from snapshot → write

  const snapVal = snap[key];
  const currNorm = normalizeValue(currentValue);

  if (Array.isArray(currNorm) && Array.isArray(snapVal)) {
    if (currNorm.length !== snapVal.length) return true;
    // Relations are semantically sets; sort before comparing so reordering
    // after a fresh hydration doesn't produce false positives.
    const a = [...currNorm].sort();
    const b = [...snapVal].sort();
    return a.some((v, i) => v !== b[i]);
  }

  return currNorm !== snapVal;
}
