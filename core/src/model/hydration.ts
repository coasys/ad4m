/**
 * hydration.ts — Instance hydration helpers extracted from Ad4mModel.
 *
 * After the Rust model_query pipeline migration, most hydration logic moved
 * to Rust. This file retains:
 *   - normalizeValue() — snapshot dirty-tracking
 */

// ──────────────────────────────────────────────────────────
//  Pure helpers
// ──────────────────────────────────────────────────────────

/**
 * Normalize a value for snapshot storage.
 * Arrays of model instances are reduced to their `.id` strings so that
 * dirty-tracking compares stable identifiers instead of object references.
 */
export function normalizeValue(value: any): any {
  if (Array.isArray(value)) {
    return value.map((v: any) =>
      v && typeof v === 'object' && typeof v.id === 'string' ? v.id : v,
    );
  }
  return value;
}


