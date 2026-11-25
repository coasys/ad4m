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
      v && typeof v === 'object' && typeof v.id === 'string' ? v.id : copyJson(v),
    );
  }
  return copyJson(value);
}

/**
 * A JSON object value is copied, so a snapshot keeps what the field held when it
 * was taken even if the caller changes that object afterwards (the write queued
 * in a transaction is a copy too). Any other value is returned as it is.
 */
function copyJson(value: any): any {
  return isPlainObject(value) ? JSON.parse(JSON.stringify(value)) : value;
}

function isPlainObject(value: any): boolean {
  return value !== null && typeof value === 'object' && Object.getPrototypeOf(value) === Object.prototype;
}

/** Whether two normalized values are the same: JSON objects by content, anything else by `===`. */
export function sameValue(a: any, b: any): boolean {
  if (isPlainObject(a) && isPlainObject(b)) return JSON.stringify(a) === JSON.stringify(b);
  return a === b;
}


