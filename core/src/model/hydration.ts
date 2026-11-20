/**
 * hydration.ts — Instance hydration helpers extracted from Ad4mModel.
 *
 * After the Rust model_query pipeline migration, most hydration logic moved
 * to Rust. This file retains:
 *   - normalizeValue(), snapshotOf() — snapshot dirty-tracking
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

/** A property that is not also a relation (`@HasOne` registers both). */
export function isPropertyOnly(
  metadata: { properties: Record<string, unknown>; relations: Record<string, unknown> },
  field: string,
): boolean {
  return field in metadata.properties && !(field in metadata.relations);
}

/**
 * Snapshot form of a field's value. An array held by a property (not a
 * relation) is one JSON value, e.g. an array of objects from
 * `fromJSONSchema`. It is compared whole, as its JSON text: reducing its
 * elements to `.id` would hide an edit that keeps the ids, sorting would hide
 * a reorder, and `save()` would then skip the field.
 */
export function snapshotOf(value: any, propertyOnly: boolean): any {
  if (propertyOnly && Array.isArray(value)) {
    try {
      return JSON.stringify(value);
    } catch {
      // Not JSON (e.g. a cycle): fall back to the relation form.
    }
  }
  return normalizeValue(Array.isArray(value) ? [...value] : value);
}
