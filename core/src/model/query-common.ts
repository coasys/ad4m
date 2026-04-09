/**
 * Shared query helpers used by both the Prolog and SPARQL query pipelines.
 */

import type { ParentScope } from "./types";
import { getRelationsMetadata } from "./decorators";

/**
 * Resolves the predicate for a parent query.
 *
 * Uses TS discriminated union narrowing:
 * - Raw form (`{ id, predicate }`) → predicate used as-is
 * - Model form (`{ model, id, field? }`) → lookup from relation metadata
 *   - With `field`: direct key lookup
 *   - Without `field`: scan for a relation whose `target()` matches `childCtor`
 */
export function resolveParentPredicate(
  parent: ParentScope,
  childCtor: Function,
): string {
  // Raw form — explicit predicate
  if ('predicate' in parent) return parent.predicate;

  // Model form — resolve from relation metadata
  const { model } = parent;
  const relMeta = getRelationsMetadata(model);

  // Direct lookup by field name when provided
  if (parent.field) {
    const entry = relMeta[parent.field];
    if (!entry) {
      throw new Error(
        `parent(): field "${parent.field}" is not a registered relation on ${model.name}`,
      );
    }
    return entry.predicate;
  }

  // Fallback: scan for a relation whose target matches the child class
  for (const [, entry] of Object.entries(relMeta)) {
    if (entry.target && entry.target() === childCtor) {
      return entry.predicate;
    }
  }
  throw new Error(
    `parent(): could not resolve predicate — no relation on ${model.name} targets ${(childCtor as any).name || 'the queried class'}`,
  );
}
