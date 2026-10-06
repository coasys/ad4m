/**
 * Shared query helpers used by both the Prolog and SPARQL query pipelines.
 */

import type { Scope, TraverseScope } from "./types";
import { isTraverseScope } from "./types";
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
/**
 * Narrow a scope to the one-parent forms, for the paths that write a link.
 *
 * A traversal describes a read outward from several anchors. Creating a record
 * "under" one would have to pick which, and nothing in the scope says — so this
 * refuses rather than guessing at the first of the list. The executor's write
 * scopes refuse it for the same reason.
 */
export function requireSingleParent(parent: Scope): Exclude<Scope, TraverseScope> {
  if (isTraverseScope(parent)) {
    throw new Error(
      'parent(): a traverse scope describes a read and names no single parent to write under. ' +
        'Use { id, predicate } or { model, id } when creating a record under a parent.',
    );
  }
  return parent;
}

export function resolveParentPredicate(
  parent: Scope,
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
