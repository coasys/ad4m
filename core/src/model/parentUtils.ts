import type { ModelMetadata, ParentQuery, ParentQueryByPredicate } from "./types";

/**
 * Resolves the predicate URI for a parent→child relation, used by
 * `Ad4mModel.create()` and the `useLive` hooks.
 *
 * If `field` is supplied, that exact relation is looked up on the parent.
 * Otherwise, all forward relations on the parent whose `relatedModel()` factory
 * returns `childCtor` are scanned; the result is used when exactly one match
 * is found.
 *
 * @throws if the field doesn't exist, or inference is ambiguous / impossible.
 *
 * @example
 * // Explicit field
 * resolveParentPredicate(Channel.getModelMetadata(), Poll, 'polls')
 *
 * // Inferred — only one @HasMany on Channel points to Poll
 * resolveParentPredicate(Channel.getModelMetadata(), Poll)
 */
export function resolveParentPredicate(
  parentMeta: ModelMetadata,
  childCtor: (new (...args: any[]) => any) | undefined,
  field?: string,
): string {
  if (field) {
    const predicate = parentMeta.relations[field]?.predicate;
    if (!predicate) {
      throw new Error(
        `resolveParentPredicate: field "${field}" not found in parent model ` +
          `"${parentMeta.className}" relations. Check that @HasMany is declared on that field.`,
      );
    }
    return predicate;
  }

  if (!childCtor) {
    throw new Error(
      `resolveParentPredicate: either "field" or a child model constructor must be provided.`,
    );
  }

  const matches = Object.values(parentMeta.relations).filter(
    (r) => r.direction !== "reverse" && r.relatedModel?.() === childCtor,
  );

  if (matches.length === 1) return matches[0].predicate;

  if (matches.length === 0) {
    throw new Error(
      `resolveParentPredicate: no forward relation pointing to ` +
        `"${(childCtor as any).name ?? String(childCtor)}" found on parent "${parentMeta.className}". ` +
        `Provide "field" explicitly or add a typed @HasMany(() => ${(childCtor as any).name ?? "ChildModel"}) on the parent.`,
    );
  }

  // matches.length > 1
  const fieldNames = Object.entries(parentMeta.relations)
    .filter((r) => r[1].direction !== "reverse" && r[1].relatedModel?.() === childCtor)
    .map(([k]) => k)
    .join(", ");
  throw new Error(
    `resolveParentPredicate: multiple relations on "${parentMeta.className}" point to ` +
      `"${(childCtor as any).name ?? String(childCtor)}" (fields: ${fieldNames}). ` +
      `Provide "field" to disambiguate.`,
  );
}

/**
 * Normalises a `Query.parent` option to its raw `{ id, predicate }` form.
 *
 * When the model-backed form `{ id, model, field? }` is supplied, the predicate
 * is resolved via {@link resolveParentPredicate}.  The raw `{ id, predicate }`
 * form is returned as-is.
 *
 * @param lf         - Either the raw or model-backed `parent` query value.
 * @param childCtor  - The child model constructor.  Required when `field` is
 *                     omitted (used for inference across `@HasMany` metadata).
 */
export function normalizeParentQuery(
  lf: ParentQuery,
  childCtor?: new (...args: any[]) => any,
): ParentQueryByPredicate {
  if ("predicate" in lf) return lf;
  const predicate = resolveParentPredicate(
    lf.model.getModelMetadata(),
    childCtor,
    lf.field,
  );
  return { id: lf.id, predicate };
}
