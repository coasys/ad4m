/**
 * Standalone Prolog fact generator for Ad4mModel classes.
 *
 * Converts decorator metadata into Prolog clauses that can be prepended to
 * `perspective.infer()` calls, enabling custom Prolog rules to reference
 * model instances and their fields.
 */

import type { ModelMetadata, PropertyMetadata, RelationMetadata } from "./types";
import type { Ad4mModel } from "./Ad4mModel";

// ── Helpers ──────────────────────────────────────────────────────────────────

/**
 * Convert a camelCase or PascalCase identifier to snake_case.
 * Examples: "TestPost" -> "test_post", "createdAt" -> "created_at"
 */
function toSnakeCase(str: string): string {
  return str
    .replace(/([A-Z])/g, "_$1")
    .toLowerCase()
    .replace(/^_/, "");
}

function buildInstanceClause(predicateName: string, metadata: ModelMetadata): string | null {
  const props = metadata.properties;
  // Collect flags first — these are the strongest recognizers
  const flags = Object.values(props).filter((p) => p.flag && p.predicate && p.initial);
  if (flags.length > 0) {
    const conditions = flags
      .map((p) => `triple(X, '${p.predicate}', '${p.initial}')`)
      .join(",\n    ");
    return `${predicateName}(X) :-\n    ${conditions}.`;
  }
  // Fallback: required non-flag properties
  const required = Object.values(props).filter((p) => p.required && p.predicate && !p.flag);
  if (required.length > 0) {
    const conditions = required
      .map((p) => `triple(X, '${p.predicate}', _)`)
      .join(",\n    ");
    return `${predicateName}(X) :-\n    ${conditions}.`;
  }
  // Open-world structural fallback: match nodes that have at least one of the
  // model's declared property predicates.  Uses disjunction (;) so any single
  // predicate is enough.
  const allPredicates = Object.values(props)
    .filter((p) => p.predicate)
    .map((p) => p.predicate!);
  if (metadata.relations) {
    for (const rel of Object.values(metadata.relations)) {
      if (rel.predicate) allPredicates.push(rel.predicate);
    }
  }
  if (allPredicates.length > 0) {
    const disjunction = allPredicates
      .map((pred) => `triple(X, '${pred}', _)`)
      .join(" ;\n    ");
    return `${predicateName}(X) :-\n    (${disjunction}).`;
  }
  return null;
}

function buildPropertyClause(modelPredicateName: string, prop: PropertyMetadata): string | null {
  if (prop.flag) return null;
  if (!prop.predicate) return null;
  const clauseName = `${modelPredicateName}_${toSnakeCase(prop.name)}`;
  return `${clauseName}(X, Value) :- triple(X, '${prop.predicate}', Value).`;
}

function buildRelationClause(modelPredicateName: string, relation: RelationMetadata): string | null {
  if (!relation.predicate) return null;
  const clauseName = `${modelPredicateName}_${toSnakeCase(relation.name)}`;
  if (relation.direction === "reverse") {
    return `${clauseName}(X, Values) :- findall(V, triple(V, '${relation.predicate}', X), Values).`;
  }
  return `${clauseName}(X, Values) :- findall(V, triple(X, '${relation.predicate}', V), Values).`;
}

// ── Public API ───────────────────────────────────────────────────────────────

/**
 * Generate Prolog predicate facts from a model class's decorator metadata.
 *
 * Given a model class decorated with `@Model` (and its `@Flag`, `@Property`,
 * `@HasMany`, `@BelongsToMany` decorators), this function emits a string of
 * Prolog clauses that can be prepended to any `perspective.infer()` call.
 *
 * The generated predicates are:
 * - **Instance recognizer** — `modelName(X)` — matches instances of the model
 * - **Property getters** — `modelName_propName(X, Value)` — one per property
 * - **Relation getters** — `modelName_relName(X, Values)` — one per relation
 *
 * @example
 * ```typescript
 * import { generatePrologFacts } from '@coasys/ad4m';
 *
 * const facts = generatePrologFacts(Poll);
 * const result = await perspective.infer(\`
 *   \${facts}
 *   recent_popular_poll(X) :-
 *     poll(X),
 *     poll_vote_count(X, N), N > 10.
 * \`);
 * ```
 *
 * @param ModelClass - A class decorated with `@Model` that extends `Ad4mModel`
 * @returns A multi-line Prolog string ready for use with `perspective.infer()`
 */
export function generatePrologFacts(ModelClass: typeof Ad4mModel): string {
  const metadata = ModelClass.getModelMetadata();
  const predicateName = toSnakeCase(metadata.className);
  const lines: string[] = [];

  lines.push(`% ${metadata.className} — generated Prolog facts`);

  // Instance recognizer
  const instanceClause = buildInstanceClause(predicateName, metadata);
  if (instanceClause) {
    lines.push("");
    lines.push(`% Instance recognizer`);
    lines.push(instanceClause);
  }

  // Property getters
  const propClauses = Object.values(metadata.properties)
    .map((p) => buildPropertyClause(predicateName, p))
    .filter((c): c is string => c !== null);
  if (propClauses.length > 0) {
    lines.push("");
    lines.push(`% Field getters`);
    lines.push(...propClauses);
  }

  // Relation getters
  const relClauses = Object.values(metadata.relations)
    .map((c) => buildRelationClause(predicateName, c))
    .filter((c): c is string => c !== null);
  if (relClauses.length > 0) {
    lines.push("");
    lines.push(`% Relation getters`);
    lines.push(...relClauses);
  }

  return lines.join("\n");
}
