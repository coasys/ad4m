import type { Ad4mModel } from '../Ad4mModel';
import type { ModelMetadata, PropertyMetadata, CollectionMetadata } from '../Ad4mModel';

type ModelClass = typeof Ad4mModel & {
  getModelMetadata(): ModelMetadata;
};

/**
 * Convert a camelCase or PascalCase identifier to snake_case.
 * Examples: "TestPost" -> "test_post", "createdAt" -> "created_at"
 */
function toSnakeCase(str: string): string {
  return str
    .replace(/([A-Z])/g, '_$1')
    .toLowerCase()
    .replace(/^_/, '');
}

/**
 * Build the instance recognizer clause for a model.
 *
 * Uses @Flag properties (fixed-value predicates) as the primary recognition
 * strategy, since they uniquely identify the type in the graph. Falls back to
 * required properties if no flags are present.
 *
 * Examples:
 *   poll(X) :- triple(X, 'flux://entry_type', 'flux://has_poll').
 *   note(X) :- triple(X, 'ad4m://title', _).
 */
function buildInstanceClause(
  predicateName: string,
  metadata: ModelMetadata,
): string | null {
  const props = metadata.properties;

  // Collect flags first — these are the strongest recognizers
  const flags = Object.values(props).filter((p) => p.flag && p.predicate && p.initial);
  if (flags.length > 0) {
    const conditions = flags
      .map((p) => `triple(X, '${p.predicate}', '${p.initial}')`)
      .join(',\n    ');
    return `${predicateName}(X) :-\n    ${conditions}.`;
  }

  // Fallback: required non-flag properties
  const required = Object.values(props).filter((p) => p.required && p.predicate && !p.flag);
  if (required.length > 0) {
    const conditions = required
      .map((p) => `triple(X, '${p.predicate}', _)`)
      .join(',\n    ');
    return `${predicateName}(X) :-\n    ${conditions}.`;
  }

  return null;
}

/**
 * Build a property getter clause for a single property.
 *
 * Example:
 *   poll_title(X, Value) :- triple(X, 'rdf://title', Value).
 */
function buildPropertyClause(
  modelPredicateName: string,
  prop: PropertyMetadata,
): string | null {
  // Flags are handled by the instance clause, not individual getters
  if (prop.flag) return null;
  // No predicate = no clause
  if (!prop.predicate) return null;

  const clauseName = `${modelPredicateName}_${toSnakeCase(prop.name)}`;
  return `${clauseName}(X, Value) :- triple(X, '${prop.predicate}', Value).`;
}

/**
 * Build a collection getter clause.
 *
 * Example:
 *   poll_entries(X, Values) :- findall(V, triple(X, 'flux://entry', V), Values).
 */
function buildCollectionClause(
  modelPredicateName: string,
  coll: CollectionMetadata,
): string | null {
  if (!coll.predicate) return null;

  const clauseName = `${modelPredicateName}_${toSnakeCase(coll.name)}`;
  return `${clauseName}(X, Values) :- findall(V, triple(X, '${coll.predicate}', V), Values).`;
}

/**
 * Generate Prolog predicate facts from a model class's SHACL metadata.
 *
 * Given a model class decorated with `@ModelOptions` (and its `@Flag`,
 * `@Property`, `@Collection` decorators), this function emits a string of
 * Prolog clauses that can be prepended to any `perspective.infer()` call.
 *
 * The generated predicates are:
 * - **Instance recognizer** — `modelName(X)` — matches instances of the model
 * - **Property getters** — `modelName_propName(X, Value)` — one per property
 * - **Collection getters** — `modelName_collName(X, Values)` — one per collection
 *
 * @example
 * ```typescript
 * import { generatePrologFacts } from '@coasys/ad4m/model/prolog';
 *
 * const facts = generatePrologFacts(Poll);
 * const result = await perspective.infer(`
 *   ${facts}
 *   recent_popular_poll(X) :-
 *     poll(X),
 *     poll_vote_count(X, N), N > 10,
 *     poll_created_at(X, T), T > ${yesterday}.
 * `);
 * ```
 *
 * @param ModelClass - A class decorated with `@ModelOptions` that extends `Ad4mModel`
 * @returns A multi-line Prolog string ready for use with `perspective.infer()`
 */
export function generatePrologFacts(ModelClass: ModelClass): string {
  const metadata = ModelClass.getModelMetadata();
  const predicateName = toSnakeCase(metadata.className);
  const lines: string[] = [];

  lines.push(`% ${metadata.className} — generated Prolog facts`);

  // Instance recognizer
  const instanceClause = buildInstanceClause(predicateName, metadata);
  if (instanceClause) {
    lines.push('');
    lines.push(`% Instance recognizer`);
    lines.push(instanceClause);
  }

  // Property getters
  const propClauses = Object.values(metadata.properties)
    .map((p) => buildPropertyClause(predicateName, p))
    .filter((c): c is string => c !== null);

  if (propClauses.length > 0) {
    lines.push('');
    lines.push(`% Property getters`);
    lines.push(...propClauses);
  }

  // Collection getters
  const collClauses = Object.values(metadata.collections)
    .map((c) => buildCollectionClause(predicateName, c))
    .filter((c): c is string => c !== null);

  if (collClauses.length > 0) {
    lines.push('');
    lines.push(`% Collection getters`);
    lines.push(...collClauses);
  }

  return lines.join('\n');
}
