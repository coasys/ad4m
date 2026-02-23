/**
 * Static query operations for Ad4mModel.
 *
 * Extracted from Ad4mModel.ts (Phase 3a Part 3). Every function here is a
 * pure-static helper that takes a model constructor (`ctor`) as its first
 * argument instead of relying on `this`. Ad4mModel wraps each one with a
 * thin static method so the public API is completely unchanged.
 *
 * The `Ad4mModelCtor<T>` type is imported (type-only) from QueryBuilder so
 * there is no runtime circular dependency.
 */

import type { PerspectiveProxy } from "../../perspectives/PerspectiveProxy";
import type { Query, ResultsWithTotalCount, PaginationResult } from "../types";
import type { Ad4mModelCtor } from "./ModelQueryBuilder";
import {
  buildSurrealQuery,
  buildSurrealCountQuery,
  matchesCondition,
} from "./surrealCompiler";
import { hydrateInstanceFromLinks, evaluateCustomGetters } from "./hydration";
import { escapeSurrealString } from "../../utils";

// ─────────────────────────────────────────────────────────────────────────────
// Query-to-SurrealQL helpers
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Translates a high-level {@link Query} object to a SurrealQL string.
 *
 * @param ctor - The Ad4mModel subclass (provides getModelMetadata)
 * @param perspective - Not used for query building but kept for API symmetry
 * @param query - High-level query parameters
 */
export async function queryToSurrealQL(
  ctor: Ad4mModelCtor<any>,
  _perspective: PerspectiveProxy,
  query: Query,
): Promise<string> {
  return buildSurrealQuery((ctor as any).getModelMetadata(), query);
}

/**
 * Translates a high-level {@link Query} object to a SurrealQL COUNT string.
 */
export async function countQueryToSurrealQL(
  ctor: Ad4mModelCtor<any>,
  _perspective: PerspectiveProxy,
  query: Query,
): Promise<string> {
  return buildSurrealCountQuery((ctor as any).getModelMetadata(), query);
}

// ─────────────────────────────────────────────────────────────────────────────
// instancesFromSurrealResult — the core hydration/assembly pipeline
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Converts raw SurrealDB result rows to hydrated Ad4mModel instances.
 *
 * This is the single-pass pipeline that:
 *  1. Creates a model instance for each `source_uri` row
 *  2. Hydrates properties, forward relations, author and timestamps
 *  3. Fetches reverse-relation links in one batch query
 *  4. Batch-hydrates nested `relatedModel` relations (no N+1)
 *  5. Evaluates custom SurrealQL getters
 *  6. Applies `where.isInstance` filtering on relations
 *  7. Post-filters and sorts in JS for operators that SurrealDB can't handle
 *  8. Applies offset/limit pagination
 *
 * @internal
 */
export async function instancesFromSurrealResult<T>(
  ctor: Ad4mModelCtor<T>,
  perspective: PerspectiveProxy,
  query: Query,
  result: any[],
  _hydrateRelations = true,
): Promise<ResultsWithTotalCount<T>> {
  if (!result || result.length === 0) return { results: [], totalCount: 0 };

  const metadata = (ctor as any).getModelMetadata();
  const requestedProperties = query?.properties || [];

  // ── 1. Build instances from rows ──────────────────────────────────────────────────
  const instances: T[] = [];
  for (const row of result) {
    try {
      const base = row.source_uri;
      if (!base) continue;

      const links: any[] = row.links || [];
      const instance = new ctor(perspective, base) as any;

      // Shared hydration: properties + forward relations + author + timestamps
      await hydrateInstanceFromLinks(instance, links, metadata, perspective);

      // If the query asked for specific properties only, strip everything else
      if (requestedProperties.length > 0) {
        Object.keys(instance).forEach((key) => {
          if (
            !requestedProperties.includes(key) &&
            key !== "createdAt" &&
            key !== "updatedAt" &&
            key !== "author" &&
            key !== "id"
          ) {
            delete instance[key];
          }
        });
      }

      instances.push(instance);
    } catch (error) {
      console.error(
        `Failed to process SurrealDB instance ${(error as any)?.base ?? "unknown"}:`,
        error,
      );
    }
  }

  // ── 2. Reverse relations (one batch query for all instances) ──────────────
  // Forward links (->link) are in row.links; reverse links (<-link) are not.
  const reverseRelationEntries = Object.entries(metadata.relations).filter(
    ([, m]: [string, any]) => !m.getter && m.direction === "reverse",
  );
  if (reverseRelationEntries.length > 0 && instances.length > 0) {
    try {
      const inList = instances
        .map((i: any) => `'${escapeSurrealString(i.id)}'`)
        .join(", ");
      const reverseLinksQuery = `
        SELECT in.uri AS source, predicate, out.uri AS target, author, timestamp
        FROM link
        WHERE out.uri IN [${inList}]
        ORDER BY timestamp ASC
      `;
      const reverseLinks: any[] =
        (await perspective.querySurrealDB(reverseLinksQuery)) ?? [];

      for (const instance of instances) {
        for (const [relationName, relationMeta] of reverseRelationEntries) {
          const matching = reverseLinks.filter(
            (l: any) =>
              l.target === (instance as any).id &&
              l.predicate === (relationMeta as any).predicate,
          );
          const values = matching.map((l: any) => l.source);
          (instance as any)[relationName] =
            (relationMeta as any).maxCount === 1 ? values[0] ?? null : values;
        }
      }
    } catch (e) {
      console.warn("Failed to fetch reverse links for instances:", e);
    }
  }

  // ── 3. Batch-hydrate relations via explicit `include` map ─────────────────
  //
  // No hydration is performed unless `query.include` is set.
  // Each key in the map is a relation field name; the value is either `true`
  // (hydrate all with no filter) or a Query to filter/order/limit the nested set.
  if (_hydrateRelations && query?.include) {
    const includeMap = query.include;
    const hydrateEntries = Object.entries(metadata.relations).filter(
      ([name, m]: [string, any]) =>
        !m.getter &&
        name in includeMap &&
        !!(m.relatedModel || m.where?.isInstance),
    );
    for (const [relationName, relationMeta] of hydrateEntries) {
      const allIds = Array.from(
        new Set(
          instances.flatMap((i: any) => {
            const val = i[relationName];
            if (!val) return [];
            return Array.isArray(val) ? val.filter(Boolean) : [val];
          }),
        ),
      ) as string[];
      if (allIds.length === 0) continue;
      try {
        // Resolve the model class from relatedModel factory or where.isInstance.
        const RelatedModel = (relationMeta as any).relatedModel
          ? (relationMeta as any).relatedModel()
          : (relationMeta as any).where?.isInstance;
        // Merge caller's sub-query with the id pre-filter.
        const entry = includeMap[relationName];
        const subQuery: Query =
          entry === true
            ? { where: { id: allIds } }
            : { ...entry, where: { id: allIds, ...(entry as Query).where } };
        const allHydrated = await _findAllInternal(
          RelatedModel,
          perspective,
          subQuery,
          false, // depth guard — no recursive nested-model hydration
        );
        const hydratedMap = new Map<string, any>(
          allHydrated.map((h: any) => [h.id, h]),
        );
        for (const instance of instances) {
          const val = (instance as any)[relationName];
          if (!val) continue;
          if (Array.isArray(val)) {
            (instance as any)[relationName] = val
              .map((id: string) => hydratedMap.get(id))
              .filter((h: any) => h !== undefined);
          } else if (typeof val === "string") {
            (instance as any)[relationName] = hydratedMap.get(val) ?? null;
          }
        }
      } catch (e) {
        console.warn(`Failed to batch-hydrate ${relationName}:`, e);
      }
    }
  }

  // ── 4. Custom SurrealQL getters (single pass, all instances) ─────────────
  for (const instance of instances) {
    await evaluateCustomGetters(instance as any, perspective, metadata);
  }

  // ── 5. where.isInstance filtering on relations ────────────────────────────
  for (const instance of instances) {
    for (const [relationName, relationMeta] of Object.entries(
      metadata.relations,
    )) {
      if (
        (relationMeta as any).where?.isInstance &&
        (instance as any)[relationName]?.length > 0
      ) {
        try {
          const targetClass = (relationMeta as any).where.isInstance;
          const subjects = (instance as any)[relationName];
          const targetClassName =
            typeof targetClass === "string"
              ? targetClass
              : (targetClass as any).prototype?.className || targetClass.name;
          const classMetadata =
            await perspective.getSubjectClassMetadataFromSDNA(targetClassName);

          if (!classMetadata) continue;

          const validSubjects = await perspective.batchCheckSubjectInstances(
            subjects,
            classMetadata,
          );
          (instance as any)[relationName] = validSubjects;
        } catch {
          // On error, leave the relation unfiltered rather than breaking everything
        }
      }
    }
  }

  // ── 6. Post-filter: where conditions that SurrealDB can't handle in SQL ───
  //    • author / timestamp (computed from grouped links)
  //    • Comparison operators: gt, gte, lt, lte, between, contains
  let filteredInstances = instances;
  if (query.where) {
    filteredInstances = instances.filter((instance) => {
      for (const [propertyName, condition] of Object.entries(query.where!)) {
        // base/id filtering is already done in SQL
        if (propertyName === "base" || propertyName === "id") continue;

        // author and timestamp: always filter in JS
        if (propertyName === "author" || propertyName === "timestamp") {
          if (!matchesCondition((instance as any)[propertyName], condition)) {
            return false;
          }
          continue;
        }

        // Comparison operators: only these need JS post-filtering
        if (
          typeof condition === "object" &&
          condition !== null &&
          !Array.isArray(condition)
        ) {
          const cond = condition as any;
          const hasComparisonOps =
            cond.gt !== undefined ||
            cond.gte !== undefined ||
            cond.lt !== undefined ||
            cond.lte !== undefined ||
            cond.between !== undefined ||
            cond.contains !== undefined;
          if (hasComparisonOps) {
            if (!matchesCondition((instance as any)[propertyName], condition)) {
              return false;
            }
          }
        }
      }
      return true;
    });
  }

  // ── 7. Sort in JavaScript ─────────────────────────────────────────────────
  // If limit/offset is used but no explicit order, default to timestamp ASC
  // to guarantee consistent pagination behaviour.
  const effectiveOrder =
    query.order ||
    (query.limit !== undefined || query.offset !== undefined
      ? { timestamp: "ASC" as "ASC" }
      : null);

  if (effectiveOrder) {
    const orderPropName = Object.keys(effectiveOrder)[0];
    const orderDirection = Object.values(effectiveOrder)[0];

    filteredInstances.sort((a: any, b: any) => {
      let aVal = a[orderPropName];
      let bVal = b[orderPropName];

      // Push undefined values to the end
      if (aVal === undefined && bVal === undefined) return 0;
      if (aVal === undefined) return orderDirection === "ASC" ? 1 : -1;
      if (bVal === undefined) return orderDirection === "ASC" ? -1 : 1;

      let comparison = 0;
      if (typeof aVal === "number" && typeof bVal === "number") {
        comparison = aVal - bVal;
      } else if (typeof aVal === "string" && typeof bVal === "string") {
        comparison = aVal.localeCompare(bVal);
      } else {
        comparison = String(aVal).localeCompare(String(bVal));
      }

      return orderDirection === "DESC" ? -comparison : comparison;
    });
  }

  // ── 8. Calculate totalCount BEFORE limit/offset, then paginate ────────────
  const totalCount = filteredInstances.length;
  let paginatedInstances = filteredInstances;
  if (query.offset !== undefined || query.limit !== undefined) {
    const start = query.offset || 0;
    const end = query.limit ? start + query.limit : undefined;
    paginatedInstances = filteredInstances.slice(start, end);
  }

  return { results: paginatedInstances, totalCount };
}

// ─────────────────────────────────────────────────────────────────────────────
// High-level find/count operations
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Internal findAll used by the public static and by eager-hydration depth-guards.
 * Pass `_hydrateRelations = false` to prevent recursive nested-model hydration.
 */
export async function _findAllInternal<T>(
  ctor: Ad4mModelCtor<T>,
  perspective: PerspectiveProxy,
  query: Query = {},
  _hydrateRelations = true,
): Promise<T[]> {
  const surrealQuery = await queryToSurrealQL(ctor, perspective, query);
  const result = await perspective.querySurrealDB(surrealQuery);
  const { results } = await instancesFromSurrealResult(
    ctor,
    perspective,
    query,
    result,
    _hydrateRelations,
  );
  return results;
}

/** Returns all matching instances. */
export async function findAll<T>(
  ctor: Ad4mModelCtor<T>,
  perspective: PerspectiveProxy,
  query: Query,
): Promise<T[]> {
  return _findAllInternal(ctor, perspective, query, true);
}

/** Returns the first matching instance, or `null` if none found. */
export async function findOne<T>(
  ctor: Ad4mModelCtor<T>,
  perspective: PerspectiveProxy,
  query: Query,
): Promise<T | null> {
  const results = await findAll(ctor, perspective, { ...query, limit: 1 });
  return results[0] ?? null;
}

/** Returns all matching instances together with the total unfilterd count. */
export async function findAllAndCount<T>(
  ctor: Ad4mModelCtor<T>,
  perspective: PerspectiveProxy,
  query: Query,
): Promise<ResultsWithTotalCount<T>> {
  const surrealQuery = await queryToSurrealQL(ctor, perspective, query);
  const result = await perspective.querySurrealDB(surrealQuery);
  return instancesFromSurrealResult(ctor, perspective, query, result);
}

/**
 * Paginates results given an explicit page size and 1-based page number.
 * Returns metadata needed to render pagination controls.
 */
export async function paginate<T>(
  ctor: Ad4mModelCtor<T>,
  perspective: PerspectiveProxy,
  pageSize: number,
  pageNumber: number,
  query: Query,
): Promise<PaginationResult<T>> {
  const paginationQuery: Query = {
    ...query,
    limit: pageSize,
    offset: pageSize * (pageNumber - 1),
    count: true,
  };
  const surrealQuery = await queryToSurrealQL(
    ctor,
    perspective,
    paginationQuery,
  );
  const result = await perspective.querySurrealDB(surrealQuery);
  const { results, totalCount } = await instancesFromSurrealResult(
    ctor,
    perspective,
    paginationQuery,
    result,
  );
  return { results, totalCount, pageSize, pageNumber };
}

/** Returns a count of all matching instances. */
export async function count(
  ctor: Ad4mModelCtor<any>,
  perspective: PerspectiveProxy,
  query: Query,
): Promise<number> {
  const surrealQuery = await queryToSurrealQL(ctor, perspective, query);
  const result = await perspective.querySurrealDB(surrealQuery);
  const { totalCount } = await instancesFromSurrealResult(
    ctor,
    perspective,
    query,
    result,
  );
  return totalCount;
}
