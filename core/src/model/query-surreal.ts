/**
 * SurrealQL query building utilities for Ad4mModel.
 *
 * Contains the graph-traversal WHERE clause builder, legacy
 * subquery-based clause builders, SELECT-field builders, and
 * the main `buildSurrealQLQuery()` function that produces a
 * complete SurrealQL query string from a `Query` object.
 *
 * All functions are stateless — model metadata and relation metadata
 * are passed in as parameters instead of accessed via `this`.
 *
 * @module
 */

import { escapeSurrealString } from "../utils";
import { formatSurrealValue } from "./surreal-utils";
import { resolveParentPredicate } from "./query-prolog";
import type { RelationMetadataEntry } from "./decorators";
import type {
  Where, Query, ModelMetadata, ParentScope,
} from "./types";

// Re-export formatSurrealValue so callers don't need surreal-utils directly
export { formatSurrealValue };

// ─── Graph Traversal WHERE Clause ────────────────────────────────────────────

/**
 * Builds WHERE clause filters using SurrealDB graph traversal syntax.
 *
 * Translates where conditions into `->link[WHERE ...]` / `<-link[WHERE ...]`
 * filters that SurrealDB can optimize via graph indexes.
 *
 * @param metadata            - Model metadata from `getModelMetadata()`
 * @param allRelationsMetadata - Full relation metadata (needed for belongs-to
 *                               direction detection)
 * @param where               - User-supplied where conditions
 * @returns Combined AND-joined condition string, or empty string
 */
export function buildGraphTraversalWhereClause(
  metadata: ModelMetadata,
  allRelationsMetadata: Record<string, RelationMetadataEntry>,
  where?: Where,
): string {
  if (!where) return '';

  const conditions: string[] = [];

  for (const [propertyName, condition] of Object.entries(where)) {
    const isSpecial = ['id', 'author', 'timestamp'].includes(propertyName);

    if (isSpecial) {
      if (propertyName === 'author' || propertyName === 'timestamp') {
        continue; // Filtered post-query in JS
      }

      const columnName = 'uri';

      if (Array.isArray(condition)) {
        const formattedValues = condition.map(v => formatSurrealValue(v)).join(', ');
        conditions.push(`${columnName} IN [${formattedValues}]`);
      } else if (typeof condition === 'object' && condition !== null) {
        const ops = condition as any;
        if (ops.not !== undefined) {
          if (Array.isArray(ops.not)) {
            const formattedValues = ops.not.map(v => formatSurrealValue(v)).join(', ');
            conditions.push(`${columnName} NOT IN [${formattedValues}]`);
          } else {
            conditions.push(`${columnName} != ${formatSurrealValue(ops.not)}`);
          }
        }
        if (ops.between !== undefined && Array.isArray(ops.between) && ops.between.length === 2) {
          conditions.push(`${columnName} >= ${formatSurrealValue(ops.between[0])} AND ${columnName} <= ${formatSurrealValue(ops.between[1])}`);
        }
        if (ops.gt !== undefined) {
          conditions.push(`${columnName} > ${formatSurrealValue(ops.gt)}`);
        }
        if (ops.gte !== undefined) {
          conditions.push(`${columnName} >= ${formatSurrealValue(ops.gte)}`);
        }
        if (ops.lt !== undefined) {
          conditions.push(`${columnName} < ${formatSurrealValue(ops.lt)}`);
        }
        if (ops.lte !== undefined) {
          conditions.push(`${columnName} <= ${formatSurrealValue(ops.lte)}`);
        }
        if (ops.contains !== undefined) {
          conditions.push(`${columnName} CONTAINS ${formatSurrealValue(ops.contains)}`);
        }
      } else {
        conditions.push(`${columnName} = ${formatSurrealValue(condition)}`);
      }
    } else {
      // Regular properties — check relation metadata first for direction
      const relMeta = allRelationsMetadata[propertyName];
      const isBelongs = relMeta?.kind === 'belongsToOne' || relMeta?.kind === 'belongsToMany';

      if (relMeta) {
        const predicate = escapeSurrealString(relMeta.predicate);

        if (Array.isArray(condition)) {
          const formattedValues = condition.map(v => formatSurrealValue(v)).join(', ');
          if (isBelongs) {
            conditions.push(`count(<-link[WHERE predicate = '${predicate}' AND in.uri IN [${formattedValues}]]) > 0`);
          } else {
            conditions.push(`count(->link[WHERE predicate = '${predicate}' AND out.uri IN [${formattedValues}]]) > 0`);
          }
        } else if (typeof condition === 'object' && condition !== null) {
          const ops = condition as any;
          if (ops.not !== undefined) {
            if (Array.isArray(ops.not)) {
              const formattedValues = ops.not.map(v => formatSurrealValue(v)).join(', ');
              if (isBelongs) {
                conditions.push(`count(<-link[WHERE predicate = '${predicate}' AND in.uri IN [${formattedValues}]]) = 0`);
              } else {
                conditions.push(`count(->link[WHERE predicate = '${predicate}' AND out.uri IN [${formattedValues}]]) = 0`);
              }
            } else {
              if (isBelongs) {
                conditions.push(`count(<-link[WHERE predicate = '${predicate}' AND in.uri = ${formatSurrealValue(ops.not)}]) = 0`);
              } else {
                conditions.push(`count(->link[WHERE predicate = '${predicate}' AND out.uri = ${formatSurrealValue(ops.not)}]) = 0`);
              }
            }
          }
        } else {
          if (isBelongs) {
            conditions.push(`count(<-link[WHERE predicate = '${predicate}' AND in.uri = ${formatSurrealValue(condition)}]) > 0`);
          } else {
            conditions.push(`count(->link[WHERE predicate = '${predicate}' AND out.uri = ${formatSurrealValue(condition)}]) > 0`);
          }
        }
        continue;
      }

      const propMeta = metadata.properties[propertyName];
      if (!propMeta) continue;

      const predicate = escapeSurrealString(propMeta.predicate);
      const targetField = propMeta.resolveLanguage === 'literal' ? 'fn::parse_literal(out.uri)' : 'out.uri';

      if (Array.isArray(condition)) {
        const formattedValues = condition.map(v => formatSurrealValue(v)).join(', ');
        conditions.push(`count(->link[WHERE predicate = '${predicate}' AND ${targetField} IN [${formattedValues}]]) > 0`);
      } else if (typeof condition === 'object' && condition !== null) {
        const ops = condition as any;
        if (ops.not !== undefined) {
          if (Array.isArray(ops.not)) {
            const formattedValues = ops.not.map(v => formatSurrealValue(v)).join(', ');
            conditions.push(`count(->link[WHERE predicate = '${predicate}' AND ${targetField} IN [${formattedValues}]]) = 0`);
          } else {
            conditions.push(`count(->link[WHERE predicate = '${predicate}' AND ${targetField} = ${formatSurrealValue(ops.not)}]) = 0`);
          }
        }
        const hasComparisonOps = ops.gt !== undefined || ops.gte !== undefined ||
                                 ops.lt !== undefined || ops.lte !== undefined ||
                                 ops.between !== undefined || ops.contains !== undefined;
        if (hasComparisonOps) {
          conditions.push(`count(->link[WHERE predicate = '${predicate}']) > 0`);
        }
      } else {
        conditions.push(`count(->link[WHERE predicate = '${predicate}' AND ${targetField} = ${formatSurrealValue(condition)}]) > 0`);
      }
    }
  }

  return conditions.join(' AND ');
}

// ─── Legacy Subquery-based WHERE Clause ──────────────────────────────────────

/**
 * Builds a WHERE clause using subqueries (legacy, kept for reference).
 *
 * @param metadata - Model metadata
 * @param where    - Where conditions
 * @returns WHERE clause fragment (without "WHERE" keyword)
 */
export function buildSurrealWhereClause(metadata: ModelMetadata, where?: Where): string {
  if (!where) return '';
  
  const conditions: string[] = [];
  
  for (const [propertyName, condition] of Object.entries(where)) {
    const isSpecial = ['id', 'author', 'timestamp'].includes(propertyName);
    
    if (isSpecial) {
      if (propertyName === 'author' || propertyName === 'timestamp') {
        continue;
      }
      
      const columnName = 'source';
      
      if (Array.isArray(condition)) {
        const formattedValues = condition.map(v => formatSurrealValue(v)).join(', ');
        conditions.push(`${columnName} IN [${formattedValues}]`);
      } else if (typeof condition === 'object' && condition !== null) {
        const ops = condition as any;
        if (ops.not !== undefined) {
          if (Array.isArray(ops.not)) {
            const formattedValues = ops.not.map(v => formatSurrealValue(v)).join(', ');
            conditions.push(`${columnName} NOT IN [${formattedValues}]`);
          } else {
            conditions.push(`${columnName} != ${formatSurrealValue(ops.not)}`);
          }
        }
        if (ops.between !== undefined && Array.isArray(ops.between) && ops.between.length === 2) {
          conditions.push(`${columnName} >= ${formatSurrealValue(ops.between[0])} AND ${columnName} <= ${formatSurrealValue(ops.between[1])}`);
        }
        if (ops.gt !== undefined) {
          conditions.push(`${columnName} > ${formatSurrealValue(ops.gt)}`);
        }
        if (ops.gte !== undefined) {
          conditions.push(`${columnName} >= ${formatSurrealValue(ops.gte)}`);
        }
        if (ops.lt !== undefined) {
          conditions.push(`${columnName} < ${formatSurrealValue(ops.lt)}`);
        }
        if (ops.lte !== undefined) {
          conditions.push(`${columnName} <= ${formatSurrealValue(ops.lte)}`);
        }
        if (ops.contains !== undefined) {
          conditions.push(`${columnName} CONTAINS ${formatSurrealValue(ops.contains)}`);
        }
      } else {
        conditions.push(`${columnName} = ${formatSurrealValue(condition)}`);
      }
    } else {
      const propMeta = metadata.properties[propertyName];
      if (!propMeta) continue;
      
      const predicate = escapeSurrealString(propMeta.predicate);
      const targetField = propMeta.resolveLanguage === 'literal' ? 'fn::parse_literal(target)' : 'target';
      
      if (Array.isArray(condition)) {
        const formattedValues = condition.map(v => formatSurrealValue(v)).join(', ');
        conditions.push(`source IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} IN [${formattedValues}])`);
      } else if (typeof condition === 'object' && condition !== null) {
        const ops = condition as any;
        if (ops.not !== undefined) {
          if (Array.isArray(ops.not)) {
            const formattedValues = ops.not.map(v => formatSurrealValue(v)).join(', ');
            conditions.push(`source NOT IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} IN [${formattedValues}])`);
          } else {
            conditions.push(`source NOT IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} = ${formatSurrealValue(ops.not)})`);
          }
        }
        const hasComparisonOps = ops.gt !== undefined || ops.gte !== undefined ||
                                 ops.lt !== undefined || ops.lte !== undefined ||
                                 ops.between !== undefined || ops.contains !== undefined;
        if (hasComparisonOps) {
          conditions.push(`source IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}')`);
        }
      } else {
        conditions.push(`source IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} = ${formatSurrealValue(condition)})`);
      }
    }
  }
  
  return conditions.join(' AND ');
}

// ─── SELECT Field Builders ───────────────────────────────────────────────────

/**
 * Builds SELECT fields using subqueries.
 */
export function buildSurrealSelectFields(
  metadata: ModelMetadata,
  properties?: string[],
  relations?: string[],
): string {
  const fields: string[] = [];
  
  const propsToFetch = properties || Object.keys(metadata.properties);
  for (const propName of propsToFetch) {
    const propMeta = metadata.properties[propName];
    if (!propMeta) continue;
    const escapedPredicate = escapeSurrealString(propMeta.predicate);
    fields.push(`(SELECT VALUE target FROM link WHERE source = source AND predicate = '${escapedPredicate}' LIMIT 1) AS ${propName}`);
  }
  
  const relsToFetch = relations || Object.keys(metadata.relations);
  for (const relName of relsToFetch) {
    const relMeta = metadata.relations[relName];
    if (!relMeta) continue;
    const escapedPredicate = escapeSurrealString(relMeta.predicate);
    fields.push(`(SELECT VALUE target FROM link WHERE source = source AND predicate = '${escapedPredicate}') AS ${relName}`);
  }
  
  fields.push(`(SELECT VALUE author FROM link WHERE source = source ORDER BY timestamp ASC LIMIT 1) AS author`);
  fields.push(`(SELECT VALUE timestamp FROM link WHERE source = source ORDER BY timestamp ASC LIMIT 1) AS createdAt`);
  fields.push(`(SELECT VALUE timestamp FROM link WHERE source = source ORDER BY timestamp DESC LIMIT 1) AS updatedAt`);
  
  return fields.join(',\n  ');
}

/**
 * Builds SELECT fields using aggregation functions (for GROUP BY queries).
 */
export function buildSurrealSelectFieldsWithAggregation(
  metadata: ModelMetadata,
  properties?: string[],
  relations?: string[],
): string {
  const fields: string[] = [];
  
  const propsToFetch = properties || Object.keys(metadata.properties);
  for (const propName of propsToFetch) {
    const propMeta = metadata.properties[propName];
    if (!propMeta) continue;
    const escapedPredicate = escapeSurrealString(propMeta.predicate);
    fields.push(`array::first(target[WHERE predicate = '${escapedPredicate}']) AS ${propName}`);
  }
  
  const relsToFetch = relations || Object.keys(metadata.relations);
  for (const relName of relsToFetch) {
    const relMeta = metadata.relations[relName];
    if (!relMeta) continue;
    const escapedPredicate = escapeSurrealString(relMeta.predicate);
    fields.push(`target[WHERE predicate = '${escapedPredicate}'] AS ${relName}`);
  }
  
  fields.push(`array::first(author) AS author`);
  fields.push(`array::first(timestamp) AS createdAt`);
  fields.push(`array::last(timestamp) AS updatedAt`);
  
  return fields.join(',\n  ');
}

// ─── Main Query Builder ─────────────────────────────────────────────────────

/**
 * Builds a complete SurrealQL query from a Query object and model metadata.
 *
 * This is the core implementation behind `Ad4mModel.queryToSurrealQL()`.
 * It produces a graph-traversal query that:
 * 1. Filters nodes by required predicates (conformance check)
 * 2. Applies user WHERE conditions
 * 3. Fetches all outgoing links for hydration
 *
 * @param metadata            - Model metadata from `getModelMetadata()`
 * @param allRelationsMetadata - Full relation registry (for belongs-to detection)
 * @param query               - Query parameters
 * @param modelClass          - The model class (for `resolveParentPredicate`)
 * @returns Complete SurrealQL query string
 */
export function buildSurrealQLQuery(
  metadata: ModelMetadata,
  allRelationsMetadata: Record<string, RelationMetadataEntry>,
  query: Query,
  modelClass: any,
): string {
  const { where } = query;

  // Build graph traversal filters for required predicates
  const graphTraversalFilters: string[] = [];

  // Parent filter
  if (query.parent) {
    const parentPredicate = resolveParentPredicate(query.parent, modelClass);
    graphTraversalFilters.push(
      `count(<-link[WHERE in.uri = ${formatSurrealValue(query.parent.id)} AND predicate = '${escapeSurrealString(parentPredicate)}']) > 0`
    );
  }

  // Required property filters
  for (const [, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.required) {
      if (propMeta.getter) continue;
      if (propMeta.flag && propMeta.initial) {
        graphTraversalFilters.push(
          `count(->link[WHERE predicate = '${escapeSurrealString(propMeta.predicate)}' AND out.uri = '${escapeSurrealString(propMeta.initial)}']) > 0`
        );
      } else {
        graphTraversalFilters.push(
          `count(->link[WHERE predicate = '${escapeSurrealString(propMeta.predicate)}']) > 0`
        );
      }
    }
  }

  // Fallback: initial-value filters
  if (graphTraversalFilters.length === 0) {
    for (const [, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.initial) {
        if (propMeta.flag) {
          graphTraversalFilters.push(
            `count(->link[WHERE predicate = '${escapeSurrealString(propMeta.predicate)}' AND out.uri = '${escapeSurrealString(propMeta.initial)}']) > 0`
          );
        } else {
          graphTraversalFilters.push(
            `count(->link[WHERE predicate = '${escapeSurrealString(propMeta.predicate)}']) > 0`
          );
        }
        break;
      }
    }
  }

  // Fallback: open-world structural matching
  if (graphTraversalFilters.length === 0) {
    const structuralPredicates: string[] = [];
    for (const [, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.predicate) {
        structuralPredicates.push(`predicate = '${escapeSurrealString(propMeta.predicate)}'`);
      }
    }
    if (metadata.relations) {
      for (const [, relMeta] of Object.entries(metadata.relations)) {
        if (relMeta.predicate) {
          structuralPredicates.push(`predicate = '${escapeSurrealString(relMeta.predicate)}'`);
        }
      }
    }
    if (structuralPredicates.length > 0) {
      graphTraversalFilters.push(
        `count(->link[WHERE (${structuralPredicates.join(' OR ')})]) > 0`
      );
    }
  }

  // User WHERE clause
  const userWhereClause = buildGraphTraversalWhereClause(metadata, allRelationsMetadata, where);

  const whereConditions: string[] = [];
  whereConditions.push(...graphTraversalFilters);
  if (userWhereClause) {
    whereConditions.push(userWhereClause);
  }
  whereConditions.push(`count(->link) > 0`);

  const fullQuery = `
SELECT
    id AS source,
    uri AS source_uri,
    ->link AS links
FROM node
WHERE ${whereConditions.join(' AND ')}
FETCH links
    `.trim();

  return fullQuery;
}
