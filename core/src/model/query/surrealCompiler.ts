/**
 * Pure SurrealQL compiler — translates high-level `Query<T>` objects into
 * raw SurrealQL strings.
 *
 * All functions are stateless — they take `ModelMetadata` (and optionally a
 * `Query`) as plain data and return strings or booleans.  No dependency on the
 * `Ad4mModel` class, which means they can be imported by tests and other
 * modules without pulling in the full class graph.
 *
 * `Ad4mModel` keeps thin static wrapper methods with the original signatures so
 * the public API is unchanged.
 */

import { escapeSurrealString } from "../../utils";
import { ModelMetadata, Query, Where, WhereCondition } from "../types";

// ── Value formatting ────────────────────────────────────────────────────────

/**
 * Formats a value for use in SurrealQL queries.
 *
 * - Strings: wrapped in single quotes, special characters escaped
 * - Numbers / booleans: converted to string
 * - Arrays: recursively formatted, wrapped in `[...]`
 */
export function formatSurrealValue(value: any): string {
  if (typeof value === "string") {
    const escaped = value
      .replace(/\\/g, "\\\\")
      .replace(/'/g, "\\'")
      .replace(/"/g, '\\"')
      .replace(/\n/g, "\\n")
      .replace(/\r/g, "\\r")
      .replace(/\t/g, "\\t");
    return `'${escaped}'`;
  } else if (typeof value === "number" || typeof value === "boolean") {
    return String(value);
  } else if (Array.isArray(value)) {
    return `[${value.map(formatSurrealValue).join(", ")}]`;
  } else {
    return String(value);
  }
}

// ── WHERE clause builders ───────────────────────────────────────────────────

/**
 * Builds graph-traversal WHERE clause filters (`->link[WHERE ...]`).
 * Used by `buildSurrealQuery` — more efficient than subqueries because
 * SurrealDB can use graph indexes.
 */
export function buildGraphTraversalWhereClause(
  metadata: ModelMetadata,
  where?: Where,
): string {
  if (!where) return "";

  const conditions: string[] = [];

  for (const [propertyName, condition] of Object.entries(where)) {
    const isSpecial = ["base", "id", "author", "timestamp"].includes(
      propertyName,
    );

    if (isSpecial) {
      if (propertyName === "author" || propertyName === "timestamp") {
        continue; // filtered post-query in JavaScript
      }

      const columnName = "uri"; // base/id → uri in node table

      if (Array.isArray(condition)) {
        const formattedValues = condition.map(formatSurrealValue).join(", ");
        conditions.push(`${columnName} IN [${formattedValues}]`);
      } else if (typeof condition === "object" && condition !== null) {
        const ops = condition as any;
        if (ops.not !== undefined) {
          if (Array.isArray(ops.not)) {
            const formattedValues = ops.not.map(formatSurrealValue).join(", ");
            conditions.push(`${columnName} NOT IN [${formattedValues}]`);
          } else {
            conditions.push(`${columnName} != ${formatSurrealValue(ops.not)}`);
          }
        }
        if (
          ops.between !== undefined &&
          Array.isArray(ops.between) &&
          ops.between.length === 2
        ) {
          conditions.push(
            `${columnName} >= ${formatSurrealValue(ops.between[0])} AND ${columnName} <= ${formatSurrealValue(ops.between[1])}`,
          );
        }
        if (ops.gt !== undefined)
          conditions.push(`${columnName} > ${formatSurrealValue(ops.gt)}`);
        if (ops.gte !== undefined)
          conditions.push(`${columnName} >= ${formatSurrealValue(ops.gte)}`);
        if (ops.lt !== undefined)
          conditions.push(`${columnName} < ${formatSurrealValue(ops.lt)}`);
        if (ops.lte !== undefined)
          conditions.push(`${columnName} <= ${formatSurrealValue(ops.lte)}`);
        if (ops.contains !== undefined)
          conditions.push(
            `${columnName} CONTAINS ${formatSurrealValue(ops.contains)}`,
          );
      } else {
        conditions.push(`${columnName} = ${formatSurrealValue(condition)}`);
      }
    } else {
      const propMeta = metadata.properties[propertyName];
      if (!propMeta) continue;

      const predicate = escapeSurrealString(propMeta.predicate);
      const targetField =
        propMeta.resolveLanguage === "literal"
          ? "fn::parse_literal(out.uri)"
          : "out.uri";

      if (Array.isArray(condition)) {
        const formattedValues = condition.map(formatSurrealValue).join(", ");
        conditions.push(
          `count(->link[WHERE predicate = '${predicate}' AND ${targetField} IN [${formattedValues}]]) > 0`,
        );
      } else if (typeof condition === "object" && condition !== null) {
        const ops = condition as any;
        if (ops.not !== undefined) {
          if (Array.isArray(ops.not)) {
            const formattedValues = ops.not.map(formatSurrealValue).join(", ");
            conditions.push(
              `count(->link[WHERE predicate = '${predicate}' AND ${targetField} IN [${formattedValues}]]) = 0`,
            );
          } else {
            conditions.push(
              `count(->link[WHERE predicate = '${predicate}' AND ${targetField} = ${formatSurrealValue(ops.not)}]) = 0`,
            );
          }
        }
        const hasComparisonOps =
          ops.gt !== undefined ||
          ops.gte !== undefined ||
          ops.lt !== undefined ||
          ops.lte !== undefined ||
          ops.between !== undefined ||
          ops.contains !== undefined;
        if (hasComparisonOps) {
          conditions.push(
            `count(->link[WHERE predicate = '${predicate}']) > 0`,
          );
        }
      } else {
        conditions.push(
          `count(->link[WHERE predicate = '${predicate}' AND ${targetField} = ${formatSurrealValue(condition)}]) > 0`,
        );
      }
    }
  }

  return conditions.join(" AND ");
}

/**
 * Builds a legacy subquery-based WHERE clause.
 * Kept for compatibility; prefer `buildGraphTraversalWhereClause`.
 */
export function buildSurrealWhereClause(
  metadata: ModelMetadata,
  where?: Where,
): string {
  if (!where) return "";

  const conditions: string[] = [];

  for (const [propertyName, condition] of Object.entries(where)) {
    const isSpecial = ["base", "id", "author", "timestamp"].includes(
      propertyName,
    );

    if (isSpecial) {
      if (propertyName === "author" || propertyName === "timestamp") {
        continue;
      }

      const columnName = "source";

      if (Array.isArray(condition)) {
        const formattedValues = condition.map(formatSurrealValue).join(", ");
        conditions.push(`${columnName} IN [${formattedValues}]`);
      } else if (typeof condition === "object" && condition !== null) {
        const ops = condition as any;
        if (ops.not !== undefined) {
          if (Array.isArray(ops.not)) {
            const formattedValues = ops.not.map(formatSurrealValue).join(", ");
            conditions.push(`${columnName} NOT IN [${formattedValues}]`);
          } else {
            conditions.push(`${columnName} != ${formatSurrealValue(ops.not)}`);
          }
        }
        if (
          ops.between !== undefined &&
          Array.isArray(ops.between) &&
          ops.between.length === 2
        ) {
          conditions.push(
            `${columnName} >= ${formatSurrealValue(ops.between[0])} AND ${columnName} <= ${formatSurrealValue(ops.between[1])}`,
          );
        }
        if (ops.gt !== undefined)
          conditions.push(`${columnName} > ${formatSurrealValue(ops.gt)}`);
        if (ops.gte !== undefined)
          conditions.push(`${columnName} >= ${formatSurrealValue(ops.gte)}`);
        if (ops.lt !== undefined)
          conditions.push(`${columnName} < ${formatSurrealValue(ops.lt)}`);
        if (ops.lte !== undefined)
          conditions.push(`${columnName} <= ${formatSurrealValue(ops.lte)}`);
        if (ops.contains !== undefined)
          conditions.push(
            `${columnName} CONTAINS ${formatSurrealValue(ops.contains)}`,
          );
      } else {
        conditions.push(`${columnName} = ${formatSurrealValue(condition)}`);
      }
    } else {
      const propMeta = metadata.properties[propertyName];
      if (!propMeta) continue;

      const predicate = escapeSurrealString(propMeta.predicate);
      const targetField =
        propMeta.resolveLanguage === "literal"
          ? "fn::parse_literal(target)"
          : "target";

      if (Array.isArray(condition)) {
        const formattedValues = condition.map(formatSurrealValue).join(", ");
        conditions.push(
          `source IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} IN [${formattedValues}])`,
        );
      } else if (typeof condition === "object" && condition !== null) {
        const ops = condition as any;
        if (ops.not !== undefined) {
          if (Array.isArray(ops.not)) {
            const formattedValues = ops.not.map(formatSurrealValue).join(", ");
            conditions.push(
              `source NOT IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} IN [${formattedValues}])`,
            );
          } else {
            conditions.push(
              `source NOT IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} = ${formatSurrealValue(ops.not)})`,
            );
          }
        }
        const hasComparisonOps =
          ops.gt !== undefined ||
          ops.gte !== undefined ||
          ops.lt !== undefined ||
          ops.lte !== undefined ||
          ops.between !== undefined ||
          ops.contains !== undefined;
        if (hasComparisonOps) {
          conditions.push(
            `source IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}')`,
          );
        }
      } else {
        conditions.push(
          `source IN (SELECT VALUE source FROM link WHERE predicate = '${predicate}' AND ${targetField} = ${formatSurrealValue(condition)})`,
        );
      }
    }
  }

  return conditions.join(" AND ");
}

// ── SELECT field builders ───────────────────────────────────────────────────

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
    fields.push(
      `(SELECT VALUE target FROM link WHERE source = source AND predicate = '${escapedPredicate}' LIMIT 1) AS ${propName}`,
    );
  }

  const relationsToFetch = relations || Object.keys(metadata.relations);
  for (const relationName of relationsToFetch) {
    const relationMeta = metadata.relations[relationName];
    if (!relationMeta) continue;
    const escapedPredicate = escapeSurrealString(relationMeta.predicate);
    fields.push(
      `(SELECT VALUE target FROM link WHERE source = source AND predicate = '${escapedPredicate}') AS ${relationName}`,
    );
  }

  fields.push(
    `(SELECT VALUE author FROM link WHERE source = source ORDER BY timestamp ASC LIMIT 1) AS author`,
  );
  fields.push(
    `(SELECT VALUE timestamp FROM link WHERE source = source ORDER BY timestamp ASC LIMIT 1) AS createdAt`,
  );
  fields.push(
    `(SELECT VALUE timestamp FROM link WHERE source = source ORDER BY timestamp DESC LIMIT 1) AS updatedAt`,
  );

  return fields.join(",\n  ");
}

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
    fields.push(
      `array::first(target[WHERE predicate = '${escapedPredicate}']) AS ${propName}`,
    );
  }

  const relationsToFetch = relations || Object.keys(metadata.relations);
  for (const relationName of relationsToFetch) {
    const relationMeta = metadata.relations[relationName];
    if (!relationMeta) continue;
    const escapedPredicate = escapeSurrealString(relationMeta.predicate);
    fields.push(
      `target[WHERE predicate = '${escapedPredicate}'] AS ${relationName}`,
    );
  }

  fields.push(`array::first(author) AS author`);
  fields.push(`array::first(timestamp) AS createdAt`);
  fields.push(`array::last(timestamp) AS updatedAt`);

  return fields.join(",\n  ");
}

// ── Main query builders ─────────────────────────────────────────────────────

/**
 * Builds the SurrealQL SELECT query for a given model metadata + query params.
 * This is the extracted, pure-function form of `Ad4mModel.queryToSurrealQL`.
 */
export function buildSurrealQuery(
  metadata: ModelMetadata,
  query: Query,
): string {
  const { where } = query;

  const graphTraversalFilters: string[] = [];
  for (const [, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.required) {
      if (propMeta.flag && propMeta.initial) {
        graphTraversalFilters.push(
          `count(->link[WHERE predicate = '${escapeSurrealString(propMeta.predicate)}' AND out.uri = '${escapeSurrealString(propMeta.initial)}']) > 0`,
        );
      } else {
        graphTraversalFilters.push(
          `count(->link[WHERE predicate = '${escapeSurrealString(propMeta.predicate)}']) > 0`,
        );
      }
    }
  }

  if (graphTraversalFilters.length === 0) {
    for (const [, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.initial) {
        if (propMeta.flag) {
          graphTraversalFilters.push(
            `count(->link[WHERE predicate = '${escapeSurrealString(propMeta.predicate)}' AND out.uri = '${escapeSurrealString(propMeta.initial)}']) > 0`,
          );
        } else {
          graphTraversalFilters.push(
            `count(->link[WHERE predicate = '${escapeSurrealString(propMeta.predicate)}']) > 0`,
          );
        }
        break;
      }
    }
  }

  const userWhereClause = buildGraphTraversalWhereClause(metadata, where);

  const whereConditions: string[] = [
    ...graphTraversalFilters,
    ...(userWhereClause ? [userWhereClause] : []),
    `count(->link) > 0`,
  ];

  return `
SELECT
    id AS source,
    uri AS source_uri,
    (SELECT predicate, out.uri AS target, author, timestamp FROM link WHERE in = $parent.id ORDER BY timestamp ASC) AS links
FROM node
WHERE ${whereConditions.join(" AND ")}
  `.trim();
}

/**
 * Builds a count query (same as `buildSurrealQuery` but without LIMIT/OFFSET).
 */
export function buildSurrealCountQuery(
  metadata: ModelMetadata,
  query: Query,
): string {
  const countQuery = { ...query };
  delete countQuery.limit;
  delete countQuery.offset;
  return buildSurrealQuery(metadata, countQuery);
}

// ── Post-query filtering ────────────────────────────────────────────────────

/**
 * Checks whether a single value satisfies a WhereCondition.
 * Used for post-query JavaScript filtering of operators that SurrealDB
 * cannot evaluate reliably (gt/gte/lt/lte/between/contains on literals).
 */
export function matchesCondition(
  value: any,
  condition: WhereCondition,
): boolean {
  if (Array.isArray(condition)) {
    return (condition as any[]).includes(value);
  }

  if (typeof condition === "object" && condition !== null) {
    const ops = condition as any;

    if (ops.not !== undefined) {
      return Array.isArray(ops.not)
        ? !(ops.not as any[]).includes(value)
        : value !== ops.not;
    }

    if (
      ops.between !== undefined &&
      Array.isArray(ops.between) &&
      ops.between.length === 2
    ) {
      return value >= ops.between[0] && value <= ops.between[1];
    }

    let allMet = true;
    if (ops.gt !== undefined) allMet = allMet && value > ops.gt;
    if (ops.gte !== undefined) allMet = allMet && value >= ops.gte;
    if (ops.lt !== undefined) allMet = allMet && value < ops.lt;
    if (ops.lte !== undefined) allMet = allMet && value <= ops.lte;
    if (ops.contains !== undefined) {
      if (typeof value === "string") {
        allMet = allMet && value.includes(String(ops.contains));
      } else if (Array.isArray(value)) {
        allMet = allMet && value.includes(ops.contains);
      } else {
        allMet = false;
      }
    }
    return allMet;
  }

  return value === condition;
}
