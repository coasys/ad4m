/**
 * SPARQL query building utilities for Ad4mModel.
 *
 * Replaces SurrealQL query generation with SPARQL queries against the
 * Oxigraph RDF store. The SPARQL store represents each link as a set of
 * triples with the `ad4m://ontology/` namespace.
 *
 * The main function `buildSPARQLQuery()` produces a SPARQL SELECT that
 * returns flat link rows (source, predicate, target, author, timestamp).
 * Grouping by source (instance) and hydration are handled in JS.
 *
 * @module
 */

import { resolveParentPredicate } from "./query-common";
import type { RelationMetadataEntry } from "./decorators";
import type { Where, Query, ModelMetadata } from "./types";

const ONT = "ad4m://ontology/";

/**
 * Escape a string for use as a SPARQL string literal.
 */
function escapeSPARQL(value: string): string {
  return value
    .replace(/\\/g, "\\\\")
    .replace(/"/g, '\\"')
    .replace(/\n/g, "\\n")
    .replace(/\r/g, "\\r")
    .replace(/\t/g, "\\t");
}

/**
 * Format a value as a SPARQL literal (always a double-quoted string).
 */
export function formatSPARQLValue(value: any): string {
  if (typeof value === "string") {
    return `"${escapeSPARQL(value)}"`;
  }
  return `"${String(value)}"`;
}

/**
 * Build a SPARQL query that returns all links belonging to instances of
 * the given model class, optionally filtered by `where` conditions.
 *
 * Returns flat rows: `?source ?predicate ?target ?author ?timestamp`
 * where `?source` is the base expression (instance URI).
 *
 * The caller groups rows by `?source` and hydrates model instances.
 */
export function buildSPARQLQuery(
  metadata: ModelMetadata,
  allRelationsMetadata: Record<string, RelationMetadataEntry>,
  query: Query,
  modelClass: any,
): string {
  // Step 1: Build conformance JOIN patterns — triple patterns that ensure
  // a source node has the required links to be considered an instance
  // of this model class. Using JOINs instead of FILTER EXISTS for
  // dramatically better performance in Oxigraph (~1000x faster).
  const joinPatterns: string[] = [];
  const filterExpressions: string[] = [];

  // Parent filter (JOIN pattern)
  if (query.parent) {
    const parentPredicate = resolveParentPredicate(query.parent, modelClass);
    joinPatterns.push(`
      ?cf_parent a <${ONT}Link> ;
        <${ONT}source> ${formatSPARQLValue(query.parent.id)} ;
        <${ONT}predicate> ${formatSPARQLValue(parentPredicate)} ;
        <${ONT}target> ?source .`);
  }

  // Required property JOIN patterns
  let hasConformance = false;
  for (const [, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.required) {
      if (propMeta.getter) continue;
      hasConformance = true;
      if (propMeta.flag && propMeta.initial) {
        joinPatterns.push(`
      ?cf_${propMeta.name} a <${ONT}Link> ;
        <${ONT}source> ?source ;
        <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
        <${ONT}target> ${formatSPARQLValue(propMeta.initial)} .`);
      } else {
        joinPatterns.push(`
      ?cf_${propMeta.name} a <${ONT}Link> ;
        <${ONT}source> ?source ;
        <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
        <${ONT}target> ?cfTarget_${propMeta.name} .`);
      }
    }
  }

  // Fallback: initial-value JOIN patterns
  if (!hasConformance) {
    for (const [, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.initial) {
        hasConformance = true;
        if (propMeta.flag) {
          joinPatterns.push(`
      ?cf_init_${propMeta.name} a <${ONT}Link> ;
        <${ONT}source> ?source ;
        <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
        <${ONT}target> ${formatSPARQLValue(propMeta.initial)} .`);
        } else {
          joinPatterns.push(`
      ?cf_init_${propMeta.name} a <${ONT}Link> ;
        <${ONT}source> ?source ;
        <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
        <${ONT}target> ?cfInitTarget_${propMeta.name} .`);
        }
        break;
      }
    }
  }

  // Fallback: open-world structural matching — at least one known predicate
  if (!hasConformance && joinPatterns.length === 0) {
    const knownPredicates: string[] = [];
    for (const [, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.predicate) {
        knownPredicates.push(formatSPARQLValue(propMeta.predicate));
      }
    }
    if (metadata.relations) {
      for (const [, relMeta] of Object.entries(metadata.relations)) {
        if (relMeta.predicate) {
          knownPredicates.push(formatSPARQLValue(relMeta.predicate));
        }
      }
    }
    if (knownPredicates.length > 0) {
      joinPatterns.push(`
      ?cf_struct a <${ONT}Link> ;
        <${ONT}source> ?source ;
        <${ONT}predicate> ?cf_structPred .`);
      filterExpressions.push(`?cf_structPred IN (${knownPredicates.join(", ")})`);
    }
  }

  // Step 2: Build WHERE clause filters from user query
  const { joins: userJoins, filters: userFilters } = buildSPARQLWhereFilters(metadata, allRelationsMetadata, query.where);
  joinPatterns.push(...userJoins);
  filterExpressions.push(...userFilters);

  // Step 3: Assemble the full query
  const joinClause = joinPatterns.join("\n");
  const filterClause = filterExpressions.length > 0
    ? `FILTER(\n      ${filterExpressions.join(" &&\n      ")}\n    )`
    : "";

  return `
    PREFIX ad4m: <${ONT}>
    SELECT ?source ?predicate ?target ?author ?timestamp WHERE {${joinClause}
      ?link a ad4m:Link ;
            ad4m:source ?source ;
            ad4m:predicate ?predicate ;
            ad4m:target ?target ;
            ad4m:author ?author ;
            ad4m:timestamp ?timestamp .
      ${filterClause}
    }
    ${buildSPARQLOrderLimitOffset(metadata, query)}
  `.trim();
}

/**
 * Build ORDER BY / LIMIT / OFFSET clauses for the SPARQL query.
 *
 * Since the query returns flat link rows (multiple per instance),
 * we can't directly LIMIT/OFFSET. Instead we add ordering hints
 * that the JS-level post-processing uses. True server-side limiting
 * requires a subquery approach.
 *
 * For now, we generate no-op — the JS layer handles ordering/pagination.
 * This function is a placeholder for future server-side optimization.
 */
function buildSPARQLOrderLimitOffset(_metadata: ModelMetadata, _query: Query): string {
  // ORDER BY, LIMIT, OFFSET are handled in JS post-processing
  // because each instance spans multiple flat link rows.
  // A future optimization could use SPARQL subqueries to pre-filter sources.
  return "";
}

/**
 * Build a SPARQL query to fetch all links for a single instance.
 */
export function buildSPARQLGetDataQuery(baseExpression: string): string {
  return `
    PREFIX ad4m: <${ONT}>
    SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
      ?link a ad4m:Link ;
            ad4m:source ${formatSPARQLValue(baseExpression)} ;
            ad4m:predicate ?predicate ;
            ad4m:target ?target ;
            ad4m:author ?author ;
            ad4m:timestamp ?timestamp .
      BIND(${formatSPARQLValue(baseExpression)} AS ?source)
    }
  `.trim();
}

/**
 * Group flat SPARQL link rows into the same shape that SurrealDB returned:
 * `{ source_uri: string, links: Array<{predicate, target, author, timestamp}> }`
 *
 * This allows reuse of the existing `instancesFromSurrealResult` logic.
 */
export function groupSPARQLResults(
  rows: Array<{ source: string; predicate: string; target: string; author: string; timestamp: string }>
): Array<{ source_uri: string; links: Array<{ predicate: string; target: string; author: string; timestamp: string }> }> {
  const grouped = new Map<string, { predicate: string; target: string; author: string; timestamp: string }[]>();
  
  for (const row of rows) {
    const key = row.source;
    if (!grouped.has(key)) {
      grouped.set(key, []);
    }
    grouped.get(key)!.push({
      predicate: row.predicate,
      target: row.target,
      author: row.author,
      timestamp: row.timestamp,
    });
  }

  return Array.from(grouped.entries()).map(([source_uri, links]) => ({
    source_uri,
    links,
  }));
}

/**
 * Build SPARQL FILTER expressions from user `where` conditions.
 *
 * These are returned as individual filter expression strings to be
 * combined with `&&` inside a single FILTER() block.
 */
function buildSPARQLWhereFilters(
  metadata: ModelMetadata,
  _allRelationsMetadata: Record<string, RelationMetadataEntry>,
  where?: Where,
): { joins: string[]; filters: string[] } {
  if (!where) return { joins: [], filters: [] };

  const joins: string[] = [];
  const filters: string[] = [];

  for (const [propertyName, condition] of Object.entries(where)) {
    // 'base' maps to ?source
    if (propertyName === "base" || propertyName === "id") {
      if (Array.isArray(condition)) {
        const formatted = (condition as any[]).map(v => formatSPARQLValue(v)).join(", ");
        filters.push(`?source IN (${formatted})`);
      } else if (typeof condition === "object" && condition !== null) {
        const ops = condition as any;
        if (ops.not !== undefined) {
          if (Array.isArray(ops.not)) {
            const formatted = (ops.not as any[]).map(v => formatSPARQLValue(v)).join(", ");
            filters.push(`!(?source IN (${formatted}))`);
          } else {
            filters.push(`?source != ${formatSPARQLValue(ops.not)}`);
          }
        }
      } else {
        filters.push(`?source = ${formatSPARQLValue(condition)}`);
      }
      continue;
    }

    // author/timestamp: handled in JS post-processing (values are computed per-instance)
    if (propertyName === "author" || propertyName === "timestamp") {
      continue;
    }

    // Property filters
    const propMeta = metadata.properties[propertyName];
    if (!propMeta) continue;

    if (Array.isArray(condition)) {
      // IN clause — needs FILTER inside a JOIN
      const formatted = (condition as any[]).map(v => formatSPARQLValue(v)).join(", ");
      if (propMeta.resolveLanguage === "literal") {
        joins.push(`
      ?w_${propertyName} a <${ONT}Link> ;
        <${ONT}source> ?source ;
        <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
        <${ONT}target> ?wTarget_${propertyName} .`);
        filters.push(`<ad4m://fn/parse_literal>(?wTarget_${propertyName}) IN (${formatted})`);
      } else {
        joins.push(`
      ?w_${propertyName} a <${ONT}Link> ;
        <${ONT}source> ?source ;
        <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
        <${ONT}target> ?wTarget_${propertyName} .`);
        filters.push(`?wTarget_${propertyName} IN (${formatted})`);
      }
    } else if (typeof condition === "object" && condition !== null) {
      const ops = condition as any;
      if (ops.not !== undefined) {
        // NOT clause — must keep FILTER NOT EXISTS (no JOIN equivalent for negation)
        if (Array.isArray(ops.not)) {
          const formatted = (ops.not as any[]).map(v => formatSPARQLValue(v)).join(", ");
          if (propMeta.resolveLanguage === "literal") {
            filters.push(`
          NOT EXISTS {
            ?wLink_not_${propertyName} a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?wTarget_not_${propertyName} .
            FILTER(<ad4m://fn/parse_literal>(?wTarget_not_${propertyName}) IN (${formatted}))
          }
        `);
          } else {
            filters.push(`
          NOT EXISTS {
            ?wLink_not_${propertyName} a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?wTarget_not_${propertyName} .
            FILTER(?wTarget_not_${propertyName} IN (${formatted}))
          }
        `);
          }
        } else {
          const formatted = formatSPARQLValue(ops.not);
          if (propMeta.resolveLanguage === "literal") {
            filters.push(`
          NOT EXISTS {
            ?wLink_not_${propertyName} a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?wTarget_not_${propertyName} .
            FILTER(<ad4m://fn/parse_literal>(?wTarget_not_${propertyName}) = ${formatted})
          }
        `);
          } else {
            filters.push(`
          NOT EXISTS {
            ?wLink_not_${propertyName} a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ${formatted} .
          }
        `);
          }
        }
      }
      // Comparison operators: gt, gte, lt, lte, between, contains
      const targetVar = `?wTarget_cmp_${propertyName}`;
      const valueExpr = propMeta.resolveLanguage === "literal"
        ? `<ad4m://fn/parse_literal>(${targetVar})`
        : targetVar;

      const compFilters: string[] = [];
      if (ops.gt !== undefined) compFilters.push(`${valueExpr} > ${formatSPARQLValue(ops.gt)}`);
      if (ops.gte !== undefined) compFilters.push(`${valueExpr} >= ${formatSPARQLValue(ops.gte)}`);
      if (ops.lt !== undefined) compFilters.push(`${valueExpr} < ${formatSPARQLValue(ops.lt)}`);
      if (ops.lte !== undefined) compFilters.push(`${valueExpr} <= ${formatSPARQLValue(ops.lte)}`);
      if (ops.between !== undefined && Array.isArray(ops.between) && ops.between.length === 2) {
        compFilters.push(`${valueExpr} >= ${formatSPARQLValue(ops.between[0])} && ${valueExpr} <= ${formatSPARQLValue(ops.between[1])}`);
      }
      if (ops.contains !== undefined) {
        compFilters.push(`CONTAINS(STR(${valueExpr}), ${formatSPARQLValue(ops.contains)})`);
      }

      if (compFilters.length > 0) {
        joins.push(`
      ?w_cmp_${propertyName} a <${ONT}Link> ;
        <${ONT}source> ?source ;
        <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
        <${ONT}target> ${targetVar} .`);
        filters.push(...compFilters);
      }
    } else {
      // Simple equality — JOIN with exact target value
      const formatted = formatSPARQLValue(condition);
      if (propMeta.resolveLanguage === "literal") {
        joins.push(`
      ?w_${propertyName} a <${ONT}Link> ;
        <${ONT}source> ?source ;
        <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
        <${ONT}target> ?wTarget_${propertyName} .`);
        filters.push(`<ad4m://fn/parse_literal>(?wTarget_${propertyName}) = ${formatted}`);
      } else {
        joins.push(`
      ?w_${propertyName} a <${ONT}Link> ;
        <${ONT}source> ?source ;
        <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
        <${ONT}target> ${formatted} .`);
      }
    }
  }

  return { joins, filters };
}
