/**
 * SPARQL query building utilities for Ad4mModel.
 *
 * Uses the direct triple + RDF-star storage model where each AD4M link
 * is stored as: <source> <predicate> <target> with RDF-star annotations
 * for metadata (author, timestamp, proof, status).
 *
 * All AD4M URIs (source, predicate, target) become RDF IRIs.
 *
 * @module
 */

import { resolveParentPredicate } from "./query-common";
import type { RelationMetadataEntry } from "./decorators";
import type { Where, Query, ModelMetadata } from "./types";

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
 * Format a value as a SPARQL string literal (double-quoted).
 */
export function formatSPARQLValue(value: any): string {
  if (typeof value === "string") {
    return `"${escapeSPARQL(value)}"`;
  }
  return `"${String(value)}"`;
}

/**
 * Format an AD4M URI as an RDF IRI for use in SPARQL triple patterns.
 * All AD4M link source/predicate/target values become IRIs in angle brackets.
 * The Rust SPARQL service transparently transforms these to valid IRI format.
 */
function iri(value: string): string {
  return `<${value}>`;
}

/**
 * Build a SPARQL query that returns all links belonging to instances of
 * the given model class, optionally filtered by `where` conditions.
 *
 * Uses direct triple patterns: ?source ?predicate ?target
 * with RDF-star annotations for metadata.
 */
export function buildSPARQLQuery(
  metadata: ModelMetadata,
  allRelationsMetadata: Record<string, RelationMetadataEntry>,
  query: Query,
  modelClass: any,
): string {
  const joinPatterns: string[] = [];
  const filterExpressions: string[] = [];

  // Parent filter — direct triple pattern
  if (query.parent) {
    const parentPredicate = resolveParentPredicate(query.parent, modelClass);
    joinPatterns.push(`
      ${iri(query.parent.id)} ${iri(parentPredicate)} ?source .`);
  }

  // Required property JOIN patterns — direct triple patterns
  let hasConformance = false;
  for (const [, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.required) {
      if (propMeta.getter) continue;
      hasConformance = true;
      if (propMeta.flag && propMeta.initial) {
        joinPatterns.push(`
      ?source ${iri(propMeta.predicate)} ${iri(propMeta.initial)} .`);
      } else {
        joinPatterns.push(`
      ?source ${iri(propMeta.predicate)} ?cfTarget_${propMeta.name} .`);
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
      ?source ${iri(propMeta.predicate)} ${iri(propMeta.initial)} .`);
        } else {
          joinPatterns.push(`
      ?source ${iri(propMeta.predicate)} ?cfInitTarget_${propMeta.name} .`);
        }
        break;
      }
    }
  }

  // Fallback: open-world structural matching
  if (!hasConformance && joinPatterns.length === 0) {
    const knownPredicates: string[] = [];
    for (const [, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.predicate) {
        knownPredicates.push(iri(propMeta.predicate));
      }
    }
    if (metadata.relations) {
      for (const [, relMeta] of Object.entries(metadata.relations)) {
        if (relMeta.predicate) {
          knownPredicates.push(iri(relMeta.predicate));
        }
      }
    }
    if (knownPredicates.length > 0) {
      joinPatterns.push(`
      ?source ?cf_structPred ?cf_structTarget .`);
      filterExpressions.push(`?cf_structPred IN (${knownPredicates.join(", ")})`);
    }
  }

  // Build WHERE clause filters from user query
  const { joins: userJoins, filters: userFilters } = buildSPARQLWhereFilters(metadata, allRelationsMetadata, query.where);
  joinPatterns.push(...userJoins);
  filterExpressions.push(...userFilters);

  // Main triple pattern — fetches all links for matched sources
  // The direct triple pattern: ?source ?predicate ?target
  // FILTER(isIRI(?source)) excludes RDF-star annotation triples
  const joinClause = joinPatterns.join("\n");
  const filterClause = filterExpressions.length > 0
    ? `FILTER(\n      ${filterExpressions.join(" &&\n      ")}\n    )`
    : "";

  return `
    SELECT ?source ?predicate ?target ?author ?timestamp WHERE {${joinClause}
      ?source ?predicate ?target .
      FILTER(isIRI(?source) && isIRI(?predicate))
      BIND(<< ?source ?predicate ?target >> AS ?ann)
      ?ann <ad4m://ontology/author> ?author .
      ?ann <ad4m://ontology/timestamp> ?timestamp .
      ${filterClause}
    }
    ${buildSPARQLOrderLimitOffset(metadata, query)}
  `.trim();
}

/**
 * Build ORDER BY / LIMIT / OFFSET clauses for the SPARQL query.
 */
function buildSPARQLOrderLimitOffset(_metadata: ModelMetadata, _query: Query): string {
  return "";
}

/**
 * Build a SPARQL query to fetch all links for a single instance.
 */
export function buildSPARQLGetDataQuery(baseExpression: string): string {
  return `
    SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
      ${iri(baseExpression)} ?predicate ?target .
      FILTER(isIRI(?predicate))
      BIND(${iri(baseExpression)} AS ?source)
      BIND(<< ${iri(baseExpression)} ?predicate ?target >> AS ?ann)
      ?ann <ad4m://ontology/author> ?author .
      ?ann <ad4m://ontology/timestamp> ?timestamp .
    }
  `.trim();
}

/**
 * Group flat SPARQL link rows into the same shape that SPARQL returned:
 * `{ source_uri: string, links: Array<{predicate, target, author, timestamp}> }`
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
 * Uses direct triple patterns instead of link-node reification.
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
        const formatted = (condition as any[]).map(v => iri(v)).join(", ");
        filters.push(`?source IN (${formatted})`);
      } else if (typeof condition === "object" && condition !== null) {
        const ops = condition as any;
        if (ops.not !== undefined) {
          if (Array.isArray(ops.not)) {
            const formatted = (ops.not as any[]).map(v => iri(v)).join(", ");
            filters.push(`!(?source IN (${formatted}))`);
          } else {
            filters.push(`?source != ${iri(ops.not)}`);
          }
        }
      } else {
        filters.push(`?source = ${iri(String(condition))}`);
      }
      continue;
    }

    if (propertyName === "author" || propertyName === "timestamp") {
      continue;
    }

    const propMeta = metadata.properties[propertyName];
    if (!propMeta) continue;

    if (Array.isArray(condition)) {
      // IN clause
      if (propMeta.resolveLanguage === "literal") {
        joins.push(`
      ?source ${iri(propMeta.predicate)} ?wTarget_${propertyName} .`);
        const formatted = (condition as any[]).map(v => formatSPARQLValue(v)).join(", ");
        filters.push(`<ad4m://fn/parse_literal>(STR(?wTarget_${propertyName})) IN (${formatted})`);
      } else {
        const formatted = (condition as any[]).map(v => iri(v)).join(", ");
        joins.push(`
      ?source ${iri(propMeta.predicate)} ?wTarget_${propertyName} .`);
        filters.push(`?wTarget_${propertyName} IN (${formatted})`);
      }
    } else if (typeof condition === "object" && condition !== null) {
      const ops = condition as any;
      if (ops.not !== undefined) {
        if (Array.isArray(ops.not)) {
          if (propMeta.resolveLanguage === "literal") {
            const formatted = (ops.not as any[]).map(v => formatSPARQLValue(v)).join(", ");
            filters.push(`
          NOT EXISTS {
            ?source ${iri(propMeta.predicate)} ?wTarget_not_${propertyName} .
            FILTER(<ad4m://fn/parse_literal>(STR(?wTarget_not_${propertyName})) IN (${formatted}))
          }
        `);
          } else {
            const formatted = (ops.not as any[]).map(v => iri(v)).join(", ");
            filters.push(`
          NOT EXISTS {
            ?source ${iri(propMeta.predicate)} ?wTarget_not_${propertyName} .
            FILTER(?wTarget_not_${propertyName} IN (${formatted}))
          }
        `);
          }
        } else {
          if (propMeta.resolveLanguage === "literal") {
            const formatted = formatSPARQLValue(ops.not);
            filters.push(`
          NOT EXISTS {
            ?source ${iri(propMeta.predicate)} ?wTarget_not_${propertyName} .
            FILTER(<ad4m://fn/parse_literal>(STR(?wTarget_not_${propertyName})) = ${formatted})
          }
        `);
          } else {
            filters.push(`
          NOT EXISTS {
            ?source ${iri(propMeta.predicate)} ${iri(ops.not)} .
          }
        `);
          }
        }
      }

      // Comparison operators
      const targetVar = `?wTarget_cmp_${propertyName}`;
      const valueExpr = propMeta.resolveLanguage === "literal"
        ? `<ad4m://fn/parse_literal>(STR(${targetVar}))`
        : `STR(${targetVar})`;

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
      ?source ${iri(propMeta.predicate)} ${targetVar} .`);
        filters.push(...compFilters);
      }
    } else {
      // Simple equality
      if (propMeta.resolveLanguage === "literal") {
        const formatted = formatSPARQLValue(condition);
        joins.push(`
      ?source ${iri(propMeta.predicate)} ?wTarget_${propertyName} .`);
        filters.push(`<ad4m://fn/parse_literal>(STR(?wTarget_${propertyName})) = ${formatted}`);
      } else {
        joins.push(`
      ?source ${iri(propMeta.predicate)} ${iri(String(condition))} .`);
      }
    }
  }

  return { joins, filters };
}
