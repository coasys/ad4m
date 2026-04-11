/**
 * SPARQL query building utilities for Ad4mModel.
 *
 * Uses the direct triple + named graph storage model where each AD4M link
 * is stored in a named graph (keyed by link hash) with metadata in the default graph.
 *
 * All AD4M URIs (source, predicate, target) become RDF IRIs.
 *
 * @module
 */

import { Literal } from "../Literal";
import { resolveParentPredicate } from "./query-common";
import type { RelationMetadataEntry } from "./decorators";
import type { Where, Query, ModelMetadata, PropertyMetadata } from "./types";

/**
 * Check whether a `where` clause contains filters that cannot be pushed down
 * to SPARQL and must be evaluated in JS after hydration.
 *
 * JS-only filters include:
 * - Literal-stored properties (equality, comparison, contains, etc.)
 * - Reverse relations (belongsToOne / belongsToMany)
 * - author / timestamp filters (skipped by buildSPARQLWhereFilters)
 *
 * When JS-only filters exist, SPARQL-level LIMIT/OFFSET must NOT be applied
 * because the database would cap the candidate set before JS filters run,
 * potentially discarding matches beyond the LIMIT.
 */
export function hasJsOnlyWhereFilters(
  metadata: ModelMetadata,
  allRelationsMetadata: Record<string, RelationMetadataEntry>,
  where?: Where,
): boolean {
  if (!where) return false;

  for (const [propertyName, condition] of Object.entries(where)) {
    if (propertyName === "base" || propertyName === "id") continue;

    // author/timestamp filters are handled in JS
    if (propertyName === "author" || propertyName === "timestamp") return true;

    const propMeta = metadata.properties[propertyName];
    if (!propMeta) continue;

    // Reverse relations are JS-only
    const relMeta = allRelationsMetadata[propertyName];
    if (relMeta && (relMeta.kind === 'belongsToOne' || relMeta.kind === 'belongsToMany')) {
      return true;
    }

    // Literal-stored properties are JS-only (value filtering can't be pushed to SPARQL)
    if (isLiteralStoredProperty(propMeta)) {
      // Check if the condition actually filters on value (not just existence)
      if (condition !== undefined && condition !== null) {
        return true;
      }
    }
  }

  return false;
}

/**
 * Check if a string value looks like a URI (has a scheme).
 */
function looksLikeUri(value: string): boolean {
  return /^[a-zA-Z][a-zA-Z0-9+\-._]*:/.test(value);
}

/**
 * Convert a JS value to its literal: IRI form, matching how the Rust executor
 * stores property values that don't have a resolveLanguage set.
 * Strings that already look like URIs are returned as-is.
 */
function valueToLiteralIri(value: any): string {
  if (typeof value === 'string') {
    if (looksLikeUri(value)) return value;
    return Literal.from(value).toUrl();
  }
  if (typeof value === 'number') {
    return Literal.from(value).toUrl();
  }
  if (typeof value === 'boolean') {
    return Literal.from(value).toUrl();
  }
  return Literal.from(String(value)).toUrl();
}

/**
 * Determine if a property stores values as literal: IRIs (needs parse_literal for comparisons).
 * Properties with resolveLanguage === "literal" AND properties without resolveLanguage
 * both store values as literal: URIs. Only properties with a non-literal resolveLanguage
 * (e.g. file storage) or flag properties store raw URIs.
 */
function isLiteralStoredProperty(propMeta: PropertyMetadata): boolean {
  if (propMeta.flag) return false;
  // If resolveLanguage is set to something other than "literal", it's a language expression URI
  if (propMeta.resolveLanguage && propMeta.resolveLanguage !== "literal") return false;
  // Everything else (resolveLanguage === "literal" or undefined) stores as literal: URIs
  return true;
}

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
 * Map a user-facing property name to the SPARQL variable alias used in the query.
 *
 * Properties appear in the query as:
 * - Required properties: `?cfTarget_${name}`
 * - Where-clause joins: `?wTarget_${name}`
 * - Initial-value fallback: `?cfInitTarget_${name}`
 *
 * For ORDER BY, we prefer the conformance variable (always present for required props),
 * then the where-target variable.
 */
function mapPropertyToSPARQLVar(propertyName: string, metadata: ModelMetadata): string {
  const propMeta = metadata.properties[propertyName];
  if (propMeta) {
    if (propMeta.required && !propMeta.getter && !(propMeta.flag && propMeta.initial)) {
      return `?cfTarget_${propertyName}`;
    }
    if (propMeta.initial && !propMeta.flag) {
      return `?cfInitTarget_${propertyName}`;
    }
  }
  // Fallback: where-clause variable or raw name
  return `?wTarget_${propertyName}`;
}

/**
 * Build a SPARQL query that returns all links belonging to instances of
 * the given model class, optionally filtered by `where` conditions.
 *
 * Uses direct triple patterns: ?source ?predicate ?target
 * with named graph storage and metadata keyed by graph IRI.
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
  // Use a subquery with DISTINCT to avoid row multiplication when the main
  // ?source ?predicate ?target pattern cross-joins with multiple structural matches.
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
      { SELECT DISTINCT ?source WHERE { ?source ?cf_structPred ?cf_structTarget . FILTER(?cf_structPred IN (${knownPredicates.join(", ")})) } }`);
    }
  }

  // Build WHERE clause filters from user query
  const { joins: userJoins, filters: userFilters } = buildSPARQLWhereFilters(metadata, allRelationsMetadata, query.where);
  joinPatterns.push(...userJoins);
  filterExpressions.push(...userFilters);

  // Main triple pattern — fetches all links for matched sources
  // The direct triple in named graph: GRAPH ?linkGraph { ?source ?predicate ?target }
  // FILTER(isIRI(?source)) excludes non-IRI subjects
  const joinClause = joinPatterns.join("\n");
  const filterClause = filterExpressions.length > 0
    ? `FILTER(\n      ${filterExpressions.join(" &&\n      ")}\n    )`
    : "";

  // When limit/offset are specified, wrap source-selection in a subquery so
  // the database only materialises links for the requested page of instances.
  // Without this, ALL instances' links are fetched and pagination happens in JS.
  const hasPagination = query.limit !== undefined || query.offset !== undefined;
  const hasJsFilters = hasJsOnlyWhereFilters(metadata, allRelationsMetadata, query.where);

  if (hasPagination && !hasJsFilters) {
    // Build a subquery that selects DISTINCT sources with ordering + pagination.
    // We need a timestamp for ordering — get the earliest link timestamp per source.
    const orderByClause = query.order
      ? Object.entries(query.order).map(([prop, dir]) => {
          const v = prop === 'timestamp' ? '?_srcTs' : mapPropertyToSPARQLVar(prop, metadata);
          return dir === 'DESC' ? `DESC(${v})` : `ASC(${v})`;
        }).join(' ')
      : 'ASC(?_srcTs)';

    const limitClause = query.limit !== undefined ? `LIMIT ${query.limit}` : '';
    const offsetClause = query.offset !== undefined ? `OFFSET ${query.offset}` : '';

    return `
    SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
      {
        SELECT DISTINCT ?source (MIN(?_ts) AS ?_srcTs) WHERE {${joinClause}
          GRAPH ?_tsg { ?source ?_p ?_t . }
          ?_tsg <ad4m://ontology/timestamp> ?_ts .
          ${filterClause}
        }
        GROUP BY ?source
        ORDER BY ${orderByClause}
        ${limitClause}
        ${offsetClause}
      }
      GRAPH ?linkGraph { ?source ?predicate ?target . }
      FILTER(isIRI(?source) && isIRI(?predicate))
      ?linkGraph <ad4m://ontology/author> ?author .
      ?linkGraph <ad4m://ontology/timestamp> ?timestamp .
    }
  `.trim();
  }

  return `
    SELECT ?source ?predicate ?target ?author ?timestamp WHERE {${joinClause}
      GRAPH ?linkGraph { ?source ?predicate ?target . }
      FILTER(isIRI(?source) && isIRI(?predicate))
      ?linkGraph <ad4m://ontology/author> ?author .
      ?linkGraph <ad4m://ontology/timestamp> ?timestamp .
      ${filterClause}
    }
    ${buildSPARQLOrderLimitOffset(metadata, query)}
  `.trim();
}

/**
 * Build ORDER BY / LIMIT / OFFSET clauses for the SPARQL query.
 *
 * NOTE: The outer SELECT returns multiple rows per instance (one per link),
 * so LIMIT/OFFSET at the outer level would cut off links mid-instance.
 * Instead, we apply these clauses so the database can at least use them
 * as hints. The JS-side pagination in instancesFromQueryResult remains
 * the authoritative mechanism, but when ORDER BY + LIMIT are present the
 * database can short-circuit scanning via its indexes.
 *
 * For queries with both `order` and `limit`, we wrap the source-selection
 * in a subquery pattern (handled in buildSPARQLQuery). This function emits
 * the raw clauses for simpler cases or as a fallback.
 */
export function buildSPARQLOrderLimitOffset(_metadata: ModelMetadata, query: Query): string {
  const parts: string[] = [];

  if (query.order) {
    const orderTerms = Object.entries(query.order).map(([prop, dir]) => {
      // Map property names to SPARQL variables used in the query
      const varName = prop === 'timestamp' ? '?timestamp' : mapPropertyToSPARQLVar(prop, _metadata);
      return dir === 'DESC' ? `DESC(${varName})` : `ASC(${varName})`;
    });
    if (orderTerms.length > 0) {
      parts.push(`ORDER BY ${orderTerms.join(' ')}`);
    }
  }

  if (query.limit !== undefined) {
    parts.push(`LIMIT ${query.limit}`);
  }

  if (query.offset !== undefined) {
    parts.push(`OFFSET ${query.offset}`);
  }

  return parts.join('\n    ');
}

/**
 * Build a SPARQL query to fetch all links for a single instance.
 */
export function buildSPARQLGetDataQuery(baseExpression: string): string {
  return `
    SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
      GRAPH ?linkGraph { ${iri(baseExpression)} ?predicate ?target . }
      FILTER(isIRI(?predicate))
      BIND(${iri(baseExpression)} AS ?source)
      ?linkGraph <ad4m://ontology/author> ?author .
      ?linkGraph <ad4m://ontology/timestamp> ?timestamp .
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

    // Skip reverse relations (belongsToOne / belongsToMany) — these are registered as
    // read-only properties but the link direction is reversed (target→source, not source→target).
    // Adding a forward join pattern would match nothing. These are filtered in JS post-filter.
    const relMeta = _allRelationsMetadata[propertyName];
    if (relMeta && (relMeta.kind === 'belongsToOne' || relMeta.kind === 'belongsToMany')) {
      continue;
    }

    // Determine if this property stores values as literal: IRIs
    const useParseLiteral = isLiteralStoredProperty(propMeta);

    // For literal-stored properties, skip SPARQL-level FILTER expressions entirely.
    // The parse_literal custom function is unreliable across oxigraph versions and
    // environments. All property-level where filtering is done in JS post-filter
    // (instancesFromQueryResult) after hydration resolves values to JS primitives.
    // We only add a JOIN pattern to ensure the property exists (conformance check).
    if (useParseLiteral) {
      // Just ensure the property exists — no value filtering in SPARQL
      if (typeof condition === "object" && condition !== null && !Array.isArray(condition)) {
        const ops = condition as any;
        const hasCompOps = ops.gt !== undefined || ops.gte !== undefined ||
          ops.lt !== undefined || ops.lte !== undefined ||
          ops.between !== undefined || ops.contains !== undefined ||
          ops.not !== undefined;
        if (hasCompOps) {
          joins.push(`
      ?source ${iri(propMeta.predicate)} ?wTarget_cmp_${propertyName} .`);
        }
        // NOT filters: no SPARQL-level filtering — handled in JS
      } else {
        // Simple equality or IN — add join for conformance.
        // NOTE: push-down FILTER with fn::parse_literal was attempted but removed because
        // Oxigraph's query parser rejects the fn:: custom function syntax at parse time
        // (custom functions are only available at execution time). All literal property
        // filtering is done in JS post-filter after hydration resolves values.
        joins.push(`
      ?source ${iri(propMeta.predicate)} ?wTarget_${propertyName} .`);
      }
    } else if (Array.isArray(condition)) {
      const formatted = (condition as any[]).map(v => iri(v)).join(", ");
      joins.push(`
      ?source ${iri(propMeta.predicate)} ?wTarget_${propertyName} .`);
      filters.push(`?wTarget_${propertyName} IN (${formatted})`);
    } else if (typeof condition === "object" && condition !== null) {
      const ops = condition as any;
      if (ops.not !== undefined) {
        if (Array.isArray(ops.not)) {
          const formatted = (ops.not as any[]).map(v => iri(v)).join(", ");
          filters.push(`
          NOT EXISTS {
            ?source ${iri(propMeta.predicate)} ?wTarget_not_${propertyName} .
            FILTER(?wTarget_not_${propertyName} IN (${formatted}))
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

      const hasCompOps = ops.gt !== undefined || ops.gte !== undefined ||
        ops.lt !== undefined || ops.lte !== undefined ||
        ops.between !== undefined || ops.contains !== undefined;
      if (hasCompOps) {
        joins.push(`
      ?source ${iri(propMeta.predicate)} ?wTarget_cmp_${propertyName} .`);
      }
    } else {
      // Simple equality — non-literal property, use exact IRI match
      joins.push(`
      ?source ${iri(propMeta.predicate)} ${iri(String(condition))} .`);
    }
  }

  return { joins, filters };
}
