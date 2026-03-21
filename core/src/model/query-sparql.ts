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
  // Step 1: Build conformance filters — SPARQL subqueries that ensure
  // a source node has the required links to be considered an instance
  // of this model class.
  const conformanceFilters: string[] = [];

  // Parent filter
  if (query.parent) {
    const parentPredicate = resolveParentPredicate(query.parent, modelClass);
    conformanceFilters.push(`
      EXISTS {
        ?parentLink a <${ONT}Link> ;
          <${ONT}source> ${formatSPARQLValue(query.parent.id)} ;
          <${ONT}predicate> ${formatSPARQLValue(parentPredicate)} ;
          <${ONT}target> ?source .
      }
    `);
  }

  // Required property filters
  for (const [, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.required) {
      if (propMeta.getter) continue;
      if (propMeta.flag && propMeta.initial) {
        conformanceFilters.push(`
          EXISTS {
            ?reqLink_${propMeta.name} a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ${formatSPARQLValue(propMeta.initial)} .
          }
        `);
      } else {
        conformanceFilters.push(`
          EXISTS {
            ?reqLink_${propMeta.name} a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?anyTarget_${propMeta.name} .
          }
        `);
      }
    }
  }

  // Fallback: initial-value filters
  if (conformanceFilters.length === 0) {
    for (const [, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.initial) {
        if (propMeta.flag) {
          conformanceFilters.push(`
            EXISTS {
              ?initLink a <${ONT}Link> ;
                <${ONT}source> ?source ;
                <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
                <${ONT}target> ${formatSPARQLValue(propMeta.initial)} .
            }
          `);
        } else {
          conformanceFilters.push(`
            EXISTS {
              ?initLink a <${ONT}Link> ;
                <${ONT}source> ?source ;
                <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
                <${ONT}target> ?anyInitTarget .
            }
          `);
        }
        break;
      }
    }
  }

  // Fallback: open-world structural matching — at least one known predicate
  if (conformanceFilters.length === 0) {
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
      conformanceFilters.push(`
        EXISTS {
          ?structLink a <${ONT}Link> ;
            <${ONT}source> ?source ;
            <${ONT}predicate> ?structPred .
          FILTER(?structPred IN (${knownPredicates.join(", ")}))
        }
      `);
    }
  }

  // Step 2: Build WHERE clause filters from user query
  const userFilters = buildSPARQLWhereFilters(metadata, allRelationsMetadata, query.where);

  // Step 3: Assemble the full query
  // We select all links for matching sources
  const allFilters = [...conformanceFilters, ...userFilters];
  const filterClause = allFilters.length > 0
    ? `FILTER(\n      ${allFilters.join(" &&\n      ")}\n    )`
    : "";

  return `
    PREFIX ad4m: <${ONT}>
    SELECT ?source ?predicate ?target ?author ?timestamp WHERE {
      ?link a ad4m:Link ;
            ad4m:source ?source ;
            ad4m:predicate ?predicate ;
            ad4m:target ?target ;
            ad4m:author ?author ;
            ad4m:timestamp ?timestamp .
      ${filterClause}
    }
  `.trim();
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
): string[] {
  if (!where) return [];

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

    // Property filters: add an EXISTS subquery that matches a link with the property predicate
    const propMeta = metadata.properties[propertyName];
    if (!propMeta) continue;

    if (Array.isArray(condition)) {
      // IN clause
      const formatted = (condition as any[]).map(v => formatSPARQLValue(v)).join(", ");
      if (propMeta.resolveLanguage === "literal") {
        filters.push(`
          EXISTS {
            ?wLink_${propertyName} a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?wTarget_${propertyName} .
            FILTER(<ad4m://fn/parse_literal>(?wTarget_${propertyName}) IN (${formatted}))
          }
        `);
      } else {
        filters.push(`
          EXISTS {
            ?wLink_${propertyName} a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?wTarget_${propertyName} .
            FILTER(?wTarget_${propertyName} IN (${formatted}))
          }
        `);
      }
    } else if (typeof condition === "object" && condition !== null) {
      const ops = condition as any;
      if (ops.not !== undefined) {
        // NOT clause
        if (Array.isArray(ops.not)) {
          const formatted = (ops.not as any[]).map(v => formatSPARQLValue(v)).join(", ");
          if (propMeta.resolveLanguage === "literal") {
            filters.push(`
              EXISTS {
                ?wLink_${propertyName} a <${ONT}Link> ;
                  <${ONT}source> ?source ;
                  <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
                  <${ONT}target> ?wTarget_${propertyName} .
                FILTER(!(<ad4m://fn/parse_literal>(?wTarget_${propertyName}) IN (${formatted})))
              }
            `);
          } else {
            filters.push(`
              EXISTS {
                ?wLink_${propertyName} a <${ONT}Link> ;
                  <${ONT}source> ?source ;
                  <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
                  <${ONT}target> ?wTarget_${propertyName} .
                FILTER(!(?wTarget_${propertyName} IN (${formatted})))
              }
            `);
          }
        } else {
          const formatted = formatSPARQLValue(ops.not);
          if (propMeta.resolveLanguage === "literal") {
            filters.push(`
              EXISTS {
                ?wLink_${propertyName} a <${ONT}Link> ;
                  <${ONT}source> ?source ;
                  <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
                  <${ONT}target> ?wTarget_${propertyName} .
                FILTER(<ad4m://fn/parse_literal>(?wTarget_${propertyName}) != ${formatted})
              }
            `);
          } else {
            filters.push(`
              EXISTS {
                ?wLink_${propertyName} a <${ONT}Link> ;
                  <${ONT}source> ?source ;
                  <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
                  <${ONT}target> ?wTarget_${propertyName} .
                FILTER(?wTarget_${propertyName} != ${formatted})
              }
            `);
          }
        }
      }
      // Comparison operators (gt, gte, lt, lte, between, contains) are handled in JS
    } else {
      // Simple equality
      const formatted = formatSPARQLValue(condition);
      if (propMeta.resolveLanguage === "literal") {
        filters.push(`
          EXISTS {
            ?wLink_${propertyName} a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?wTarget_${propertyName} .
            FILTER(<ad4m://fn/parse_literal>(?wTarget_${propertyName}) = ${formatted})
          }
        `);
      } else {
        filters.push(`
          EXISTS {
            ?wLink_${propertyName} a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?wTarget_${propertyName} .
            FILTER(?wTarget_${propertyName} = ${formatted})
          }
        `);
      }
    }
  }

  return filters;
}
