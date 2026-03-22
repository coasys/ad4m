/**
 * Batch SPARQL query builder for Ad4mModel eager-loading (includes).
 *
 * Generates a single SPARQL query that fetches root instances AND their
 * related instances (possibly multiple levels deep) in one round-trip.
 * Each row is tagged with `?depth` and `?parentBase` so the hydrator
 * can reconstruct the object graph.
 *
 * @module
 */

import { resolveParentPredicate } from "./query-common";
import { getRelationsMetadata } from "./decorators";
import type { RelationMetadataEntry } from "./decorators";
import { formatSPARQLValue } from "./query-sparql";
import type { Query, IncludeMap, ModelMetadata } from "./types";

const ONT = "ad4m://ontology/";

// ──────────────────────────────────────────────────────────
//  Types
// ──────────────────────────────────────────────────────────

interface DepthBranch {
  depth: number;
  relationName: string;
  parentDepth: number;
  parentPredicate: string;
  targetClass: any;
  targetMetadata: ModelMetadata;
  includeMap?: IncludeMap;
  /** 'hasMany'|'hasOne' = forward (parent→child), 'belongsTo*' = reverse (child→parent) */
  direction: 'forward' | 'reverse';
}

// ──────────────────────────────────────────────────────────
//  Helpers
// ──────────────────────────────────────────────────────────

function buildConformanceFilters(metadata: ModelMetadata, sourceVar: string): string[] {
  const filters: string[] = [];

  for (const [, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.required) {
      if (propMeta.getter) continue;
      if (propMeta.flag && propMeta.initial) {
        filters.push(`
          EXISTS {
            ?cfLink a <${ONT}Link> ;
              <${ONT}source> ${sourceVar} ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ${formatSPARQLValue(propMeta.initial)} .
          }
        `);
      } else {
        filters.push(`
          EXISTS {
            ?cfLink a <${ONT}Link> ;
              <${ONT}source> ${sourceVar} ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?cfAny .
          }
        `);
      }
    }
  }

  // Fallback: initial values
  if (filters.length === 0) {
    for (const [, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.initial) {
        if (propMeta.flag) {
          filters.push(`
            EXISTS {
              ?cfInit a <${ONT}Link> ;
                <${ONT}source> ${sourceVar} ;
                <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
                <${ONT}target> ${formatSPARQLValue(propMeta.initial)} .
            }
          `);
        } else {
          filters.push(`
            EXISTS {
              ?cfInit a <${ONT}Link> ;
                <${ONT}source> ${sourceVar} ;
                <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
                <${ONT}target> ?cfInitAny .
            }
          `);
        }
        break;
      }
    }
  }

  return filters;
}

/**
 * Walk the include tree and flatten into an array of DepthBranch descriptors.
 */
function flattenIncludeTree(
  parentDepth: number,
  parentClass: any,
  includeMap: IncludeMap,
  nextDepth: { value: number },
): DepthBranch[] {
  const branches: DepthBranch[] = [];
  const relMeta = getRelationsMetadata(parentClass);

  for (const [relName, includeValue] of Object.entries(includeMap)) {
    const meta: RelationMetadataEntry | undefined = relMeta[relName];
    if (!meta || !meta.target) continue;

    const TargetClass = meta.target();
    const targetMetadata: ModelMetadata = (TargetClass as any).getModelMetadata
      ? (TargetClass as any).getModelMetadata()
      : { className: "Unknown", properties: {}, relations: {} };

    const depth = nextDepth.value++;
    const direction = meta.kind === 'belongsToOne' || meta.kind === 'belongsToMany'
      ? 'reverse' : 'forward';

    const nestedInclude = typeof includeValue === 'object' && includeValue !== null
      ? (includeValue as any).include
      : undefined;

    branches.push({
      depth,
      relationName: relName,
      parentDepth,
      parentPredicate: meta.predicate,
      targetClass: TargetClass,
      targetMetadata,
      includeMap: nestedInclude,
      direction,
    });

    // Recurse
    if (nestedInclude) {
      branches.push(...flattenIncludeTree(depth, TargetClass, nestedInclude, nextDepth));
    }
  }

  return branches;
}

// ──────────────────────────────────────────────────────────
//  Main entry point
// ──────────────────────────────────────────────────────────

/**
 * Build a single SPARQL query that fetches root instances plus all
 * included relations in one query using UNION branches.
 *
 * Each result row has: ?depth ?parentBase ?source ?predicate ?target ?author ?timestamp
 * - depth 0 = root instances, depth N = Nth level of includes
 * - parentBase links each child row back to its parent's base expression
 */
export function buildBatchSPARQLQuery(
  rootMetadata: ModelMetadata,
  query: Query,
  rootModelClass: any,
): string {
  const includeMap = query.include;
  if (!includeMap || Object.keys(includeMap).length === 0) {
    // No includes — shouldn't be called, but degrade gracefully
    throw new Error("buildBatchSPARQLQuery requires query.include to be non-empty");
  }

  // Flatten include tree
  const nextDepth = { value: 1 };
  const branches = flattenIncludeTree(0, rootModelClass, includeMap, nextDepth);

  // ── Root UNION branch (depth 0) ──
  const rootConformance = buildConformanceFilters(rootMetadata, "?source");

  // Parent filter for root
  const rootFilters = [...rootConformance];
  if (query.parent) {
    const parentPredicate = resolveParentPredicate(query.parent, rootModelClass);
    rootFilters.push(`
      EXISTS {
        ?parentLink a <${ONT}Link> ;
          <${ONT}source> ${formatSPARQLValue(query.parent.id)} ;
          <${ONT}predicate> ${formatSPARQLValue(parentPredicate)} ;
          <${ONT}target> ?source .
      }
    `);
  }

  // Where filters for root (simple equality and id/base only — comparison operators in JS)
  if (query.where) {
    for (const [propertyName, condition] of Object.entries(query.where)) {
      if (propertyName === "base" || propertyName === "id") {
        if (Array.isArray(condition)) {
          const formatted = (condition as any[]).map(v => formatSPARQLValue(v)).join(", ");
          rootFilters.push(`?source IN (${formatted})`);
        } else if (typeof condition === "string") {
          rootFilters.push(`?source = ${formatSPARQLValue(condition)}`);
        }
        continue;
      }
      if (propertyName === "author" || propertyName === "timestamp") continue;

      const propMeta = rootMetadata.properties[propertyName];
      if (!propMeta) continue;

      // Simple equality only at SPARQL level
      if (typeof condition === "string" || typeof condition === "number" || typeof condition === "boolean") {
        rootFilters.push(`
          EXISTS {
            ?wLink a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?wTarget .
            FILTER(?wTarget = ${formatSPARQLValue(condition)})
          }
        `);
      } else if (Array.isArray(condition)) {
        const formatted = (condition as any[]).map(v => formatSPARQLValue(v)).join(", ");
        rootFilters.push(`
          EXISTS {
            ?wLink a <${ONT}Link> ;
              <${ONT}source> ?source ;
              <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
              <${ONT}target> ?wTarget .
            FILTER(?wTarget IN (${formatted}))
          }
        `);
      }
    }
  }

  const rootFilterClause = rootFilters.length > 0
    ? `FILTER(\n          ${rootFilters.join(" &&\n          ")}\n        )`
    : "";

  const unionBranches: string[] = [];

  // Depth 0: root
  unionBranches.push(`
      {
        ?link a ad4m:Link ;
              ad4m:source ?source ;
              ad4m:predicate ?predicate ;
              ad4m:target ?target ;
              ad4m:author ?author ;
              ad4m:timestamp ?timestamp .
        ${rootFilterClause}
        BIND("0" AS ?depth)
        BIND("" AS ?parentBase)
        BIND("" AS ?relationName)
      }
  `);

  // Depth N: include branches
  for (const branch of branches) {
    const childConformance = buildConformanceFilters(branch.targetMetadata, "?source");
    const childFilterClause = childConformance.length > 0
      ? `FILTER(\n            ${childConformance.join(" &&\n            ")}\n          )`
      : "";

    if (branch.direction === 'forward') {
      // Forward: parent has a link with predicate→child
      // Find parentBase from depth branch.parentDepth
      unionBranches.push(`
      {
        # Depth ${branch.depth}: ${branch.relationName} (forward)
        ?parentLinkD${branch.depth} a ad4m:Link ;
              ad4m:source ?parentBase ;
              ad4m:predicate ${formatSPARQLValue(branch.parentPredicate)} ;
              ad4m:target ?source .
        ?link a ad4m:Link ;
              ad4m:source ?source ;
              ad4m:predicate ?predicate ;
              ad4m:target ?target ;
              ad4m:author ?author ;
              ad4m:timestamp ?timestamp .
        ${childFilterClause}
        ${branch.parentDepth === 0 ? rootFilterClause.replace(/\?source/g, '?parentBase') : ''}
        BIND("${branch.depth}" AS ?depth)
        BIND("${branch.relationName}" AS ?relationName)
      }
      `);
    } else {
      // Reverse: child has a link with predicate→parent
      unionBranches.push(`
      {
        # Depth ${branch.depth}: ${branch.relationName} (reverse)
        ?reverseLinkD${branch.depth} a ad4m:Link ;
              ad4m:source ?source ;
              ad4m:predicate ${formatSPARQLValue(branch.parentPredicate)} ;
              ad4m:target ?parentBase .
        ?link a ad4m:Link ;
              ad4m:source ?source ;
              ad4m:predicate ?predicate ;
              ad4m:target ?target ;
              ad4m:author ?author ;
              ad4m:timestamp ?timestamp .
        ${childFilterClause}
        ${branch.parentDepth === 0 ? rootFilterClause.replace(/\?source/g, '?parentBase') : ''}
        BIND("${branch.depth}" AS ?depth)
        BIND("${branch.relationName}" AS ?relationName)
      }
      `);
    }
  }

  return `
    PREFIX ad4m: <${ONT}>
    SELECT ?depth ?parentBase ?relationName ?source ?predicate ?target ?author ?timestamp WHERE {
      ${unionBranches.join("\n      UNION\n")}
    }
  `.trim();
}
