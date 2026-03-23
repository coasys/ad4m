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

function buildConformanceJoins(metadata: ModelMetadata, sourceVar: string, prefix: string): { joins: string[]; filters: string[] } {
  const joins: string[] = [];
  const filters: string[] = [];
  let hasConformance = false;

  for (const [, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.required) {
      if (propMeta.getter) continue;
      hasConformance = true;
      if (propMeta.flag && propMeta.initial) {
        joins.push(`
          ?${prefix}_cf_${propMeta.name} a <${ONT}Link> ;
            <${ONT}source> ${sourceVar} ;
            <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
            <${ONT}target> ${formatSPARQLValue(propMeta.initial)} .`);
      } else {
        joins.push(`
          ?${prefix}_cf_${propMeta.name} a <${ONT}Link> ;
            <${ONT}source> ${sourceVar} ;
            <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
            <${ONT}target> ?${prefix}_cfTarget_${propMeta.name} .`);
      }
    }
  }

  // Fallback: initial values
  if (!hasConformance) {
    for (const [, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.initial) {
        if (propMeta.flag) {
          joins.push(`
          ?${prefix}_cfInit_${propMeta.name} a <${ONT}Link> ;
            <${ONT}source> ${sourceVar} ;
            <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
            <${ONT}target> ${formatSPARQLValue(propMeta.initial)} .`);
        } else {
          joins.push(`
          ?${prefix}_cfInit_${propMeta.name} a <${ONT}Link> ;
            <${ONT}source> ${sourceVar} ;
            <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
            <${ONT}target> ?${prefix}_cfInitAny_${propMeta.name} .`);
        }
        break;
      }
    }
  }

  return { joins, filters };
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
  const rootConformance = buildConformanceJoins(rootMetadata, "?source", "root");

  // Parent JOIN for root
  const rootJoins = [...rootConformance.joins];
  const rootFilters = [...rootConformance.filters];
  if (query.parent) {
    const parentPredicate = resolveParentPredicate(query.parent, rootModelClass);
    rootJoins.push(`
        ?root_parentLink a <${ONT}Link> ;
          <${ONT}source> ${formatSPARQLValue(query.parent.id)} ;
          <${ONT}predicate> ${formatSPARQLValue(parentPredicate)} ;
          <${ONT}target> ?source .`);
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

      // Simple equality — use JOIN
      if (typeof condition === "string" || typeof condition === "number" || typeof condition === "boolean") {
        rootJoins.push(`
        ?root_w_${propertyName} a <${ONT}Link> ;
          <${ONT}source> ?source ;
          <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
          <${ONT}target> ${formatSPARQLValue(condition)} .`);
      } else if (Array.isArray(condition)) {
        const formatted = (condition as any[]).map(v => formatSPARQLValue(v)).join(", ");
        rootJoins.push(`
        ?root_w_${propertyName} a <${ONT}Link> ;
          <${ONT}source> ?source ;
          <${ONT}predicate> ${formatSPARQLValue(propMeta.predicate)} ;
          <${ONT}target> ?root_wTarget_${propertyName} .`);
        rootFilters.push(`?root_wTarget_${propertyName} IN (${formatted})`);
      }
    }
  }

  const rootJoinClause = rootJoins.join("\n");
  const rootFilterClause = rootFilters.length > 0
    ? `FILTER(\n          ${rootFilters.join(" &&\n          ")}\n        )`
    : "";

  const unionBranches: string[] = [];

  // Depth 0: root
  unionBranches.push(`
      {
        ${rootJoinClause}
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
    const childConformance = buildConformanceJoins(branch.targetMetadata, "?source", `d${branch.depth}`);
    const childJoinClause = childConformance.joins.join("\n");
    const childFilterClause = childConformance.filters.length > 0
      ? `FILTER(\n            ${childConformance.filters.join(" &&\n            ")}\n          )`
      : "";

    // Build parent conformance JOINs for depth-0 parents
    const parentJoinClause = branch.parentDepth === 0 ? rootJoinClause.replace(/\?source/g, '?parentBase') : '';
    const parentFilterClause = branch.parentDepth === 0 ? rootFilterClause.replace(/\?source/g, '?parentBase') : '';

    if (branch.direction === 'forward') {
      unionBranches.push(`
      {
        # Depth ${branch.depth}: ${branch.relationName} (forward)
        ?parentLinkD${branch.depth} a ad4m:Link ;
              ad4m:source ?parentBase ;
              ad4m:predicate ${formatSPARQLValue(branch.parentPredicate)} ;
              ad4m:target ?source .
        ${childJoinClause}
        ?link a ad4m:Link ;
              ad4m:source ?source ;
              ad4m:predicate ?predicate ;
              ad4m:target ?target ;
              ad4m:author ?author ;
              ad4m:timestamp ?timestamp .
        ${childFilterClause}
        ${parentJoinClause}
        ${parentFilterClause}
        BIND("${branch.depth}" AS ?depth)
        BIND("${branch.relationName}" AS ?relationName)
      }
      `);
    } else {
      unionBranches.push(`
      {
        # Depth ${branch.depth}: ${branch.relationName} (reverse)
        ?reverseLinkD${branch.depth} a ad4m:Link ;
              ad4m:source ?source ;
              ad4m:predicate ${formatSPARQLValue(branch.parentPredicate)} ;
              ad4m:target ?parentBase .
        ${childJoinClause}
        ?link a ad4m:Link ;
              ad4m:source ?source ;
              ad4m:predicate ?predicate ;
              ad4m:target ?target ;
              ad4m:author ?author ;
              ad4m:timestamp ?timestamp .
        ${childFilterClause}
        ${parentJoinClause}
        ${parentFilterClause}
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
