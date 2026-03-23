/**
 * Batch SPARQL query builder for Ad4mModel eager-loading (includes).
 *
 * Uses direct triple + RDF-star storage model.
 * Each AD4M link is stored as: <source> <predicate> <target>
 * with RDF-star annotations for metadata.
 *
 * @module
 */

import { resolveParentPredicate } from "./query-common";
import { getRelationsMetadata } from "./decorators";
import type { RelationMetadataEntry } from "./decorators";
import { formatSPARQLValue } from "./query-sparql";
import type { Query, IncludeMap, ModelMetadata } from "./types";

// ──────────────────────────────────────────────────────────
//  Helpers
// ──────────────────────────────────────────────────────────

function iri(value: string): string {
  return `<${value}>`;
}

interface DepthBranch {
  depth: number;
  relationName: string;
  parentDepth: number;
  parentPredicate: string;
  targetClass: any;
  targetMetadata: ModelMetadata;
  includeMap?: IncludeMap;
  direction: 'forward' | 'reverse';
}

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
          ${sourceVar} ${iri(propMeta.predicate)} ${iri(propMeta.initial)} .`);
      } else {
        joins.push(`
          ${sourceVar} ${iri(propMeta.predicate)} ?${prefix}_cfTarget_${propMeta.name} .`);
      }
    }
  }

  if (!hasConformance) {
    for (const [, propMeta] of Object.entries(metadata.properties)) {
      if (propMeta.initial) {
        if (propMeta.flag) {
          joins.push(`
          ${sourceVar} ${iri(propMeta.predicate)} ${iri(propMeta.initial)} .`);
        } else {
          joins.push(`
          ${sourceVar} ${iri(propMeta.predicate)} ?${prefix}_cfInitAny_${propMeta.name} .`);
        }
        break;
      }
    }
  }

  return { joins, filters };
}

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
 * Uses direct triple patterns with RDF-star annotations.
 */
export function buildBatchSPARQLQuery(
  rootMetadata: ModelMetadata,
  query: Query,
  rootModelClass: any,
): string {
  const includeMap = query.include;
  if (!includeMap || Object.keys(includeMap).length === 0) {
    throw new Error("buildBatchSPARQLQuery requires query.include to be non-empty");
  }

  const nextDepth = { value: 1 };
  const branches = flattenIncludeTree(0, rootModelClass, includeMap, nextDepth);

  // Root conformance
  const rootConformance = buildConformanceJoins(rootMetadata, "?source", "root");
  const rootJoins = [...rootConformance.joins];
  const rootFilters = [...rootConformance.filters];

  if (query.parent) {
    const parentPredicate = resolveParentPredicate(query.parent, rootModelClass);
    rootJoins.push(`
        ${iri(query.parent.id)} ${iri(parentPredicate)} ?source .`);
  }

  if (query.where) {
    for (const [propertyName, condition] of Object.entries(query.where)) {
      if (propertyName === "base" || propertyName === "id") {
        if (Array.isArray(condition)) {
          const formatted = (condition as any[]).map(v => iri(v)).join(", ");
          rootFilters.push(`?source IN (${formatted})`);
        } else if (typeof condition === "string") {
          rootFilters.push(`?source = ${iri(condition)}`);
        }
        continue;
      }
      if (propertyName === "author" || propertyName === "timestamp") continue;

      const propMeta = rootMetadata.properties[propertyName];
      if (!propMeta) continue;

      if (typeof condition === "string" || typeof condition === "number" || typeof condition === "boolean") {
        rootJoins.push(`
        ?source ${iri(propMeta.predicate)} ${iri(String(condition))} .`);
      } else if (Array.isArray(condition)) {
        const formatted = (condition as any[]).map(v => iri(v)).join(", ");
        rootJoins.push(`
        ?source ${iri(propMeta.predicate)} ?root_wTarget_${propertyName} .`);
        rootFilters.push(`?root_wTarget_${propertyName} IN (${formatted})`);
      }
    }
  }

  const rootJoinClause = rootJoins.join("\n");
  const rootFilterClause = rootFilters.length > 0
    ? `FILTER(\n          ${rootFilters.join(" &&\n          ")}\n        )`
    : "";

  const unionBranches: string[] = [];

  // Depth 0: root — direct triple pattern with RDF-star
  unionBranches.push(`
      {
        ${rootJoinClause}
        ?source ?predicate ?target .
        FILTER(isIRI(?source) && isIRI(?predicate))
        BIND(<< ?source ?predicate ?target >> AS ?ann)
        ?ann <ad4m://ontology/author> ?author .
        ?ann <ad4m://ontology/timestamp> ?timestamp .
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

    const parentJoinClause = branch.parentDepth === 0 ? rootJoinClause.replace(/\?source/g, '?parentBase') : '';
    const parentFilterClause = branch.parentDepth === 0 ? rootFilterClause.replace(/\?source/g, '?parentBase') : '';

    if (branch.direction === 'forward') {
      // Forward: parent --predicate--> child (source)
      unionBranches.push(`
      {
        # Depth ${branch.depth}: ${branch.relationName} (forward)
        ?parentBase ${iri(branch.parentPredicate)} ?source .
        ${childJoinClause}
        ?source ?predicate ?target .
        FILTER(isIRI(?source) && isIRI(?predicate))
        BIND(<< ?source ?predicate ?target >> AS ?ann)
        ?ann <ad4m://ontology/author> ?author .
        ?ann <ad4m://ontology/timestamp> ?timestamp .
        ${childFilterClause}
        ${parentJoinClause}
        ${parentFilterClause}
        BIND("${branch.depth}" AS ?depth)
        BIND("${branch.relationName}" AS ?relationName)
      }
      `);
    } else {
      // Reverse: child (source) --predicate--> parent
      unionBranches.push(`
      {
        # Depth ${branch.depth}: ${branch.relationName} (reverse)
        ?source ${iri(branch.parentPredicate)} ?parentBase .
        ${childJoinClause}
        ?source ?predicate ?target .
        FILTER(isIRI(?source) && isIRI(?predicate))
        BIND(<< ?source ?predicate ?target >> AS ?ann)
        ?ann <ad4m://ontology/author> ?author .
        ?ann <ad4m://ontology/timestamp> ?timestamp .
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
    SELECT ?depth ?parentBase ?relationName ?source ?predicate ?target ?author ?timestamp WHERE {
      ${unionBranches.join("\n      UNION\n")}
    }
  `.trim();
}
