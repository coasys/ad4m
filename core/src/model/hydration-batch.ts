/**
 * Batch hydration for SPARQL batch query results.
 *
 * Takes the flat rows returned by `buildBatchSPARQLQuery()` (each tagged
 * with ?depth, ?parentBase, ?relationName) and reconstructs a tree of
 * model instances.
 *
 * @module
 */

import { hydrateFromLinks, evaluateCustomGettersForInstance } from "./hydration";
import { getRelationsMetadata } from "./decorators";
import type { IncludeMap, ModelMetadata } from "./types";

/**
 * A row returned by the batch SPARQL query.
 */
interface BatchRow {
  depth: string;
  parentBase: string;
  relationName: string;
  source: string;
  predicate: string;
  target: string;
  author: string;
  timestamp: string;
}

/**
 * Grouped link data for one instance (same shape as SPARQL grouped result).
 */
interface GroupedInstance {
  source_uri: string;
  parentBase: string;
  relationName: string;
  links: Array<{ predicate: string; target: string; author: string; timestamp: string }>;
}

/**
 * Group batch rows by (depth, source) → array of links, preserving parentBase/relationName.
 */
function groupBatchRows(rows: BatchRow[]): Map<number, GroupedInstance[]> {
  const byDepth = new Map<number, Map<string, GroupedInstance>>();

  for (const row of rows) {
    const depth = parseInt(row.depth, 10);
    if (!byDepth.has(depth)) byDepth.set(depth, new Map());
    const depthMap = byDepth.get(depth)!;

    const key = row.source;
    if (!depthMap.has(key)) {
      depthMap.set(key, {
        source_uri: key,
        parentBase: row.parentBase,
        relationName: row.relationName,
        links: [],
      });
    }
    depthMap.get(key)!.links.push({
      predicate: row.predicate,
      target: row.target,
      author: row.author,
      timestamp: row.timestamp,
    });
  }

  const result = new Map<number, GroupedInstance[]>();
  for (const [depth, map] of byDepth) {
    result.set(depth, Array.from(map.values()));
  }
  return result;
}

/**
 * Hydrate batch SPARQL results into a tree of model instances.
 *
 * @param rows - Flat rows from the batch SPARQL query
 * @param rootClass - The root model class (extends Ad4mModel)
 * @param includeMap - The include specification from the query
 * @param perspective - The PerspectiveProxy for creating instances
 * @returns Array of hydrated root instances with relations populated
 */
export async function hydrateBatchResult<T>(
  rows: BatchRow[],
  rootClass: any,
  includeMap: IncludeMap,
  perspective: any,
): Promise<T[]> {
  if (!rows || rows.length === 0) return [];

  const grouped = groupBatchRows(rows);

  // Hydrate root instances (depth 0)
  const rootInstances = grouped.get(0) || [];
  const metadata: ModelMetadata = rootClass.getModelMetadata();
  const instances: T[] = [];
  const instanceMap = new Map<string, T>();

  for (const group of rootInstances) {
    const instance = new rootClass(perspective, group.source_uri) as any;
    await hydrateFromLinks(instance, group.links, metadata, perspective);
    await evaluateCustomGettersForInstance(instance, perspective, metadata);
    instances.push(instance);
    instanceMap.set(group.source_uri, instance);
  }

  // Hydrate each depth level and wire to parents
  const relMeta = getRelationsMetadata(rootClass);
  const maxDepth = Math.max(...Array.from(grouped.keys()), 0);

  // Build a map of depth → (class, relMetadata, parentInstanceMap)
  // For depth 1, parents are root instances; for deeper depths, we need
  // the instances from the previous level.
  const depthInstanceMaps = new Map<number, Map<string, any>>();
  depthInstanceMaps.set(0, instanceMap);

  // Build depth→class mapping from include tree
  const depthClassMap = new Map<number, { targetClass: any; includeMap?: IncludeMap }>();
  buildDepthClassMap(rootClass, includeMap, 0, { value: 1 }, depthClassMap);

  for (let depth = 1; depth <= maxDepth; depth++) {
    const depthGroups = grouped.get(depth) || [];
    if (depthGroups.length === 0) continue;

    const depthInfo = depthClassMap.get(depth);
    if (!depthInfo) continue;

    const TargetClass = depthInfo.targetClass;
    const targetMetadata: ModelMetadata = TargetClass.getModelMetadata
      ? TargetClass.getModelMetadata()
      : { className: "Unknown", properties: {}, relations: {} };

    const currentDepthInstances = new Map<string, any>();

    // Group by parentBase to wire children to parents
    for (const group of depthGroups) {
      const instance = new TargetClass(perspective, group.source_uri) as any;
      await hydrateFromLinks(instance, group.links, targetMetadata, perspective);
      await evaluateCustomGettersForInstance(instance, perspective, targetMetadata);
      currentDepthInstances.set(group.source_uri, instance);

      // Wire to parent
      // Find the parent depth's instance map
      // The parentBase should exist in one of the prior depth maps
      for (let pd = depth - 1; pd >= 0; pd--) {
        const parentMap = depthInstanceMaps.get(pd);
        if (parentMap && parentMap.has(group.parentBase)) {
          const parent = parentMap.get(group.parentBase)!;
          const relName = group.relationName;

          // Determine relation metadata to know if it's an array or single
          const parentRelMeta = getRelationsMetadata(pd === 0 ? rootClass : (depthClassMap.get(pd)?.targetClass || rootClass));
          const rel = parentRelMeta[relName];

          if (rel) {
            if (rel.kind === 'hasMany' || rel.kind === 'belongsToMany') {
              if (!Array.isArray(parent[relName])) {
                parent[relName] = [];
              }
              parent[relName].push(instance);
            } else {
              parent[relName] = instance;
            }
          }
          break;
        }
      }
    }

    depthInstanceMaps.set(depth, currentDepthInstances);
  }

  return instances;
}

/**
 * Build a mapping from depth number to target class info.
 */
function buildDepthClassMap(
  parentClass: any,
  includeMap: IncludeMap,
  _parentDepth: number,
  nextDepth: { value: number },
  result: Map<number, { targetClass: any; includeMap?: IncludeMap }>,
): void {
  const relMeta = getRelationsMetadata(parentClass);

  for (const [relName, includeValue] of Object.entries(includeMap)) {
    const meta = relMeta[relName];
    if (!meta || !meta.target) continue;

    const TargetClass = meta.target();
    const depth = nextDepth.value++;

    const nestedInclude = typeof includeValue === 'object' && includeValue !== null
      ? (includeValue as any).include
      : undefined;

    result.set(depth, { targetClass: TargetClass, includeMap: nestedInclude });

    if (nestedInclude) {
      buildDepthClassMap(TargetClass, nestedInclude, depth, nextDepth, result);
    }
  }
}
