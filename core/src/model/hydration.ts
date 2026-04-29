/**
 * hydration.ts — Instance hydration helpers extracted from Ad4mModel.
 *
 * After the Rust model_query pipeline migration (Phases 1-13), most hydration
 * logic moved to Rust. This file retains:
 *   - normalizeValue() — snapshot dirty-tracking
 *   - matchesCondition() — JS-side post-filter (used by evaluateCustomGettersForInstance)
 *   - evaluateCustomGettersForInstance() — lazy getter evaluation (Ad4mModel.evaluateGetters)
 *   - buildConformanceGetter() — conformance SPARQL generation for relation getters
 */

import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { buildConformanceFilter } from "./decorators";
import { compileWhereClause } from "./query-utils";
import { escapeQueryString } from "../utils";
import type {
  RelationMetadata, ModelMetadata,
  WhereCondition,
} from "./types";

// ──────────────────────────────────────────────────────────
//  Pure helpers
// ──────────────────────────────────────────────────────────

/**
 * Normalize a value for snapshot storage.
 * Arrays of model instances are reduced to their `.id` strings so that
 * dirty-tracking compares stable identifiers instead of object references.
 */
export function normalizeValue(value: any): any {
  if (Array.isArray(value)) {
    return value.map((v: any) =>
      v && typeof v === 'object' && typeof v.id === 'string' ? v.id : v,
    );
  }
  return value;
}

/**
 * Checks if a value matches a where-condition (for post-query filtering).
 */
export function matchesCondition(value: any, condition: WhereCondition): boolean {
  // Handle array values (IN clause)
  if (Array.isArray(condition)) {
    return (condition as any[]).includes(value);
  }
  
  // Handle operator object
  if (typeof condition === 'object' && condition !== null) {
    const ops = condition as any;
    
    // Special case: 'not' operator (exclusive with other operators)
    if (ops.not !== undefined) {
      if (Array.isArray(ops.not)) {
        return !(ops.not as any[]).includes(value);
      } else {
        return value !== ops.not;
      }
    }
    
    // Special case: 'between' operator (inclusive range, exclusive with gt/gte/lt/lte)
    if (ops.between !== undefined && Array.isArray(ops.between) && ops.between.length === 2) {
      return value >= ops.between[0] && value <= ops.between[1];
    }
    
    // For all other operators (gt, gte, lt, lte, contains), we need to check ALL of them
    // and return true only if ALL conditions are satisfied
    let allConditionsMet = true;
    
    if (ops.gt !== undefined) {
      allConditionsMet = allConditionsMet && (value > ops.gt);
    }
    
    if (ops.gte !== undefined) {
      allConditionsMet = allConditionsMet && (value >= ops.gte);
    }
    
    if (ops.lt !== undefined) {
      allConditionsMet = allConditionsMet && (value < ops.lt);
    }
    
    if (ops.lte !== undefined) {
      allConditionsMet = allConditionsMet && (value <= ops.lte);
    }
    
    if (ops.contains !== undefined) {
      if (typeof value === 'string') {
        allConditionsMet = allConditionsMet && value.includes(String(ops.contains));
      } else if (Array.isArray(value)) {
        allConditionsMet = allConditionsMet && value.includes(ops.contains);
      } else {
        allConditionsMet = false;
      }
    }
    
    return allConditionsMet;
  }
  
  // Simple equality
  return value === condition;
}

// ──────────────────────────────────────────────────────────
//  SPARQL custom getter evaluation
// ──────────────────────────────────────────────────────────

/**
 * Builds a SPARQL conformance getter for a relation whose `target` model
 * is known but no explicit `getter` string was supplied.
 *
 * The generated getter traverses outgoing links matching the relation's
 * predicate and then filters the target nodes to only those that conform to
 * the target model's shape (required properties / flags).
 *
 * Delegates to the shared `buildConformanceFilter()` utility in decorators.ts
 * so the same logic is used at shape-definition time and at query time.
 *
 * @param relationPredicate - The relation's predicate URI (e.g. "flux://entry_type")
 * @param targetClass       - The target model class (result of calling the `target()` thunk)
 * @returns A SPARQL expression string, or `undefined` if no conformance
 *          conditions could be derived from the target model.
 */
export function buildConformanceGetter(
  relationPredicate: string,
  targetClass: any
): string | undefined {
  const filter = buildConformanceFilter(relationPredicate, targetClass);
  if (!filter) {
    const targetName = targetClass?.prototype?.className || targetClass?.name || 'unknown';
    console.warn(`[Ad4mModel] buildConformanceGetter: no conditions found for target "${targetName}".`);
    return undefined;
  }
  return filter.getter;
}

/**
 * Evaluates custom SurrealQL getters for properties and relations on a specific instance.
 *
 * For relations that declare a `target` but no explicit `getter`, a conformance
 * getter is auto-generated from the target model's metadata (unless `filter: false`).
 */

export async function evaluateCustomGettersForInstance(
  instance: any,
  perspective: PerspectiveProxy,
  metadata: ModelMetadata,
  options?: { requestedProperties?: string[]; include?: Record<string, any>; skipPropertyGetters?: boolean }
): Promise<void> {
  const safeBaseExpression = `<${instance.id}>`;

  // Build projection filter — when requestedProperties is active, only
  // evaluate getters for fields that are requested (or included).
  const projectionActive = options?.requestedProperties && options.requestedProperties.length > 0;
  const projectionSet = projectionActive ? new Set(options!.requestedProperties) : null;

  // Evaluate property getters (skip when caller only wants relation type-filtering)
  if (!options?.skipPropertyGetters) {
  for (const [propName, propMeta] of Object.entries(metadata.properties)) {
    if (projectionSet && !projectionSet.has(propName)) continue;
    if ((propMeta as any).getter) {
      try {
        const rawGetter = (propMeta as any).getter;
        const getterWithBase = rawGetter.replace(/\?source\b/g, `<${instance.id}>`).replace(/<Base>/g, `<${instance.id}>`).replace(/Base/g, `<${instance.id}>`);
        
        // If the getter is already SPARQL (starts with SELECT/ASK/CONSTRUCT), execute directly
        const trimmed = getterWithBase.trim().toUpperCase();
        if (trimmed.startsWith('SELECT') || trimmed.startsWith('ASK') || trimmed.startsWith('CONSTRUCT')) {
          const result = await perspective.querySparql(getterWithBase);
          if (trimmed.startsWith('ASK')) {
            instance[propName] = result === true || result === 'true';
          } else if (result && result.length > 0) {
            // Get first binding's first value
            const firstRow = result[0];
            const firstKey = Object.keys(firstRow)[0];
            const val = firstRow[firstKey]?.value ?? firstRow[firstKey];
            if (val !== undefined && val !== null && val !== 'None' && val !== '') {
              instance[propName] = val;
            }
          }
        } else {
          // Legacy SurrealDB-style getter — safety guard for getters that don't match
          // SPARQL patterns (SELECT/ASK). Logs a warning but does not crash.
          console.warn(`Unsupported legacy getter syntax for property ${propName} — use native SPARQL (SELECT/ASK): ${rawGetter.slice(0, 100)}`);
        }
      } catch (error) {
        console.warn(`Failed to evaluate getter for ${propName}:`, error);
      }
    }
  }
  } // end skipPropertyGetters guard

  // Evaluate relation getters (explicit or auto-generated from target)
  for (const [relName, relMeta] of Object.entries(metadata.relations)) {
    // Skip relations excluded by property projection (unless in include map)
    if (projectionSet && !projectionSet.has(relName) && !(options?.include && relName in options.include)) continue;
    const meta = relMeta as RelationMetadata;

    // Determine the getter to execute:
    // 1. Explicit `getter` always wins
    // 2. If `target` is set and `filter !== false`, auto-generate conformance getter
    //    BUT skip auto-generation for reverse relations (belongsToMany / belongsToOne)
    //    because buildConformanceGetter traverses outgoing links which is wrong for
    //    reverse relations. Their values are already populated by the reverse link
    //    lookup in instancesFromQueryResult / getData.
    let getter = meta.getter;
    if (!getter && meta.where && meta.direction !== 'reverse') {
      try {
        const TargetClass = meta.target?.();
        const targetMetadata = TargetClass
          ? (TargetClass as any).getModelMetadata?.() ?? null
          : null;

        // Check if all where conditions can be SPARQL-filtered.
        // literal-resolved properties store signed expressions (literal:json:{…}),
        // not raw literal:string: IRIs, so SPARQL exact-match fails for them.
        let allSparqlFilterable = true;
        if (targetMetadata) {
          for (const propName of Object.keys(meta.where)) {
            if (['id', 'author', 'timestamp'].includes(propName)) continue;
            const propMeta = targetMetadata.properties[propName];
            if (propMeta && (!propMeta.resolveLanguage || propMeta.resolveLanguage === 'literal')) {
              allSparqlFilterable = false;
              break;
            }
          }
        } else {
          allSparqlFilterable = false;
        }

        if (allSparqlFilterable) {
          const conditions = compileWhereClause(meta.where, targetMetadata);
          if (conditions.length > 0) {
            const escapedPredicate = escapeQueryString(meta.predicate);
            getter = `SELECT ?target WHERE { <Base> <${escapedPredicate}> ?target . ${conditions.join(' ')} }`;
          }
        }
        // If not filterable, fall through to buildConformanceGetter + JS post-filter below
      } catch (e) {
        console.warn(`[Ad4mModel] where-clause compilation failed for relation "${relName}":`, e);
      }
    }
    if (!getter && meta.target && meta.filter !== false && meta.direction !== 'reverse') {
      try {
        const TargetClass = meta.target();
        getter = buildConformanceGetter(meta.predicate, TargetClass);
        if (!getter) {
          console.warn(`[Ad4mModel] buildConformanceGetter returned undefined for relation "${relName}" (predicate: "${meta.predicate}")`);
        }
      } catch (e) {
        console.warn(`[Ad4mModel] auto-generation failed for relation "${relName}":`, e);
      }
    }

    if (getter) {
      try {
        const getterWithBase = getter.replace(/\?source\b/g, `<${instance.id}>`).replace(/<Base>/g, `<${instance.id}>`).replace(/\bBase\b/g, `<${instance.id}>`);
        const trimmed = getterWithBase.trim().toUpperCase();
        
        if (trimmed.startsWith('SELECT') || trimmed.startsWith('ASK') || trimmed.startsWith('CONSTRUCT')) {
          // Native SPARQL getter — execute directly
          const result = await perspective.querySparql(getterWithBase);
          if (result && result.length > 0) {
            const values = result.map((r: any) => {
              const firstKey = Object.keys(r)[0];
              return r[firstKey]?.value ?? r[firstKey];
            }).filter((v: any) => v !== undefined && v !== null && v !== '' && v !== 'None');
            instance[relName] = values;
          }
        } else {
          // Legacy SurrealDB-style getter — safety guard for relation getters that don't
          // match SPARQL patterns (SELECT/ASK). Logs a warning but does not crash.
          console.warn(`Unsupported legacy getter syntax for relation ${relName} — use native SPARQL (SELECT/ASK): ${getter.slice(0, 100)}`);
        }
      } catch (error) {
        console.warn(`Failed to evaluate getter for ${relName}:`, error);
      }
    }

    // JS post-filter for where conditions that couldn't be SPARQL-filtered
    // (e.g. literal-resolved properties storing signed expressions)
    const currentValues = instance[relName];
    if (meta.where && Array.isArray(currentValues) && currentValues.length > 0) {
      const TargetClass = meta.target?.();
      if (TargetClass) {
        const filtered: string[] = [];
        for (const targetId of currentValues) {
          try {
            const inst = new (TargetClass as any)(perspective, targetId);
            await inst.get();
            let matches = true;
            for (const [prop, expected] of Object.entries(meta.where)) {
              if ((inst as any)[prop] !== expected) { matches = false; break; }
            }
            if (matches) filtered.push(targetId);
          } catch {
            // If we can't hydrate, keep the value (conservative)
            filtered.push(targetId);
          }
        }
        instance[relName] = filtered;
      }
    }
  }
}
