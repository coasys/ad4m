/**
 * hydration.ts — Instance hydration helpers extracted from Ad4mModel.
 *
 * Pure / stateless functions that populate Ad4mModel instances from
 * raw link arrays, Prolog tuples, or SurrealDB rows.  None of these
 * functions depend on the `Ad4mModel` class at runtime — they accept
 * the instance, metadata, or perspective as explicit parameters.
 */

import { Literal } from "../Literal";
import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { getPropertiesMetadata, buildConformanceFilter } from "./decorators";
import { escapeSurrealString } from "../utils";
import { formatSurrealValue, compileWhereClause } from "./surreal-utils";
import type {
  PropertyMetadata, RelationMetadata, ModelMetadata,
  ValueTuple, WhereCondition,
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
//  Property / link hydration
// ──────────────────────────────────────────────────────────

/**
 * Resolves a single property value from a raw link target string.
 *
 * Handles expression resolution, literal URI parsing, primitive
 * type coercion, and optional transform functions.
 *
 * @param target        - The raw target string from the link
 * @param propMeta      - Property metadata from the decorator registry
 * @param perspective   - The perspective for expression resolution
 * @param expectedType  - Optional JS typeof hint for coercion (e.g. 'number')
 * @returns The resolved value
 */
export async function hydratePropertyValue(
  target: string,
  propMeta: PropertyMetadata,
  perspective: PerspectiveProxy,
  expectedType?: string,
): Promise<any> {
  let value: any = target;

  if (target !== undefined && target !== null && target !== '') {
    // Non-literal expression resolution
    if (
      propMeta.resolveLanguage != null &&
      propMeta.resolveLanguage !== 'literal' &&
      typeof target === 'string' &&
      !target.startsWith('literal://')
    ) {
      try {
        const expression = await perspective.getExpression(target);
        if (expression) {
          try { value = JSON.parse(expression.data); } catch { value = expression.data; }
        }
      } catch (e) {
        console.warn(`hydratePropertyValue: failed to resolve expression for "${propMeta.name}":`, e);
      }
    }
    // Literal URI parsing
    else if (
      propMeta.resolveLanguage === 'literal' &&
      typeof target === 'string' &&
      target.startsWith('literal://')
    ) {
      try {
        const parsed = Literal.fromUrl(target).get();
        value = parsed.data !== undefined ? parsed.data : parsed;
      } catch {
        // Keep raw value
      }
    }
    // Primitive type coercion
    else if (typeof target === 'string' && expectedType) {
      if (expectedType === 'number') value = Number(target);
      else if (expectedType === 'boolean') value = target === 'true' || target === '1';
    }
  }

  // Transform function
  if (propMeta.transform && typeof propMeta.transform === 'function') {
    value = propMeta.transform(value);
  }

  return value;
}

/**
 * Hydrates an instance from an array of raw links.
 *
 * Processes properties (latest-wins semantics), relations
 * (chronological accumulation), and timestamps/author in a single
 * pass over the links array.
 *
 * @param instance     - The blank model instance to populate
 * @param links        - Array of link objects (predicate, target, author?, timestamp?)
 * @param metadata     - Model metadata from `getModelMetadata()`
 * @param perspective  - The perspective for expression resolution
 * @param requestedProperties - Optional sparse fieldset; when provided, only these
 *                              property names are hydrated (relations are unaffected).
 *                              Omit or pass `undefined` to hydrate all properties.
 */
export async function hydrateFromLinks(
  instance: any,
  links: Array<{ predicate: string; target: string; author?: string; timestamp?: string | number }>,
  metadata: ModelMetadata,
  perspective: PerspectiveProxy,
  requestedProperties?: string[],
): Promise<void> {
  if (!links || links.length === 0) return;

  let minTimestamp: string | number | null = null;
  let maxTimestamp: string | number | null = null;
  let originalAuthor: string | null = null;
  let latestAuthor: string | null = null;

  // Build predicate→propName and predicate→relName lookup maps for O(1) matching
  // When requestedProperties is provided, only include those properties in the map
  const propFilter = requestedProperties && requestedProperties.length > 0
    ? new Set(requestedProperties)
    : null;

  const predToProperty = new Map<string, [string, PropertyMetadata]>();
  for (const [propName, propMeta] of Object.entries(metadata.properties)) {
    if (propMeta.getter) continue;  // Handled via custom getter evaluation
    if (propFilter && !propFilter.has(propName)) continue;  // Skip unrequested properties
    predToProperty.set(propMeta.predicate, [propName, propMeta]);
  }

  const predToRelation = new Map<string, [string, RelationMetadata]>();

  // Two-pass approach: first add all non-getter, non-target-filtered relations,
  // then try to add target+filter relations only if their predicate isn't
  // already claimed.  This prevents predicate collisions (e.g. entries +
  // messages sharing the same predicate) while still populating unique-
  // predicate relations normally via link hydration.
  const deferredTargetRelations: [string, RelationMetadata][] = [];
  for (const [relName, relMeta] of Object.entries(metadata.relations)) {
    if (relMeta.getter) continue;  // Handled via explicit custom getter evaluation
    if (relMeta.target && relMeta.filter !== false) {
      deferredTargetRelations.push([relName, relMeta]);
      continue;
    }
    predToRelation.set(relMeta.predicate, [relName, relMeta]);
  }
  // Second pass: add deferred target+filter relations only if their predicate
  // is not already claimed by a base relation.  When a collision exists, the
  // conformance getter will handle the filtered view instead.
  for (const [relName, relMeta] of deferredTargetRelations) {
    if (!predToRelation.has(relMeta.predicate)) {
      predToRelation.set(relMeta.predicate, [relName, relMeta]);
    }
  }

  // Per-property accumulator: track all matching targets so we can pick the last (latest-wins)
  const propertyLatest = new Map<string, { target: string; timestamp?: string | number }>();

  // Per-relation accumulator: ordered targets with metadata for sorting
  const relationAccum = new Map<string, Array<{ target: string; timestamp: string | number; index: number }>>();

  // Single pass over all links
  for (let i = 0; i < links.length; i++) {
    const link = links[i];
    const { predicate, target, author, timestamp } = link;
    if (target === 'None' || target === undefined || target === null) continue;

    // Track timestamps/authors
    if (timestamp != null) {
      if (minTimestamp == null || timestamp < minTimestamp) {
        minTimestamp = timestamp;
        originalAuthor = author ?? null;
      }
      if (maxTimestamp == null || timestamp > maxTimestamp) {
        maxTimestamp = timestamp;
        latestAuthor = author ?? null;
      }
    }

    // Property match — accumulate for latest-wins
    const propEntry = predToProperty.get(predicate);
    if (propEntry) {
      const existing = propertyLatest.get(propEntry[0]);
      // Latest-wins: always overwrite (links are ordered ASC so last = latest)
      propertyLatest.set(propEntry[0], { target, timestamp });
      continue;
    }

    // Relation match — accumulate all
    const relEntry = predToRelation.get(predicate);
    if (relEntry) {
      const [relName] = relEntry;
      let arr = relationAccum.get(relName);
      if (!arr) {
        arr = [];
        relationAccum.set(relName, arr);
      }
      arr.push({ target, timestamp: timestamp ?? '', index: i });
    }
  }

  // Resolve properties
  for (const [propName, { target }] of propertyLatest) {
    const [, propMeta] = predToProperty.get(
      metadata.properties[propName].predicate
    )!;
    const expectedType = typeof instance[propName];
    instance[propName] = await hydratePropertyValue(
      target,
      propMeta,
      perspective,
      expectedType !== 'undefined' ? expectedType : undefined,
    );
  }

  // Resolve relations: sort by timestamp (stable via index tiebreaker), filter empties
  for (const [relName, items] of relationAccum) {
    items.sort((a, b) => {
      const cmp = String(a.timestamp).localeCompare(String(b.timestamp));
      return cmp !== 0 ? cmp : a.index - b.index;
    });
    instance[relName] = items
      .map(i => i.target)
      .filter((v: any) => v !== undefined && v !== null && v !== '' && v !== 'None');
  }

  // Assign author / timestamps
  if (originalAuthor) instance.author = originalAuthor;
  if (minTimestamp != null) {
    instance.createdAt = typeof minTimestamp === 'string' && minTimestamp.includes('T')
      ? new Date(minTimestamp).getTime()
      : typeof minTimestamp === 'string'
        ? (isNaN(parseInt(minTimestamp, 10)) ? minTimestamp : parseInt(minTimestamp, 10))
        : minTimestamp;
  }
  if (maxTimestamp != null) {
    instance.updatedAt = typeof maxTimestamp === 'string' && maxTimestamp.includes('T')
      ? new Date(maxTimestamp).getTime()
      : typeof maxTimestamp === 'string'
        ? (isNaN(parseInt(maxTimestamp, 10)) ? maxTimestamp : parseInt(maxTimestamp, 10))
        : maxTimestamp;
  }
}

// ──────────────────────────────────────────────────────────
//  Prolog result hydration (assignValuesToInstance)
// ──────────────────────────────────────────────────────────

/**
 * Assigns decoded Prolog property values to an instance.
 *
 * Handles UTF-8 byte sequence reconstruction, expression resolution,
 * transform functions, and read-only property filtering.
 */
export async function assignValuesToInstance(
  perspective: PerspectiveProxy,
  instance: any,
  values: ValueTuple[],
): Promise<void> {
  // Map properties to object
  const propsObject = Object.fromEntries(
    await Promise.all(
      values.map(async ([name, value, resolve]) => {
        let finalValue = value;

        // Handle UTF-8 byte sequences from Prolog URL decoding
        if (!resolve && typeof value === 'string') {
          // Only attempt reconstruction if the string looks like a byte string (all code points <= 0xFF)
          // and contains at least one high byte (>= 0x80). This avoids mangling valid Unicode.
          const codePoints = Array.from(value, ch => ch.codePointAt(0)!);
          const looksByteString = codePoints.every(cp => cp <= 0xFF);
          const hasHighByte = codePoints.some(cp => cp >= 0x80);
          if (looksByteString && hasHighByte) {
            try {
              const bytes = Uint8Array.from(codePoints);
              const decoded = new TextDecoder('utf-8', { fatal: true }).decode(bytes);
              if (decoded !== value) finalValue = decoded;
            } catch (error) {
              // If UTF-8 conversion fails, keep the original value
              console.warn(`UTF-8 byte reconstruction failed for property "${name}"`, { value, error });
            }
          }
        }

        // Resolve the value if necessary
        if (resolve) {
          let resolvedExpression = await perspective.getExpression(value);
          if (resolvedExpression) {
            try {
              // Attempt to parse the data as JSON
              finalValue = JSON.parse(resolvedExpression.data);
            } catch (error) {
              // If parsing fails, keep the original data
              finalValue = resolvedExpression.data;
            }
          }
        }
        // Apply transform function if it exists
        const propsMeta = getPropertiesMetadata(instance.constructor);
        const transform = propsMeta[name]?.transform;
        if (transform && typeof transform === "function") {
          finalValue = transform(finalValue);
        }
        return [name, finalValue];
      })
    )
  );
  // Filter out properties that are read-only (getters without setters)
  const writableProps = Object.fromEntries(
    Object.entries(propsObject).filter(([key]) => {
      const descriptor = Object.getOwnPropertyDescriptor(Object.getPrototypeOf(instance), key);
      if (!descriptor) {
        // No descriptor means it's a regular property on the instance, allow it
        return true;
      }
      // Check if it's an accessor descriptor (has get/set) vs data descriptor (has value/writable)
      const isAccessor = descriptor.get !== undefined || descriptor.set !== undefined;
      if (isAccessor) {
        // Accessor descriptor: only allow if it has a setter
        return descriptor.set !== undefined;
      } else {
        // Data descriptor: only allow if writable is not explicitly false
        return descriptor.writable !== false;
      }
    })
  );
  // Assign properties to instance
  Object.assign(instance, writableProps);
}

// ──────────────────────────────────────────────────────────
//  SurrealQL custom getter evaluation
// ──────────────────────────────────────────────────────────

/**
 * Builds a SurrealQL conformance getter for a relation whose `target` model
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
 * @returns A SurrealQL expression string, or `undefined` if no conformance
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
  options?: { requestedProperties?: string[]; include?: Record<string, any> }
): Promise<void> {
  const safeBaseExpression = formatSurrealValue(instance.id);

  // Build projection filter — when requestedProperties is active, only
  // evaluate getters for fields that are requested (or included).
  const projectionActive = options?.requestedProperties && options.requestedProperties.length > 0;
  const projectionSet = projectionActive ? new Set(options!.requestedProperties) : null;

  // Evaluate property getters
  for (const [propName, propMeta] of Object.entries(metadata.properties)) {
    if (projectionSet && !projectionSet.has(propName)) continue;
    if ((propMeta as any).getter) {
      try {
        // Replace 'Base' placeholder with actual base expression
        const query = (propMeta as any).getter.replace(/Base/g, safeBaseExpression);
        // Query from node table to have graph traversal context
        const result = await perspective.querySurrealDB(
          `SELECT (${query}) AS value FROM node WHERE uri = ${safeBaseExpression}`
        );
        if (result && result.length > 0 && result[0].value !== undefined && result[0].value !== null && result[0].value !== 'None' && result[0].value !== '') {
          instance[propName] = result[0].value;
        }
      } catch (error) {
        console.warn(`Failed to evaluate getter for ${propName}:`, error);
      }
    }
  }

  // Evaluate relation getters (explicit or auto-generated from target)
  for (const [relName, relMeta] of Object.entries(metadata.relations)) {
    // Skip relations excluded by property projection (unless in include map)
    if (projectionSet && !projectionSet.has(relName) && !(options?.include && relName in options.include)) continue;
    const meta = relMeta as RelationMetadata;

    // Determine the getter to execute:
    // 1. Explicit `getter` always wins
    // 2. `where` clause → compile DSL to SurrealQL getter
    // 3. If `target` is set and `filter !== false`, auto-generate from target metadata
    //    BUT skip auto-generation for reverse relations (belongsToMany / belongsToOne)
    //    because buildConformanceGetter traverses outgoing links (->link) which is
    //    wrong for reverse relations. Their values are already populated by the
    //    reverse link lookup in instancesFromSurrealResult / getData.
    let getter = meta.getter;
    if (!getter && meta.where && meta.direction !== 'reverse') {
      try {
        const TargetClass = meta.target?.();
        const targetMetadata = TargetClass
          ? (TargetClass as any).getModelMetadata?.() ?? null
          : null;
        const conditions = compileWhereClause(meta.where, targetMetadata);
        if (conditions.length > 0) {
          const escapedPred = escapeSurrealString(meta.predicate);
          getter = `(->link[WHERE predicate = '${escapedPred}'].out[WHERE ${conditions.join(' AND ')}].uri)`;
        }
      } catch (e) {
        // Target metadata may not be available yet
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
        // Replace 'Base' placeholder with actual base expression
        const query = getter.replace(/Base/g, safeBaseExpression);
        const fullQuery = `SELECT (${query}) AS value FROM node WHERE uri = ${safeBaseExpression}`;
        // Query from node table to have graph traversal context
        const result = await perspective.querySurrealDB(fullQuery);

        if (result && result.length > 0 && result[0].value !== undefined && result[0].value !== null) {
          // Filter out 'None' from relation results
          const value = result[0].value;
          instance[relName] = Array.isArray(value) 
            ? value.filter((v: any) => v !== undefined && v !== null && v !== '' && v !== 'None')
            : value;
        }
      } catch (error) {
        console.warn(`Failed to evaluate getter for ${relName}:`, error);
      }
    }
  }
}
