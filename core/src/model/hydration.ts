/**
 * hydration.ts — Instance hydration helpers extracted from Ad4mModel.
 *
 * Pure / stateless functions that populate Ad4mModel instances from
 * raw link arrays, Prolog tuples, or query results.  None of these
 * functions depend on the `Ad4mModel` class at runtime — they accept
 * the instance, metadata, or perspective as explicit parameters.
 */

import { Literal } from "../Literal";
import { LinkQuery } from "../perspectives/LinkQuery";
import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { getPropertiesMetadata, getRelationsMetadata, buildConformanceFilter } from "./decorators";
import type { RelationMetadataEntry } from "./decorators";
import { compileWhereClause } from "./query-utils";
import { escapeQueryString } from "../utils";
import type {
  PropertyMetadata, RelationMetadata, ModelMetadata,
  ValueTuple, WhereCondition, IncludeMap, RelationSubQuery,
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
      !target.startsWith('literal:')
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
      target.startsWith('literal:')
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
  options?: { requestedProperties?: string[]; include?: Record<string, any> }
): Promise<void> {
  const safeBaseExpression = `<${instance.id}>`;

  // Build projection filter — when requestedProperties is active, only
  // evaluate getters for fields that are requested (or included).
  const projectionActive = options?.requestedProperties && options.requestedProperties.length > 0;
  const projectionSet = projectionActive ? new Set(options!.requestedProperties) : null;

  // Evaluate property getters
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
          // Legacy SurrealDB-style getter — no longer supported
          console.warn(`Unsupported legacy getter syntax for property ${propName} — use native SPARQL (SELECT/ASK): ${rawGetter.slice(0, 100)}`);
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
          // Legacy SurrealDB-style getter — no longer supported
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

/**
 * Hydrates relation fields on instances according to the provided IncludeMap.
 *
 * For each relation listed in `includeMap`, the raw expression-URI strings
 * stored on the instance are replaced with fully-hydrated model instances
 * (fetched via the relation's `target()` class).  Nested IncludeMaps are
 * supported for multi-level eager loading.
 *
 * @param modelClass - The model class whose relation metadata to read
 * @param instances - The instances whose relations should be hydrated
 * @param perspective - The perspective to fetch related instances from
 * @param includeMap - Describes which relations to hydrate
 */
export async function hydrateRelations<T>(
  modelClass: any,
  instances: T[],
  perspective: PerspectiveProxy,
  includeMap: IncludeMap | undefined,
): Promise<void> {
  if (!includeMap || Object.keys(includeMap).length === 0) return;

  const relMeta = getRelationsMetadata(modelClass);

  for (const [relName, includeValue] of Object.entries(includeMap)) {
    const meta: RelationMetadataEntry | undefined = relMeta[relName];
    if (!meta) {
      console.warn(`include: relation "${relName}" not found in metadata, skipping`);
      continue;
    }

    const TargetClass = meta.target() as any;

    // Determine if a RelationSubQuery was supplied (object) vs a plain `true`
    const subQuery: RelationSubQuery | undefined =
      typeof includeValue === 'object' && includeValue !== null
        ? (includeValue as RelationSubQuery)
        : undefined;
    const nestedInclude: IncludeMap | undefined = subQuery?.include;

    // ── Reverse relations (belongsToOne / belongsToMany) ──────────────────
    // The link goes target→instance, so we query backwards:
    //   predicate = meta.predicate, target = inst.id  →  source is the related id
    if (meta.kind === 'belongsToOne' || meta.kind === 'belongsToMany') {
      // Per-instance reverse lookup (can't batch easily across instances)
      for (const inst of instances) {
        const reverseLinks = await perspective.get(
          new LinkQuery({ predicate: meta.predicate, target: (inst as any).id })
        );
        // Defensive filter: perspective.get may return extra results; ensure
        // we only use links that genuinely point to this instance.
        const sourceIds = reverseLinks
          .filter(l => l.data.target === (inst as any).id)
          .map(l => l.data.source);

        if (meta.kind === 'belongsToOne') {
          if (sourceIds.length === 0) {
            (inst as any)[relName] = null;
            continue;
          }
          const sourceId = sourceIds[sourceIds.length - 1]; // latest-wins
          try {
            const related = new TargetClass(perspective, sourceId);
            await related.get();
            (inst as any)[relName] = related;
          } catch {
            (inst as any)[relName] = null;
          }
        } else {
          // belongsToMany — return array of hydrated instances
          let hydrated: any[] = [];

          // If there's a where/order sub-query, delegate to findAll for filtering
          if (subQuery && (subQuery.where || subQuery.order || subQuery.properties)) {
            const whereWithIds: Record<string, any> = {
              id: sourceIds,
              ...(subQuery.where ?? {}),
            };
            hydrated = await TargetClass.findAll(perspective, {
              where: whereWithIds as any,
              ...(subQuery.order && { order: subQuery.order as any }),
              ...(subQuery.properties && { properties: subQuery.properties }),
            });
          } else {
            await Promise.all(sourceIds.map(async (sid: string) => {
              try {
                const related = new TargetClass(perspective, sid);
                await related.get(
                  subQuery?.properties ? { properties: subQuery.properties } : undefined
                );
                hydrated.push(related);
              } catch { /* skip */ }
            }));
          }

          // Apply order (client-side, if not already handled by findAll above)
          if (subQuery?.order && !(subQuery.where || subQuery.properties)) {
            const orderEntries = Object.entries(subQuery.order);
            hydrated = hydrated.sort((a: any, b: any) => {
              for (const [field, dir] of orderEntries) {
                const av = String(a[field] ?? '');
                const bv = String(b[field] ?? '');
                const diff = av.localeCompare(bv);
                if (diff !== 0) return dir === 'DESC' ? -diff : diff;
              }
              return 0;
            });
          }

          // Apply offset and limit
          if (subQuery?.offset != null || subQuery?.limit != null) {
            const start = subQuery.offset ?? 0;
            const end = subQuery.limit != null ? start + subQuery.limit : undefined;
            hydrated = hydrated.slice(start, end);
          }

          (inst as any)[relName] = hydrated;

          // Recurse for nested includes
          if (nestedInclude && hydrated.length > 0) {
            await hydrateRelations(TargetClass, hydrated, perspective, nestedInclude);
          }
        }
      }
      continue; // skip the forward-relation path below
    }

    // ── Forward relations (hasMany / hasOne) ──────────────────────────────
    // Collect all unique URIs across all instances for batch-friendly lookup.
    // IMPORTANT: We cache each instance's raw value NOW (before the async
    // findAll) so that concurrent getData() calls on the same instance can't
    // overwrite the relation field between the uriSet relation and the
    // post-await assignment loop.  Without this cache, a concurrent call
    // can replace the raw URI strings with hydrated model objects, causing
    // the `typeof v === 'string'` check to fail and the array to end up empty.
    const uriSet = new Set<string>();
    const rawCache = new Map<T, any>();
    for (const inst of instances) {
      const raw = (inst as any)[relName];
      rawCache.set(inst, Array.isArray(raw) ? [...raw] : raw);
      if (raw == null) continue;
      if (Array.isArray(raw)) {
        for (const v of raw) {
          if (typeof v === 'string') uriSet.add(v);
          // Handle already-hydrated instances (from a concurrent call)
          else if (v && typeof v === 'object' && typeof v.id === 'string') uriSet.add(v.id);
        }
      } else if (typeof raw === 'string') {
        uriSet.add(raw);
      } else if (raw && typeof raw === 'object' && typeof raw.id === 'string') {
        uriSet.add(raw.id);
      }
    }

    if (uriSet.size === 0) continue;

    // Hydrate related instances using findAll to ensure conformance checking.
    // findAll validates model membership via graph traversal (required predicates / flags),
    // so non-conforming linked URIs are silently dropped — matching the documented behaviour.
    const hydrated = new Map<string, any>();

    const whereWithIds: Record<string, any> = {
      id: Array.from(uriSet),
      ...(subQuery?.where ?? {}),
    };
    const fetchQuery: any = {
      where: whereWithIds,
      ...(subQuery?.order && { order: subQuery.order }),
      ...(subQuery?.properties && { properties: subQuery.properties }),
    };
    const results = await TargetClass.findAll(perspective, fetchQuery);
    for (const result of results) {
      hydrated.set(result.id, result);
    }

    // Replace raw URIs with hydrated instances on each parent instance.
    // Use the cached raw values captured BEFORE the async findAll to avoid
    // the race condition where a concurrent getData() already replaced the
    // strings with hydrated objects.
    for (const inst of instances) {
      const raw = rawCache.get(inst);
      if (raw == null) continue;
      if (Array.isArray(raw)) {
        // Map URIs → instances; handle both raw strings and already-hydrated objects
        let resolved: any[] = raw
          .map((v: any) => {
            if (typeof v === 'string') {
              return hydrated.has(v) ? hydrated.get(v)! : null;
            }
            // Already a hydrated model instance (from a concurrent call)
            if (v && typeof v === 'object' && typeof v.id === 'string') {
              return hydrated.has(v.id) ? hydrated.get(v.id)! : v;
            }
            return null;
          })
          .filter((v: any): v is any => v !== null);

        // Per-instance sort (client-side, after filtering)
        if (subQuery?.order) {
          const orderEntries = Object.entries(subQuery.order);
          resolved = resolved.sort((a: any, b: any) => {
            for (const [field, dir] of orderEntries) {
              const av = String(a[field] ?? '');
              const bv = String(b[field] ?? '');
              const diff = av.localeCompare(bv);
              if (diff !== 0) return dir === 'DESC' ? -diff : diff;
            }
            return 0;
          });
        }

        // Per-instance limit / offset
        if (subQuery?.offset != null || subQuery?.limit != null) {
          const start = subQuery.offset ?? 0;
          const end =
            subQuery.limit != null ? start + subQuery.limit : undefined;
          resolved = resolved.slice(start, end);
        }

        // Enforce maxCount guard — single-valued relations keep only the last item
        if (meta.maxCount === 1) {
          if (resolved.length > 1) {
            console.warn(
              `include: relation "${relName}" has maxCount 1 but ${resolved.length} values found; keeping the last`,
            );
          }
          (inst as any)[relName] = resolved.length > 0
            ? resolved[resolved.length - 1]
            : null;
        } else {
          (inst as any)[relName] = resolved;
        }
      } else if (typeof raw === 'string' && hydrated.has(raw)) {
        (inst as any)[relName] = hydrated.get(raw);
      } else if (raw && typeof raw === 'object' && typeof raw.id === 'string') {
        // Already-hydrated object — look up refreshed version or keep as-is
        (inst as any)[relName] = hydrated.has(raw.id) ? hydrated.get(raw.id) : raw;
      }
    }

    // Recurse for nested includes
    if (nestedInclude) {
      const hydratedInstances = Array.from(hydrated.values());
      await hydrateRelations(TargetClass, hydratedInstances, perspective, nestedInclude);
    }
  }
}
