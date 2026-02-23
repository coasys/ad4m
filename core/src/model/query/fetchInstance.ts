/**
 * Single-instance hydration pipeline for Ad4mModel.
 *
 * Extracted from Ad4mModel.getData() (Phase 3a Part 5).
 *
 * `fetchInstanceData()` queries SurrealDB for all links belonging to one
 * instance and populates the instance in-place:
 *  1. Forward-link query → shared `hydrateInstanceFromLinks`
 *  2. Post-filters: where.condition (per-value SurrealQL), where.isInstance
 *  3. relatedModel eager hydration via `_findAllInternal`
 *  4. Reverse-relation batch query
 *  5. Custom SurrealQL getters (`evaluateCustomGetters`)
 *  6. where.isInstance filtering on getter-backed relations
 */

import type { PerspectiveProxy } from "../../perspectives/PerspectiveProxy";
import type { ModelMetadata, IncludeMap, Query } from "../types";
import { formatSurrealValue } from "./SurrealQueryBuilder";
import { hydrateInstanceFromLinks, evaluateCustomGetters } from "./hydration";
import { _findAllInternal } from "./operations";

/**
 * Hydrates `instance` in-place from SurrealDB and returns it.
 *
 * @param instance     - The Ad4mModel instance to populate
 * @param perspective  - Perspective that owns the instance
 * @param baseExpression - The instance's base expression URI
 * @param metadata     - Pre-resolved model metadata (from `getModelMetadata()`)
 */
export async function fetchInstanceData(
  instance: any,
  perspective: PerspectiveProxy,
  baseExpression: string,
  metadata: ModelMetadata,
  include?: IncludeMap,
): Promise<any> {
  try {
    const safeBase = formatSurrealValue(baseExpression);

    // ── 1. Forward links ────────────────────────────────────────────────────
    const links = await perspective.querySurrealDB(`
      SELECT id, predicate, out.uri AS target, author, timestamp
      FROM link
      WHERE in.uri = ${safeBase}
      ORDER BY timestamp ASC
    `);

    if (links && links.length > 0) {
      // ── 2. Shared hydration: properties + forward relations + timestamps ──
      await hydrateInstanceFromLinks(instance, links, metadata, perspective);

      // ── 3. Post-filters on forward relations ─────────────────────────────
      const forwardRelations = Object.entries(metadata.relations).filter(
        ([, m]: [string, any]) => !m.getter && m.direction !== "reverse",
      );

      for (const [relationName, relationMeta] of forwardRelations) {
        const current = instance[relationName];
        let values: string[] = Array.isArray(current)
          ? [...current]
          : current != null
            ? [current as string]
            : [];

        // where.condition: per-value SurrealQL evaluation
        if ((relationMeta as any).where?.condition && values.length > 0) {
          try {
            const filteredValues: string[] = [];
            for (const value of values) {
              let condition = (relationMeta as any).where.condition
                .replace(/\$perspective/g, `'${perspective.uuid}'`)
                .replace(/\$base/g, `'${baseExpression}'`)
                .replace(/Target/g, `'${value.replace(/'/g, "'")}'`);
              if (condition.trim().startsWith("WHERE")) {
                condition = `array::len(SELECT * FROM link ${condition}) > 0`;
              }
              const result = await perspective.querySurrealDB(
                `RETURN ${condition}`,
              );
              const isTrue =
                result === true ||
                (Array.isArray(result) &&
                  result.length > 0 &&
                  result[0] === true);
              if (isTrue) filteredValues.push(value);
            }
            values = filteredValues;
          } catch (error) {
            console.warn(
              `Failed to apply condition filter for ${relationName}:`,
              error,
            );
          }
        }

        // where.isInstance: batch subject-class check
        if ((relationMeta as any).where?.isInstance && values.length > 0) {
          try {
            const targetClass = (relationMeta as any).where.isInstance;
            const className =
              typeof targetClass === "string" ? targetClass : targetClass.name;
            const filterMetadata =
              await perspective.getSubjectClassMetadataFromSDNA(className);
            if (filterMetadata) {
              values = await perspective.batchCheckSubjectInstances(
                values,
                filterMetadata,
              );
            }
          } catch {
            // keep unfiltered on error
          }
        }

        // relatedModel: eager hydration — only when caller asked for it via include
        const includeEntry = include?.[relationName];
        if (
          includeEntry !== undefined &&
          (relationMeta as any).relatedModel &&
          values.length > 0
        ) {
          try {
            const RelatedModel = (relationMeta as any).relatedModel() as any;
            const subQuery: Query =
              includeEntry === true
                ? { where: { id: values } }
                : {
                    ...includeEntry,
                    where: { id: values, ...(includeEntry as Query).where },
                  };
            const hydrated = await _findAllInternal(
              RelatedModel,
              perspective,
              subQuery,
              false,
            );
            instance[relationName] =
              (relationMeta as any).maxCount === 1
                ? hydrated[0] ?? null
                : hydrated;
          } catch (e) {
            console.warn(`Failed to hydrate ${relationName}:`, e);
            instance[relationName] =
              (relationMeta as any).maxCount === 1 ? values[0] ?? null : values;
          }
        } else {
          instance[relationName] =
            (relationMeta as any).maxCount === 1 ? values[0] ?? null : values;
        }
      }
    }

    // ── 4. Reverse relations ────────────────────────────────────────────────
    const reverseRelations = Object.entries(metadata.relations).filter(
      ([, m]: [string, any]) => !m.getter && m.direction === "reverse",
    );
    if (reverseRelations.length > 0) {
      let reverseLinks: any[] = [];
      try {
        reverseLinks =
          (await perspective.querySurrealDB(`
            SELECT in.uri AS source, predicate, id, author, timestamp
            FROM link
            WHERE out.uri = ${safeBase}
            ORDER BY timestamp ASC
          `)) ?? [];
      } catch {
        // leave empty — instance just won't have reverse relation data
      }

      for (const [relationName, relationMeta] of reverseRelations) {
        const matching = reverseLinks.filter(
          (l: any) => l.predicate === (relationMeta as any).predicate,
        );
        const values = matching.map((l: any) => l.source);

        const includeEntry = include?.[relationName];
        if (
          includeEntry !== undefined &&
          (relationMeta as any).relatedModel &&
          values.length > 0
        ) {
          try {
            const RelatedModel = (relationMeta as any).relatedModel() as any;
            const subQuery: Query =
              includeEntry === true
                ? { where: { id: values } }
                : {
                    ...includeEntry,
                    where: { id: values, ...(includeEntry as Query).where },
                  };
            const hydrated = await _findAllInternal(
              RelatedModel,
              perspective,
              subQuery,
              false,
            );
            instance[relationName] =
              (relationMeta as any).maxCount === 1
                ? hydrated[0] ?? null
                : hydrated;
          } catch (e) {
            instance[relationName] =
              (relationMeta as any).maxCount === 1 ? values[0] ?? null : values;
          }
        } else {
          instance[relationName] =
            (relationMeta as any).maxCount === 1 ? values[0] ?? null : values;
        }
      }
    }

    // ── 5. Custom SurrealQL getters ─────────────────────────────────────────
    await evaluateCustomGetters(instance, perspective, metadata);

    // ── 6. where.isInstance filtering for getter-backed relations ───────────
    for (const [relationName, relationMeta] of Object.entries(
      metadata.relations,
    )) {
      if (
        (relationMeta as any).getter &&
        (relationMeta as any).where?.isInstance &&
        instance[relationName]?.length > 0
      ) {
        try {
          const targetClass = (relationMeta as any).where.isInstance;
          const className =
            typeof targetClass === "string" ? targetClass : targetClass.name;
          const filterMetadata =
            await perspective.getSubjectClassMetadataFromSDNA(className);
          if (filterMetadata) {
            instance[relationName] =
              await perspective.batchCheckSubjectInstances(
                instance[relationName],
                filterMetadata,
              );
          }
        } catch {
          // keep unfiltered on error
        }
      }
    }
  } catch (e) {
    console.error(`SurrealDB getData failed for ${baseExpression}:`, e);
  }

  return instance;
}
