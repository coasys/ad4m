/**
 * Single-instance hydration pipeline for Ad4mModel.
 *
 * Extracted from Ad4mModel.getData() (Phase 3a Part 5).
 *
 * `fetchInstanceData()` queries SurrealDB for all links belonging to one
 * instance and populates the instance in-place:
 *  1. Forward-link query → shared `hydrateInstanceFromLinks`
 *  2. relatedModel eager hydration via `_findAllInternal`
 *  3. Reverse-relation batch query
 *  4. Custom SurrealQL getters (`evaluateCustomGetters`)
 */

import type { PerspectiveProxy } from "../../perspectives/PerspectiveProxy";
import type { ModelMetadata, IncludeMap, Query } from "../types";
import { formatSurrealValue } from "./surrealCompiler";
import { hydrateInstanceFromLinks, evaluateCustomGetters } from "./hydration";
import { captureSnapshot } from "./snapshot";
import { _findAllInternal } from "./operations";

/**
 * Hydrates `instance` in-place from SurrealDB and returns it.
 *
 * @param instance       - The Ad4mModel instance to populate
 * @param perspective    - Perspective that owns the instance
 * @param id             - The instance's base expression URI
 * @param metadata       - Pre-resolved model metadata (from `getModelMetadata()`)
 * @param include        - Optional eager-load map
 */
export async function fetchInstanceData(
  instance: any,
  perspective: PerspectiveProxy,
  id: string,
  metadata: ModelMetadata,
  include?: IncludeMap,
): Promise<any> {
  try {
    const safeBase = formatSurrealValue(id);

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

      // ── 2. relatedModel eager hydration ──────────────────────────────────
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
                ? (hydrated[0] ?? null)
                : hydrated;
          } catch (e) {
            console.warn(`Failed to hydrate ${relationName}:`, e);
            instance[relationName] =
              (relationMeta as any).maxCount === 1
                ? (values[0] ?? null)
                : values;
          }
        } else {
          instance[relationName] =
            (relationMeta as any).maxCount === 1 ? (values[0] ?? null) : values;
        }
      }
    }

    // ── 3. Reverse relations ────────────────────────────────────────────────
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
                ? (hydrated[0] ?? null)
                : hydrated;
          } catch (e) {
            instance[relationName] =
              (relationMeta as any).maxCount === 1
                ? (values[0] ?? null)
                : values;
          }
        } else {
          instance[relationName] =
            (relationMeta as any).maxCount === 1 ? (values[0] ?? null) : values;
        }
      }
    }

    // ── 4. Custom SurrealQL getters ─────────────────────────────────────────
    await evaluateCustomGetters(instance, perspective, metadata);

    // ── 5. Snapshot capture — baseline for dirty tracking on next save() ────
    const schemaKeys = [
      ...Object.keys(metadata.properties),
      ...Object.keys(metadata.relations),
    ];
    captureSnapshot(instance, schemaKeys);
  } catch (e) {
    console.error(`SurrealDB getData failed for ${id}:`, e);
  }

  return instance;
}
