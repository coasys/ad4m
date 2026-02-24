/**
 * Mutation helpers for Ad4mModel — extracted Phase 3c.
 *
 * All persistence functions that previously lived as private methods on Ad4mModel.
 * Each takes a `MutationContext` (perspective, id, instance)
 * instead of using `this`, making them independently testable and composable.
 *
 * Functions exported here cover the full write path:
 *   - Pure action builders: `generatePropertySetterAction`, `generateRelationAction`
 *   - Per-field setters: `setProperty`, `setRelationSetter/Adder/Remover`
 *   - Instance-level persistence: `cleanCopy`, `innerUpdate`, `saveInstance`
 */

import { Literal } from "../Literal";
import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import type { PropertyOptions, RelationOptions } from "./decorators";
import {
  getPropertiesMetadata,
  getRelationsMetadata,
  propertyRegistry,
} from "./decorators";
import { formatSurrealValue } from "./query/surrealCompiler";
import { fetchInstanceData } from "./query/fetchInstance";
import { getModelMetadata as _getModelMetadata } from "./schema/metadata";

// ── Context ────────────────────────────────────────────────────────────────

/**
 * Bundles the per-instance state needed by every mutation function,
 * replacing the private `this.#*` fields from Ad4mModel.
 */
export interface MutationContext {
  /** The perspective that owns this instance. */
  perspective: PerspectiveProxy;
  /** URI of the instance's root node in the graph (the base expression). */
  id: string;
  /** The Ad4mModel instance itself (for `Object.entries`, prototype lookups, etc.) */
  instance: any;
}

// ── Pure action builders ───────────────────────────────────────────────────

/**
 * Builds a `setSingleTarget` action descriptor from property metadata.
 * Throws if the property is a flag, read-only, or has no predicate.
 */
export function generatePropertySetterAction(
  key: string,
  metadata: PropertyOptions,
): any[] {
  if (metadata.flag) {
    throw new Error(
      `Property "${key}" is a @Flag and cannot be set after creation`,
    );
  }
  if (metadata.writable === false) {
    throw new Error(`Property "${key}" is read-only and cannot be written`);
  }
  if (!metadata.through) {
    throw new Error(`Property "${key}" has no 'through' predicate defined`);
  }
  return [
    {
      action: "setSingleTarget",
      source: "this",
      predicate: metadata.through,
      target: "value",
      ...(metadata.local && { local: true }),
    },
  ];
}

/**
 * Builds an `addLink` / `removeLink` / `relationSetter` action descriptor
 * from relation metadata.
 */
export function generateRelationAction(
  key: string,
  metadata: RelationOptions,
  actionType: "adder" | "remover" | "setter",
): any[] {
  if (!metadata.through) {
    throw new Error(`Relation "${key}" has no 'through' predicate defined`);
  }
  const actionMap = {
    adder: "addLink",
    remover: "removeLink",
    setter: "relationSetter",
  };
  return [
    {
      action: actionMap[actionType],
      source: "this",
      predicate: metadata.through,
      target: "value",
      ...(metadata.local && { local: true }),
    },
  ];
}

// ── Property / relation setters ────────────────────────────────────────────

/**
 * Persists a single scalar property value for `ctx.instance`.
 *
 * Values that already carry a URI scheme are passed through unchanged.
 * Raw scalars are encoded as `literal://` URIs.
 * When a `resolveLanguage` is set, the value is first stored via `createExpression`.
 */
export async function setProperty(
  ctx: MutationContext,
  key: string,
  value: any,
  batchId?: string,
): Promise<void> {
  const proto = Object.getPrototypeOf(ctx.instance);
  const metadata = getPropertiesMetadata(proto.constructor)?.[key] as
    | PropertyOptions
    | undefined;
  if (!metadata) {
    throw new Error(
      `setProperty called with unknown key "${key}" — ensure the field has a @Property decorator`,
    );
  }

  const actions = generatePropertySetterAction(key, metadata);
  const resolveLanguage = metadata.resolveLanguage;

  // Skip empty/null/undefined to avoid storing invalid empty literals.
  if (value === undefined || value === null || value === "") return;

  if (resolveLanguage) {
    value = await ctx.perspective.createExpression(value, resolveLanguage);
  } else if (
    typeof value !== "string" ||
    !/^[a-zA-Z][a-zA-Z0-9+\-.]*:/.test(value)
  ) {
    // Encode raw scalars as literal:// URIs — mirrors Rust's resolve_property_value.
    value = Literal.from(value).toUrl();
  }

  await ctx.perspective.executeAction(
    actions,
    ctx.id,
    [{ name: "value", value }],
    batchId,
  );
}

/** Normalises an Ad4mModel instance to its id URI, passing other values through unchanged. */
const toId = (v: any): any =>
  v && typeof v === "object" && typeof v.id === "string" ? v.id : v;

/** Sets (replaces) the full set of targets for a relation. */
export async function setRelationSetter(
  ctx: MutationContext,
  key: string,
  value: any,
  batchId?: string,
): Promise<void> {
  const proto = Object.getPrototypeOf(ctx.instance);
  const metadata = getRelationsMetadata(proto.constructor)?.[key] as
    | RelationOptions
    | undefined;
  if (!metadata) {
    console.warn(`Relation "${key}" has no metadata, skipping`);
    return;
  }

  const actions = generateRelationAction(key, metadata, "setter");

  if (value != null) {
    if (Array.isArray(value)) {
      await ctx.perspective.executeAction(
        actions,
        ctx.id,
        value.map((v) => ({ name: "value", value: toId(v) })),
        batchId,
      );
    } else {
      await ctx.perspective.executeAction(
        actions,
        ctx.id,
        [{ name: "value", value: toId(value) }],
        batchId,
      );
    }
  }
}

/** Adds one or more targets to a relation without removing existing ones. */
export async function setRelationAdder(
  ctx: MutationContext,
  key: string,
  value: any,
  batchId?: string,
): Promise<void> {
  const proto = Object.getPrototypeOf(ctx.instance);
  const metadata = getRelationsMetadata(proto.constructor)?.[key] as
    | RelationOptions
    | undefined;
  if (!metadata) {
    console.warn(`Relation "${key}" has no metadata, skipping`);
    return;
  }

  const actions = generateRelationAction(key, metadata, "adder");

  if (value != null) {
    if (Array.isArray(value)) {
      await Promise.all(
        value.map((v) =>
          ctx.perspective.executeAction(
            actions,
            ctx.id,
            [{ name: "value", value: toId(v) }],
            batchId,
          ),
        ),
      );
    } else {
      await ctx.perspective.executeAction(
        actions,
        ctx.id,
        [{ name: "value", value: toId(value) }],
        batchId,
      );
    }
  }
}

/** Removes one or more targets from a relation. */
export async function setRelationRemover(
  ctx: MutationContext,
  key: string,
  value: any,
  batchId?: string,
): Promise<void> {
  const proto = Object.getPrototypeOf(ctx.instance);
  const metadata = getRelationsMetadata(proto.constructor)?.[key] as
    | RelationOptions
    | undefined;
  if (!metadata) {
    console.warn(`Relation "${key}" has no metadata, skipping`);
    return;
  }

  const actions = generateRelationAction(key, metadata, "remover");

  if (value != null) {
    if (Array.isArray(value)) {
      await Promise.all(
        value.map((v) =>
          ctx.perspective.executeAction(
            actions,
            ctx.id,
            [{ name: "value", value: toId(v) }],
            batchId,
          ),
        ),
      );
    } else {
      await ctx.perspective.executeAction(
        actions,
        ctx.id,
        [{ name: "value", value: toId(value) }],
        batchId,
      );
    }
  }
}

// ── Persistence ────────────────────────────────────────────────────────────

/**
 * Returns a shallow copy of `instance` with `null`, `undefined`,
 * `author`, and `timestamp` fields omitted.
 */
export function cleanCopy(instance: any): Record<string, any> {
  const clean: Record<string, any> = {};
  for (const [key, value] of Object.entries(instance)) {
    if (
      value !== undefined &&
      value !== null &&
      key !== "author" &&
      key !== "timestamp"
    ) {
      clean[key] = value;
    }
  }
  return clean;
}

/**
 * Iterates all instance fields and persists each one according to its metadata.
 *
 * - Fields with an `.action` shape → dispatched to the matching relation mutator.
 * - Arrays → treated as relation setters (including empty arrays to clear relations).
 * - Scalar values → written via `setProperty` when `setProperties` is `true` and the
 *   field is not a relation or a flag.
 *
 * @note `#subjectClassName` was written here in the original implementation but
 * is never read anywhere — the write is intentionally omitted.
 */
export async function innerUpdate(
  ctx: MutationContext,
  setProperties: boolean = true,
  batchId?: string,
): Promise<void> {
  const proto = Object.getPrototypeOf(ctx.instance);

  for (const [key, value] of Object.entries(ctx.instance)) {
    if (value !== undefined && value !== null) {
      if ((value as any)?.action) {
        switch ((value as any).action) {
          case "setter":
            await setRelationSetter(ctx, key, (value as any).value, batchId);
            break;
          case "adder":
            await setRelationAdder(ctx, key, (value as any).value, batchId);
            break;
          case "remover":
            await setRelationRemover(ctx, key, (value as any).value, batchId);
            break;
          default:
            await setRelationSetter(ctx, key, (value as any).value, batchId);
            break;
        }
      } else if (Array.isArray(value)) {
        // All arrays (including empty) treated as relation setters.
        await setRelationSetter(ctx, key, value, batchId);
      } else if (value !== undefined && value !== null && value !== "") {
        if (setProperties) {
          // Skip relation fields — they are not scalar properties.
          if (getRelationsMetadata(proto.constructor)?.[key]) continue;
          const propMeta = getPropertiesMetadata(proto.constructor)?.[key];
          // No @Property decorator for this key — skip silently.
          // This covers generated relation methods (addX / removeX / setX)
          // that appear as own enumerable properties on the instance, and
          // base-class fields like `author` / `createdAt` that have no
          // associated predicate.
          if (!propMeta) continue;
          // Skip flag fields — flags are immutable, written once by createSubject.
          if (propMeta.flag) continue;
          await setProperty(ctx, key, value, batchId);
        }
      }
    }
  }
}

/**
 * Persists `ctx.instance` to `ctx.perspective`.
 *
 * Auto-detects create vs update by checking whether any links already exist
 * for `ctx.id`.
 *
 * - **Create path**: `createSubject` → `innerUpdate(false)` (relations only).
 * - **Update path**: `innerUpdate(true)` (properties + relations).
 *
 * @param ctx     - Mutation context (perspective, id, instance).
 * @param batchId - Optional caller-managed batch.  When omitted an internal batch
 *                  is created, committed, and the instance is rehydrated automatically.
 */
export async function saveInstance(
  ctx: MutationContext,
  batchId?: string,
  alreadyExists?: boolean,
): Promise<void> {
  const safeBase = formatSurrealValue(ctx.id);
  // Skip the DB round-trip when the caller already knows the instance was
  // saved once (e.g. the second save() call inside the same uncommitted batch).
  let isNew: boolean;
  if (alreadyExists === true) {
    isNew = false;
  } else {
    const existingLinks = await ctx.perspective.querySurrealDB(
      `SELECT 1 FROM link WHERE in.uri = ${safeBase} LIMIT 1`,
    );
    isNew = !existingLinks || existingLinks.length === 0;
  }

  let batchCreatedHere = false;
  if (!batchId) {
    batchId = await ctx.perspective.createBatch();
    batchCreatedHere = true;
  }

  if (isNew) {
    // ── CREATE PATH ─────────────────────────────────────────────────────────
    // Build initialValues from scalar (non-relation, non-action) fields for
    // createSubject, then use innerUpdate(false) for relations.
    const initialValues: Record<string, any> = {};
    for (const [key, value] of Object.entries(ctx.instance)) {
      if (
        value !== undefined &&
        value !== null &&
        !(Array.isArray(value) && (value as any[]).length > 0) &&
        !(value as any)?.action
      ) {
        initialValues[key] = value;
      }
    }

    const className =
      await ctx.perspective.stringOrTemplateObjectToSubjectClassName(
        ctx.instance,
      );
    await ctx.perspective.createSubject(
      className,
      ctx.id,
      initialValues,
      batchId,
    );

    await innerUpdate(ctx, false, batchId);

    // Write inherited properties not present in the class's own SHACL shape.
    // When a derived class uses sh:node to reference its parent shape, createSubject
    // only writes properties defined in the derived shape. Inherited @Property fields
    // (registered on the parent constructor) are silently ignored by the Rust backend.
    // We detect them by comparing the full merged metadata against the own-only registry.
    const proto = Object.getPrototypeOf(ctx.instance);
    const ownPropKeys = new Set(
      Object.keys(propertyRegistry.get(proto.constructor) ?? {}),
    );
    const allPropMeta = getPropertiesMetadata(proto.constructor);
    for (const [key, propMeta] of Object.entries(allPropMeta)) {
      if (ownPropKeys.has(key)) continue; // already handled by createSubject
      if ((propMeta as PropertyOptions).flag) continue; // flags are immutable
      const value = (ctx.instance as any)[key];
      if (value !== undefined && value !== null && value !== "") {
        await setProperty(ctx, key, value, batchId);
      }
    }
  } else {
    // ── UPDATE PATH ─────────────────────────────────────────────────────────
    // Instance already exists — update properties and relations only.
    await innerUpdate(ctx, true, batchId);
  }

  if (batchCreatedHere) {
    await ctx.perspective.commitBatch(batchId);

    // Rehydrate the instance so callers see the persisted state.
    const metadata = _getModelMetadata(ctx.instance.constructor);
    await fetchInstanceData(ctx.instance, ctx.perspective, ctx.id, metadata);
  }
}
