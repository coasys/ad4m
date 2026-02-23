/**
 * Metadata extraction and instance-hydration helpers for Ad4mModel.
 *
 * Extracted from Ad4mModel.ts (Phase 3a Part 4).
 *
 * - `getModelMetadata(ctor)` — builds a ModelMetadata descriptor from the
 *   decorator registries, or falls back to the attached JSON Schema.
 * - `assignValuesToInstance()` — writes a batch of (name, value, resolve) tuples
 *   onto a model instance, resolving Literal expressions when requested.
 */

import type { PerspectiveProxy } from "../../perspectives/PerspectiveProxy";
import { getPropertiesMetadata, getRelationsMetadata } from "../decorators";
import type { PropertyOptions, RelationOptions } from "../decorators";
import {
  isArrayType,
  determinePredicate,
  determineNamespace,
} from "./fromJSONSchema";
import type { JSONSchemaProperty } from "./fromJSONSchema";
import type {
  ModelMetadata,
  PropertyMetadata,
  RelationMetadata,
} from "../types";

// ─────────────────────────────────────────────────────────────────────────────
// Shared value-tuple type
// ─────────────────────────────────────────────────────────────────────────────

/**
 * A tuple describing one property assignment: `[propertyName, rawValue, resolve?]`
 *
 * - `resolve = true`  → `rawValue` is an expression URL; fetch it and unwrap `.data`
 * - `resolve = false` (default) → use `rawValue` as-is (with UTF-8 reconstruction)
 */
export type ModelValueTuple = [name: string, value: any, resolve?: boolean];

// ─────────────────────────────────────────────────────────────────────────────
// getModelMetadataForClass
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Builds a {@link ModelMetadata} descriptor for `ctor` from its decorator
 * registries, falling back to the attached JSON Schema if no decorators are
 * found.
 *
 * The matching `Ad4mModel.getModelMetadata()` static method delegates here.
 */
export function getModelMetadata(ctor: any): ModelMetadata {
  const prototype = ctor.prototype as any;

  if (!prototype.className || prototype.className === "Ad4mModel") {
    throw new Error("Model class must be decorated with @Model");
  }

  const className: string = prototype.className;

  // ── Extract from decorator registries ──────────────────────────────────────
  const propertiesMetadata: Record<string, PropertyMetadata> = {};
  const prototypeProperties = getPropertiesMetadata(ctor);

  for (const [propertyName, opts] of Object.entries(prototypeProperties)) {
    const options = opts as PropertyOptions & {
      required?: boolean;
      flag?: boolean;
    };
    propertiesMetadata[propertyName] = {
      name: propertyName,
      predicate: options.through || "",
      required: options.required || false,
      writable: options.writable || false,
      ...(options.initial !== undefined && { initial: options.initial }),
      ...(options.resolveLanguage !== undefined && {
        resolveLanguage: options.resolveLanguage,
      }),
      ...(options.getter !== undefined && { getter: options.getter }),
      ...(options.local !== undefined && { local: options.local }),
      ...(options.transform !== undefined && { transform: options.transform }),
      ...(options.flag !== undefined && { flag: options.flag }),
    };
  }

  const relationsMetadata: Record<string, RelationMetadata> = {};
  const prototypeRelations = getRelationsMetadata(ctor);

  for (const [relationName, opts] of Object.entries(prototypeRelations)) {
    const options = opts as RelationOptions;
    relationsMetadata[relationName] = {
      name: relationName,
      predicate: options.through || "",
      ...(options.where !== undefined && { where: options.where }),
      ...(options.local !== undefined && { local: options.local }),
      ...(options.getter !== undefined && { getter: options.getter }),
      ...((opts as any).direction !== undefined && {
        direction: (opts as any).direction,
      }),
      ...((opts as any).maxCount !== undefined && {
        maxCount: (opts as any).maxCount,
      }),
      ...((opts as any).relatedModel !== undefined && {
        relatedModel: (opts as any).relatedModel,
      }),
    };
  }

  // ── Fallback: derive from attached JSON Schema if registries are empty ──────
  const hasMetadata =
    Object.keys(propertiesMetadata).length > 0 ||
    Object.keys(relationsMetadata).length > 0;

  if (!hasMetadata && prototype.__jsonSchema) {
    const schema = prototype.__jsonSchema;
    const schemaOptions = prototype.__jsonSchemaOptions || {};

    if (schema.properties) {
      for (const [propertyName, propertySchema] of Object.entries(
        schema.properties,
      )) {
        const isArray = isArrayType(propertySchema as JSONSchemaProperty);
        const predicate = determinePredicate(
          schema,
          propertyName,
          propertySchema as JSONSchemaProperty,
          determineNamespace(schema, schemaOptions),
          schemaOptions,
        );

        if (isArray) {
          relationsMetadata[propertyName] = {
            name: propertyName,
            predicate,
            ...((propertySchema as any)["x-ad4m"]?.local !== undefined && {
              local: (propertySchema as any)["x-ad4m"].local,
            }),
          };
        } else {
          propertiesMetadata[propertyName] = {
            name: propertyName,
            predicate,
            required: schema.required?.includes(propertyName) || false,
            writable: (propertySchema as any)["x-ad4m"]?.writable !== false,
            ...((propertySchema as any)["x-ad4m"]?.resolveLanguage && {
              resolveLanguage: (propertySchema as any)["x-ad4m"]
                .resolveLanguage,
            }),
            ...((propertySchema as any)["x-ad4m"]?.initial && {
              initial: (propertySchema as any)["x-ad4m"].initial,
            }),
            ...((propertySchema as any)["x-ad4m"]?.local !== undefined && {
              local: (propertySchema as any)["x-ad4m"].local,
            }),
          };
        }
      }
    }
  }

  return {
    className,
    properties: propertiesMetadata,
    relations: relationsMetadata,
  };
}

// ─────────────────────────────────────────────────────────────────────────────
// assignValuesToInstance
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Writes a batch of `(name, rawValue, resolve?)` tuples onto `instance`.
 *
 * - When `resolve` is true the raw value is treated as an expression URL and
 *   fetched via `perspective.getExpression()`; the `.data` field (JSON-parsed
 *   if possible) becomes the final value.
 * - Read-only accessor descriptors (getter without setter) are silently skipped.
 */
export async function assignValuesToInstance(
  perspective: PerspectiveProxy,
  instance: any,
  values: ModelValueTuple[],
): Promise<void> {
  const propsObject = Object.fromEntries(
    await Promise.all(
      values.map(async ([name, value, resolve]) => {
        let finalValue = value;

        // Handle UTF-8 byte sequences from Prolog URL decoding
        if (!resolve && typeof value === "string") {
          const codePoints = Array.from(value, (ch) => ch.codePointAt(0)!);
          const looksByteString = codePoints.every((cp) => cp <= 0xff);
          const hasHighByte = codePoints.some((cp) => cp >= 0x80);
          if (looksByteString && hasHighByte) {
            try {
              const bytes = Uint8Array.from(codePoints);
              const decoded = new TextDecoder("utf-8", { fatal: true }).decode(
                bytes,
              );
              if (decoded !== value) finalValue = decoded;
            } catch (error) {
              console.warn(
                `UTF-8 byte reconstruction failed for property "${name}"`,
                { value, error },
              );
            }
          }
        }

        if (resolve) {
          const resolvedExpression = await perspective.getExpression(value);
          if (resolvedExpression) {
            try {
              finalValue = JSON.parse(resolvedExpression.data);
            } catch {
              finalValue = resolvedExpression.data;
            }
          }
        }

        // Apply transform function if defined on the property
        const transform = getPropertiesMetadata(
          Object.getPrototypeOf(instance).constructor,
        )?.[name]?.transform;
        if (transform && typeof transform === "function") {
          finalValue = transform(finalValue);
        }

        return [name, finalValue];
      }),
    ),
  );

  // Skip read-only accessor descriptors (getters without setters)
  const writableProps = Object.fromEntries(
    Object.entries(propsObject).filter(([key]) => {
      const descriptor = Object.getOwnPropertyDescriptor(
        Object.getPrototypeOf(instance),
        key,
      );
      if (!descriptor) return true; // plain instance property — always writable
      const isAccessor =
        descriptor.get !== undefined || descriptor.set !== undefined;
      if (isAccessor) return descriptor.set !== undefined;
      return descriptor.writable !== false;
    }),
  );

  Object.assign(instance, writableProps);
}
