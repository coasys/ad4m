/**
 * Metadata extraction helper for Ad4mModel.
 *
 * - `getModelMetadata(ctor)` — builds a {@link ModelMetadata} descriptor from
 *   the decorator registries, or falls back to the attached JSON Schema.
 */

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
