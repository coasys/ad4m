/**
 * Dynamic model creation from JSON Schema.
 *
 * Extracted from Ad4mModel.ts so it can be tested and reused independently.
 * The main entry point — `createModelFromJSONSchema` — takes the `Ad4mModel`
 * class as `BaseClass` to avoid a circular import between this module and the
 * class file.
 */

import {
  Model,
  PropertyOptions,
  propertyRegistry,
  relationRegistry,
} from "../decorators";
import { capitalize, propertyNameToSetterName } from "../util";

// ── JSON Schema type definitions ────────────────────────────────────────────

export interface JSONSchemaProperty {
  type: string | string[];
  items?: JSONSchemaProperty;
  properties?: { [key: string]: JSONSchemaProperty };
  required?: string[];
  "x-ad4m"?: {
    through?: string;
    resolveLanguage?: string;
    local?: boolean;
    writable?: boolean;
    initial?: string;
  };
}

export interface JSONSchema {
  $schema?: string;
  title?: string;
  $id?: string;
  type?: string;
  properties?: { [key: string]: JSONSchemaProperty };
  required?: string[];
  "x-ad4m"?: {
    namespace?: string;
    className?: string;
  };
}

export interface JSONSchemaToModelOptions {
  name: string;
  namespace?: string;
  predicateTemplate?: string;
  predicateGenerator?: (title: string, property: string) => string;
  propertyMapping?: Record<string, string>;
  resolveLanguage?: string;
  local?: boolean;
  propertyOptions?: Record<string, Partial<PropertyOptions>>;
}

// ── Internal schema helpers ─────────────────────────────────────────────────

export function normalizeNamespaceString(namespace: string): string {
  if (!namespace) return "";
  if (namespace.includes("://")) {
    const [scheme, rest] = namespace.split("://");
    const path = (rest || "").replace(/\/+$/, "");
    return `${scheme}://${path}`;
  } else {
    return namespace.replace(/\/+$/, "");
  }
}

export function normalizeSchemaType(
  type?: string | string[],
): string | undefined {
  if (!type) return undefined;
  if (typeof type === "string") return type;
  if (Array.isArray(type) && type.length > 0) {
    const nonNull = type.find((t) => t !== "null");
    return nonNull || type[0];
  }
  return undefined;
}

export function isSchemaType(
  schema: JSONSchemaProperty,
  expectedType: string,
): boolean {
  return normalizeSchemaType(schema.type) === expectedType;
}

export function isArrayType(schema: JSONSchemaProperty): boolean {
  return isSchemaType(schema, "array");
}

export function isObjectType(schema: JSONSchemaProperty): boolean {
  return isSchemaType(schema, "object");
}

export function isNumericType(schema: JSONSchemaProperty): boolean {
  const normalized = normalizeSchemaType(schema.type);
  return normalized === "number" || normalized === "integer";
}

// ── Predicate + namespace resolution ───────────────────────────────────────

export function determineNamespace(
  schema: JSONSchema,
  options: JSONSchemaToModelOptions,
): string {
  if (options.namespace) return options.namespace;
  if (schema["x-ad4m"]?.namespace) return schema["x-ad4m"].namespace;
  if (schema.title) return `${schema.title.toLowerCase()}://`;

  if (schema.$id) {
    try {
      const url = new URL(schema.$id);
      const pathParts = url.pathname.split("/").filter((p) => p);
      if (pathParts.length > 0) {
        const lastPart = pathParts[pathParts.length - 1];
        const baseName = lastPart
          .replace(/\.schema\.json$/, "")
          .replace(/\.json$/, "");
        return `${baseName.toLowerCase()}://`;
      }
    } catch {
      // not a valid URL — fall through to error
    }
  }

  throw new Error(
    `Cannot infer namespace for JSON Schema. Please provide one of:\n` +
      `  - options.namespace\n` +
      `  - schema["x-ad4m"].namespace\n` +
      `  - schema.title\n` +
      `  - valid schema.$id`,
  );
}

export function determinePredicate(
  schema: JSONSchema,
  propertyName: string,
  propertySchema: JSONSchemaProperty,
  namespace: string,
  options: JSONSchemaToModelOptions,
): string {
  if (options.propertyMapping?.[propertyName]) {
    return options.propertyMapping[propertyName];
  }
  if (propertySchema["x-ad4m"]?.through) {
    return propertySchema["x-ad4m"].through;
  }
  if (options.predicateTemplate) {
    const normalizedNs = normalizeNamespaceString(namespace);
    const [scheme, rest] = normalizedNs.includes("://")
      ? normalizedNs.split("://")
      : ["", normalizedNs];
    const nsNoScheme = rest || "";
    return options.predicateTemplate
      .replace("${namespace}", nsNoScheme)
      .replace("${scheme}", scheme)
      .replace("${ns}", nsNoScheme)
      .replace("${title}", schema.title || "")
      .replace("${property}", propertyName);
  }
  if (options.predicateGenerator) {
    return options.predicateGenerator(schema.title || "", propertyName);
  }
  const normalizedNs = normalizeNamespaceString(namespace);
  if (normalizedNs.includes("://")) {
    return `${normalizedNs}${propertyName}`;
  } else {
    return `${normalizedNs}://${propertyName}`;
  }
}

export function getPropertyOption(
  propertyName: string,
  propertySchema: JSONSchemaProperty,
  options: JSONSchemaToModelOptions,
  optionName: keyof PropertyOptions,
  defaultValue?: any,
): any {
  if (options.propertyOptions?.[propertyName]?.[optionName] !== undefined) {
    return options.propertyOptions[propertyName][optionName];
  }
  if (
    propertySchema["x-ad4m"]?.[
      optionName as keyof JSONSchemaProperty["x-ad4m"]
    ] !== undefined
  ) {
    return propertySchema["x-ad4m"][
      optionName as keyof JSONSchemaProperty["x-ad4m"]
    ];
  }
  if (options[optionName as keyof JSONSchemaToModelOptions] !== undefined) {
    return options[optionName as keyof JSONSchemaToModelOptions];
  }
  return defaultValue;
}

export function getDefaultValueForType(type?: string): any {
  switch (type) {
    case "string":
      return "";
    case "number":
      return 0;
    case "integer":
      return 0;
    case "boolean":
      return false;
    case "array":
      return [];
    case "object":
      return {};
    default:
      return "";
  }
}

// ── Main factory ────────────────────────────────────────────────────────────

/**
 * Creates an Ad4mModel subclass dynamically from a JSON Schema definition.
 *
 * Takes `BaseClass` as a parameter (rather than importing `Ad4mModel` directly)
 * to avoid a circular dependency between this module and `Ad4mModel.ts`.
 *
 * `Ad4mModel.fromJSONSchema()` is a thin wrapper that calls this with `this`.
 */
export function createModelFromJSONSchema(
  BaseClass: any,
  schema: JSONSchema,
  options: JSONSchemaToModelOptions,
): any {
  if (
    schema?.properties &&
    Object.prototype.hasOwnProperty.call(schema.properties, "author")
  ) {
    throw new Error(
      'JSON Schema must not define a top-level "author" property because Ad4mModel already exposes it. Please rename the property (e.g., "writer").',
    );
  }

  const namespace = determineNamespace(schema, options);
  const DynamicModelClass = class extends BaseClass {};

  if (!options.name || options.name.trim() === "") {
    throw new Error("options.name is required and cannot be empty");
  }
  (DynamicModelClass as any).className = options.name;
  (DynamicModelClass.prototype as any).className = options.name;

  const properties: any = {};
  const relations: any = {};

  if (schema.properties) {
    for (const [propertyName, propertySchema] of Object.entries(
      schema.properties,
    )) {
      const predicate = determinePredicate(
        schema,
        propertyName,
        propertySchema,
        namespace,
        options,
      );
      const isRequired = schema.required?.includes(propertyName) || false;
      const propertyType = normalizeSchemaType(propertySchema.type);
      const isArray = isArrayType(propertySchema);

      if (isArray) {
        relations[propertyName] = {
          through: predicate,
          local: getPropertyOption(
            propertyName,
            propertySchema,
            options,
            "local",
          ),
        };

        Object.defineProperty(DynamicModelClass.prototype, propertyName, {
          configurable: true,
          writable: true,
          value: [],
        });

        const adderName = `add${capitalize(propertyName)}`;
        const removerName = `remove${capitalize(propertyName)}`;
        const setterName = `set${capitalize(propertyName)}`;

        (DynamicModelClass.prototype as any)[adderName] = function () {};
        (DynamicModelClass.prototype as any)[removerName] = function () {};
        (DynamicModelClass.prototype as any)[setterName] = function () {};
      } else {
        let resolveLanguage = getPropertyOption(
          propertyName,
          propertySchema,
          options,
          "resolveLanguage",
        );
        if (!resolveLanguage && options.resolveLanguage) {
          resolveLanguage = options.resolveLanguage;
        }
        const local = getPropertyOption(
          propertyName,
          propertySchema,
          options,
          "local",
        );
        const writable = getPropertyOption(
          propertyName,
          propertySchema,
          options,
          "writable",
          true,
        );
        let initial = getPropertyOption(
          propertyName,
          propertySchema,
          options,
          "initial",
        );

        if (isObjectType(propertySchema) && !resolveLanguage) {
          resolveLanguage = "literal";
          console.warn(
            `Property "${propertyName}" is an object type. It will be stored as JSON. Consider flattening complex objects for better semantic querying.`,
          );
        }

        // No auto-assignment for numeric types — resolveLanguage === undefined
        // is now the implicit 'literal' default throughout the model layer.

        if (isRequired && !initial) {
          if (isObjectType(propertySchema)) {
            initial = "literal://json:{}";
          } else {
            initial = "ad4m://undefined";
          }
        }

        properties[propertyName] = {
          through: predicate,
          required: isRequired,
          writable: writable,
          ...(resolveLanguage && { resolveLanguage }),
          ...(local !== undefined && { local }),
          ...(initial && { initial }),
        };

        Object.defineProperty(DynamicModelClass.prototype, propertyName, {
          configurable: true,
          writable: true,
          value: getDefaultValueForType(propertyType),
        });

        if (writable) {
          const setterName = propertyNameToSetterName(propertyName);
          (DynamicModelClass.prototype as any)[setterName] = function () {};
        }
      }
    }
  }

  // Ensure at least one property has an initial value (needed for a valid SDNA constructor)
  const hasPropertyWithInitial = Object.values(properties).some(
    (prop: any) => prop.initial,
  );

  if (!hasPropertyWithInitial) {
    const typeProperty = `ad4m://type`;
    let typeValue: string;
    if (namespace.includes("://")) {
      const [scheme, rest] = namespace.split("://");
      const path = (rest || "").replace(/\/+$/, "");
      typeValue = path
        ? `${scheme}://${path}/instance`
        : `${scheme}://instance`;
    } else {
      typeValue = `${namespace.replace(/\/+$/, "")}/instance`;
    }

    properties["__ad4m_type"] = {
      through: typeProperty,
      required: true,
      writable: false,
      initial: typeValue,
      flag: true,
    };

    Object.defineProperty(DynamicModelClass.prototype, "__ad4m_type", {
      configurable: true,
      writable: false,
      value: typeValue,
    });

    console.warn(
      `No properties with initial values found. Added automatic type flag: ${typeProperty} = ${typeValue}`,
    );
  }

  propertyRegistry.set(DynamicModelClass, properties);
  relationRegistry.set(DynamicModelClass, relations);

  (DynamicModelClass.prototype as any).__jsonSchema = schema;
  (DynamicModelClass.prototype as any).__jsonSchemaOptions = options;

  const ModelDecorator = Model({ name: options.name });
  ModelDecorator(DynamicModelClass);

  return DynamicModelClass;
}
