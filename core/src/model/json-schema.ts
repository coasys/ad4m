/**
 * JSON Schema → Ad4mModel class builder and related utilities.
 *
 * Contains:
 * - JSON Schema type definitions (JSONSchemaProperty, JSONSchema, JSONSchemaToModelOptions)
 * - Schema type helpers (normalizeNamespaceString, isArrayType, etc.)
 * - Predicate / namespace resolution helpers
 * - `buildModelFromJSONSchema()` — core implementation of `Ad4mModel.fromJSONSchema()`
 *
 * @module
 */

import type { Ad4mModel } from "./Ad4mModel";
import { capitalize, propertyNameToSetterName } from "./util";
import type { PropertyOptions } from "./decorators";
import { Model, setPropertyRegistryEntry, setRelationRegistryEntry } from "./decorators";

// ─── JSON Schema Type Definitions ────────────────────────────────────────────

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

// ─── Schema Type Helpers ─────────────────────────────────────────────────────

export function normalizeNamespaceString(namespace: string): string {
  if (!namespace) return '';
  if (namespace.includes('://')) {
    const [scheme, rest] = namespace.split('://');
    const path = (rest || '').replace(/\/+$/,'');
    return `${scheme}://${path}`;
  } else {
    return namespace.replace(/\/+$/,'');
  }
}

export function normalizeSchemaType(type?: string | string[]): string | undefined {
  if (!type) return undefined;
  if (typeof type === "string") return type;
  if (Array.isArray(type) && type.length > 0) {
    const nonNull = type.find((t) => t !== "null");
    return nonNull || type[0];
  }
  return undefined;
}

export function isSchemaType(schema: JSONSchemaProperty, expectedType: string): boolean {
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

// ─── Predicate / Namespace Resolution ────────────────────────────────────────

/**
 * Determines the namespace for predicates using cascading precedence:
 * 1. Explicit namespace in options
 * 2. x-ad4m metadata in schema
 * 3. Infer from schema title
 * 4. Try to extract from $id
 * 5. Error if nothing works
 */
export function determineNamespace(schema: JSONSchema, options: JSONSchemaToModelOptions): string {
  // 1. Explicit namespace in options (highest precedence)
  if (options.namespace) {
    return options.namespace;
  }
  
  // 2. x-ad4m metadata in schema
  if (schema["x-ad4m"]?.namespace) {
    return schema["x-ad4m"].namespace;
  }
  
  // 3. Infer from schema title
  if (schema.title) {
    return `${schema.title.toLowerCase()}://`;
  }
  
  // 4. Try to extract from $id if it's a URL
  if (schema.$id) {
    try {
      const url = new URL(schema.$id);
      const pathParts = url.pathname.split('/').filter(p => p);
      if (pathParts.length > 0) {
        const lastPart = pathParts[pathParts.length - 1];
        const baseName = lastPart.replace(/\.schema\.json$/, '').replace(/\.json$/, '');
        return `${baseName.toLowerCase()}://`;
      }
    } catch {
      // If $id is not a valid URL, continue to error
    }
  }
  
  // 5. Error if no namespace can be determined
  throw new Error(
    `Cannot infer namespace for JSON Schema. Please provide one of:
      - options.namespace
      - schema["x-ad4m"].namespace  
      - schema.title
      - valid schema.$id`
  );
}

/**
 * Determines the predicate for a specific property using cascading precedence:
 * 1. Explicit property mapping
 * 2. x-ad4m metadata in property schema
 * 3. Predicate template
 * 4. Custom predicate generator
 * 5. Default: namespace + property name
 */
export function determinePredicate(
  schema: JSONSchema,
  propertyName: string,
  propertySchema: JSONSchemaProperty,
  namespace: string,
  options: JSONSchemaToModelOptions
): string {
  // 1. Explicit property mapping (highest precedence)
  if (options.propertyMapping?.[propertyName]) {
    return options.propertyMapping[propertyName];
  }
  
  // 2. x-ad4m metadata in property schema
  if (propertySchema["x-ad4m"]?.through) {
    return propertySchema["x-ad4m"].through;
  }
  
  // 3. Generate from namespace + property name
  if (options.predicateTemplate) {
    const normalizedNs = normalizeNamespaceString(namespace);
    const [scheme, rest] = normalizedNs.includes('://') ? normalizedNs.split('://') : ['', normalizedNs];
    const nsNoScheme = rest || '';
    return options.predicateTemplate
      .replace('${namespace}', nsNoScheme)
      .replace('${scheme}', scheme)
      .replace('${ns}', nsNoScheme)
      .replace('${title}', schema.title || '')
      .replace('${property}', propertyName);
  }
  
  // 4. Custom predicate generator
  if (options.predicateGenerator) {
    return options.predicateGenerator(schema.title || '', propertyName);
  }
  
  // 5. Default: namespace + property name
  const normalizedNs = normalizeNamespaceString(namespace);
  if (normalizedNs.includes('://')) {
    // For namespaces like "product://", append property directly
    return `${normalizedNs}${propertyName}`;
  } else {
    return `${normalizedNs}://${propertyName}`;
  }
}

/**
 * Gets property-specific options using cascading precedence:
 * 1. Property-specific options
 * 2. x-ad4m metadata in property
 * 3. Global option
 * 4. Default value
 */
export function getPropertyOption(
  propertyName: string,
  propertySchema: JSONSchemaProperty,
  options: JSONSchemaToModelOptions,
  optionName: keyof PropertyOptions,
  defaultValue?: any
): any {
  // 1. Property-specific options
  if (options.propertyOptions?.[propertyName]?.[optionName] !== undefined) {
    return options.propertyOptions[propertyName][optionName];
  }
  
  // 2. x-ad4m metadata in property
  if (propertySchema["x-ad4m"]?.[optionName as keyof JSONSchemaProperty["x-ad4m"]] !== undefined) {
    return propertySchema["x-ad4m"][optionName as keyof JSONSchemaProperty["x-ad4m"]];
  }
  
  // 3. Global option
  if (options[optionName as keyof JSONSchemaToModelOptions] !== undefined) {
    return options[optionName as keyof JSONSchemaToModelOptions];
  }
  
  // 4. Default value
  return defaultValue;
}

/**
 * Gets default value for a JSON Schema type.
 */
export function getDefaultValueForType(type?: string): any {
  switch (type) {
    case 'string': return '';
    case 'number': return 0;
    case 'integer': return 0;
    case 'boolean': return false;
    case 'array': return [];
    case 'object': return {};
    default: return '';
  }
}

// ─── buildModelFromJSONSchema ────────────────────────────────────────────────

/**
 * Core implementation of `Ad4mModel.fromJSONSchema()`.
 *
 * Creates a dynamic Ad4mModel subclass from a JSON Schema definition,
 * wiring up property / relation metadata, setters, and the `@Model` decorator.
 *
 * @param BaseClass - The Ad4mModel class (or subclass) to extend
 * @param schema    - JSON Schema definition
 * @param options   - Configuration options
 * @returns Generated Ad4mModel subclass
 */
export function buildModelFromJSONSchema(
  BaseClass: typeof Ad4mModel,
  schema: JSONSchema,
  options: JSONSchemaToModelOptions
): typeof Ad4mModel {
  // Disallow top-level "author" property since Ad4mModel provides it implicitly via link authorship
  if (schema?.properties && Object.prototype.hasOwnProperty.call(schema.properties, "author")) {
    throw new Error('JSON Schema must not define a top-level "author" property because Ad4mModel already exposes it. Please rename the property (e.g., "writer").');
  }
  // Determine namespace with cascading precedence
  const namespace = determineNamespace(schema, options);
  
  // Create the dynamic class
  const DynamicModelClass = class extends (BaseClass as any) {} as unknown as typeof Ad4mModel;
  
  // Set up class metadata
  if (!options.name || options.name.trim() === '') {
    throw new Error("options.name is required and cannot be empty");
  }
  (DynamicModelClass as any).className = options.name;
  (DynamicModelClass.prototype as any).className = options.name;
  
  // Generate properties and relations metadata
  const properties: any = {};
  const relations: any = {};
  
  if (schema.properties) {
    for (const [propertyName, propertySchema] of Object.entries(schema.properties)) {
      const predicate = determinePredicate(schema, propertyName, propertySchema, namespace, options);
      const isRequired = schema.required?.includes(propertyName) || false;
      const propertyType = normalizeSchemaType(propertySchema.type);
      const isArray = isArrayType(propertySchema);
      
      if (isArray) {
        // Handle arrays as relations
        // Store the singular form as the relation key since SDNA generation expects singular
        relations[propertyName] = {
          through: predicate,
          local: getPropertyOption(propertyName, propertySchema, options, 'local')
        };
        
        // Define the property on prototype
        Object.defineProperty(DynamicModelClass.prototype, propertyName, {
          configurable: true,
          writable: true,
          value: []
        });
        
        // Add relation methods
        const adderName = `add${capitalize(propertyName)}`;
        const removerName = `remove${capitalize(propertyName)}`;
        const setterName = `set${capitalize(propertyName)}`;
        
        (DynamicModelClass.prototype as any)[adderName] = function() {
          // Placeholder function for SDNA generation
        };
        (DynamicModelClass.prototype as any)[removerName] = function() {
          // Placeholder function for SDNA generation
        };
        (DynamicModelClass.prototype as any)[setterName] = function() {
          // Placeholder function for SDNA generation
        };
        
      } else {
        // Handle regular properties
        let resolveLanguage = getPropertyOption(propertyName, propertySchema, options, 'resolveLanguage');
        // If no specific resolveLanguage for this property, use the global one
        if (!resolveLanguage && options.resolveLanguage) {
          resolveLanguage = options.resolveLanguage;
        }
        const local = getPropertyOption(propertyName, propertySchema, options, 'local');
        // Determine readOnly: check PropertyOptions first, then x-ad4m.writable (inverted) for JSON Schema wire format
        let readOnly = getPropertyOption(propertyName, propertySchema, options, 'readOnly');
        if (readOnly === undefined) {
          const xWritable = propertySchema["x-ad4m"]?.writable;
          readOnly = xWritable !== undefined ? !xWritable : false;
        }
        const writable = !readOnly;
        let initial = getPropertyOption(propertyName, propertySchema, options, 'initial');
        
        // Handle nested objects by serializing to JSON
        if (isObjectType(propertySchema) && !resolveLanguage) {
          resolveLanguage = 'literal';
          console.warn(`Property "${propertyName}" is an object type. It will be stored as JSON. Consider flattening complex objects for better semantic querying.`);
        }

        // Ensure numeric properties use literal language for correct typing
        if ((resolveLanguage === undefined || resolveLanguage === null) && isNumericType(propertySchema)) {
          resolveLanguage = 'literal';
        }

        // Default resolveLanguage to 'literal' for all remaining property types,
        // matching the @Property decorator behaviour. Without this, string values
        // stored as literal: URLs are returned unparsed.
        if (resolveLanguage === undefined || resolveLanguage === null) {
          resolveLanguage = 'literal';
        }
        
        // If property is required, ensure it has an initial value
        if (isRequired && !initial) {
          if (isObjectType(propertySchema)) {
            initial = 'literal:json:{}';
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
          ...(initial && { initial })
        };
        
        // Define the property on prototype
        Object.defineProperty(DynamicModelClass.prototype, propertyName, {
          configurable: true,
          writable: true,
          value: getDefaultValueForType(propertyType)
        });
        
        // Add setter function if writable
        if (writable) {
          const setterName = propertyNameToSetterName(propertyName);
          (DynamicModelClass.prototype as any)[setterName] = function() {
            // This is a placeholder function that the SDNA generation looks for
            // The actual setter logic is handled by the Ad4mModel base class
          };
        }
      }
    }
  }
  
  // No auto-flag generation — models with all-optional properties use
  // open-world structural matching.  Consumers who need type discrimination
  // should add an explicit @Flag to their model definition.
  
  // Attach metadata to WeakMap registries
  for (const [propName, propMeta] of Object.entries(properties)) {
    setPropertyRegistryEntry(DynamicModelClass, propName, propMeta as any);
  }
  for (const [relName, relMeta] of Object.entries(relations)) {
    setRelationRegistryEntry(DynamicModelClass, relName, {
      predicate: (relMeta as any).through || "",
      kind: 'hasMany',
      ...(( relMeta as any).getter !== undefined && { getter: (relMeta as any).getter }),
      ...(( relMeta as any).local !== undefined && { local: (relMeta as any).local }),
    });
  }
  
  // Store the JSON schema and options on the prototype for potential fallback use by getModelMetadata()
  (DynamicModelClass.prototype as any).__jsonSchema = schema;
  (DynamicModelClass.prototype as any).__jsonSchemaOptions = options;
  
  // Apply the Model decorator to set up the generateSDNA method
  const ModelDecorator = Model({ name: options.name });
  ModelDecorator(DynamicModelClass);
  
  return DynamicModelClass as typeof Ad4mModel;
}
