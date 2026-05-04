import { capitalize } from "./util";
import type { Where } from "./types";
import { buildSDNA } from "./sdna";
import { buildSHACL } from "./shacl-gen";
import { escapeQueryString } from "../utils";
import type { ConformanceCondition } from "../shacl/SHACLShape";

// ============================================================================
// WeakMap-based metadata registry
// ============================================================================
// Stores property and relation metadata per-class using WeakMaps keyed by
// the class constructor. This replaces the old prototype-mutation approach
// and avoids issues with inheritance chains sharing mutable state.

/** Metadata stored for each property via @Property / @Optional / @ReadOnly / @Flag */
export interface PropertyMetadataEntry extends PropertyOptions {
    /** Internal computed writable flag (inverse of readOnly) for SDNA/SHACL compatibility */
    writable?: boolean;
    flag?: boolean;
}

/** Metadata stored for each relation via @HasMany / @HasOne / @BelongsToOne / @BelongsToMany */
export interface RelationMetadataEntry {
    predicate: string;
    /** Target model class thunk. Optional for untyped string relations. */
    target?: () => Ad4mModelLike;
    kind: 'hasMany' | 'hasOne' | 'belongsToOne' | 'belongsToMany';
    /**
     * Maximum number of related instances.
     * Set automatically: 1 for `@HasOne`/`@BelongsToOne`, undefined (unlimited) for `*Many`.
     */
    maxCount?: number;
    local?: boolean;
    /**
     * Custom getter to resolve the relation values.
     * The expression can reference 'Base' which will be replaced with the instance's base expression.
     */
    getter?: string;
    /**
     * Whether to auto-generate a conformance filter when `target` is set.
     * Defaults to `true` — set to `false` to opt out of DB-level type filtering
     * while keeping hydration capability via `include`.
     */
    filter?: boolean;
    /**
     * Filter constraints on linked target nodes using the query DSL.
     * Property names reference the target model's properties.
     * Overrides auto-derived conformance when set.
     */
    where?: Where;
}

/** Registry of property metadata keyed by constructor → { propName → metadata } */
const propertyRegistry = new WeakMap<Function, Record<string, PropertyMetadataEntry>>();

/** Registry of relation metadata keyed by constructor → { propName → metadata } */
const relationRegistry = new WeakMap<Function, Record<string, RelationMetadataEntry>>();

/** Memoisation cache for getPropertiesMetadata — avoids repeated prototype-chain walks */
const propertiesMetadataCache = new WeakMap<Function, Record<string, PropertyMetadataEntry>>();

/** Memoisation cache for getRelationsMetadata — avoids repeated prototype-chain walks */
const relationsMetadataCache = new WeakMap<Function, Record<string, RelationMetadataEntry>>();

/** Memoisation cache for generateSHACL — avoids recomputing SHACL shapes */
const shaclCache = new WeakMap<Function, any>();

/**
 * Retrieve property metadata for a given class constructor.
 * Walks the prototype chain so subclass decorators compose with parent decorators.
 * Results are memoised per class constructor.
 */
export function getPropertiesMetadata(ctor: Function): Record<string, PropertyMetadataEntry> {
    const cached = propertiesMetadataCache.get(ctor);
    if (cached) return cached;

    const result: Record<string, PropertyMetadataEntry> = {};
    const chain: Function[] = [];
    let current = ctor;
    while (current && current !== Object) {
        chain.unshift(current); // parent-first order
        current = Object.getPrototypeOf(current);
    }
    for (const c of chain) {
        const meta = propertyRegistry.get(c);
        if (meta) Object.assign(result, meta);
    }
    propertiesMetadataCache.set(ctor, result);
    return result;
}

/**
 * Retrieve relation metadata for a given class constructor.
 * Walks the prototype chain so subclass decorators compose with parent decorators.
 * Results are memoised per class constructor.
 */
export function getRelationsMetadata(ctor: Function): Record<string, RelationMetadataEntry> {
    const cached = relationsMetadataCache.get(ctor);
    if (cached) return cached;

    const result: Record<string, RelationMetadataEntry> = {};
    const chain: Function[] = [];
    let current = ctor;
    while (current && current !== Object) {
        chain.unshift(current);
        current = Object.getPrototypeOf(current);
    }
    for (const c of chain) {
        const meta = relationRegistry.get(c);
        if (meta) Object.assign(result, meta);
    }
    relationsMetadataCache.set(ctor, result);
    return result;
}

/**
 * Get or compute memoised SHACL for a class. Used by @Model decorator.
 */
export function getMemoizedSHACL(target: Function, compute: () => any): any {
    const cached = shaclCache.get(target);
    if (cached) return cached;
    const result = compute();
    shaclCache.set(target, result);
    return result;
}


/**
 * Programmatically register property metadata for a given constructor.
 * Used by `fromJSONSchema()` and other dynamic model builders.
 */
export function setPropertyRegistryEntry(
    ctor: Function,
    propName: string,
    meta: PropertyMetadataEntry & { writable?: boolean },
): void {
    if (!propertyRegistry.has(ctor)) propertyRegistry.set(ctor, {});
    propertyRegistry.get(ctor)![propName] = meta;
}

/**
 * Programmatically register relation metadata for a given constructor.
 * Used by `fromJSONSchema()` and other dynamic model builders.
 */
export function setRelationRegistryEntry(
    ctor: Function,
    relName: string,
    meta: RelationMetadataEntry,
): void {
    if (!relationRegistry.has(ctor)) relationRegistry.set(ctor, {});
    relationRegistry.get(ctor)![relName] = meta;
}

/**
 * Interface for any class that looks like an Ad4mModel (used in circular-ref-safe typings).
 */
export interface Ad4mModelLike {
    new (...args: any[]): any;
    className?: string;
    generateSDNA?: () => any;
    generateSHACL?: () => any;
}

/**
 * Build a conformance filter for a relation whose target model is known.
 *
 * Inspects the target class's property metadata to derive:
 * - `conformanceConditions`: Structured, DB-agnostic conditions (flag & required checks)
 * - `getter`: Pre-computed graph traversal expression that traverses outgoing links and filters
 *   target nodes to only those conforming to the target shape.
 *
 * @param relationPredicate - The relation's predicate URI (e.g. "flux://entry_type")
 * @param targetClass       - The target model class (resolved from the target() thunk)
 * @returns `{ getter, conformanceConditions }` or `undefined` if no conditions could be derived
 */
export function buildConformanceFilter(
    relationPredicate: string,
    targetClass: Ad4mModelLike
): { getter: string; conformanceConditions: ConformanceCondition[] } | undefined {
    try {
        const targetProps = getPropertiesMetadata(targetClass);
        const conditions: ConformanceCondition[] = [];
        const sparqlConditions: string[] = [];
        let varIdx = 0;

        // 1. Flags — check predicate + value
        for (const [_propName, propMeta] of Object.entries(targetProps)) {
            if (propMeta.flag && propMeta.initial && propMeta.through) {
                conditions.push({
                    type: 'flag',
                    predicate: propMeta.through,
                    value: propMeta.initial,
                });
                sparqlConditions.push(
                    `?target <${escapeQueryString(propMeta.through)}> <${escapeQueryString(propMeta.initial)}> .`
                );
            }
        }

        // 2. Required (non-flag) properties — check predicate exists
        //    Skip properties with custom getters (they don't follow the standard link pattern)
        for (const [_propName, propMeta] of Object.entries(targetProps)) {
            if (propMeta.required && !propMeta.flag && !propMeta.getter && propMeta.through) {
                conditions.push({
                    type: 'required',
                    predicate: propMeta.through,
                });
                sparqlConditions.push(
                    `?target <${escapeQueryString(propMeta.through)}> ?_v${varIdx++} .`
                );
            }
        }

        if (conditions.length === 0) {
            return undefined;
        }

        const getter = `SELECT ?target WHERE { <Base> <${escapeQueryString(relationPredicate)}> ?target . ${sparqlConditions.join(' ')} }`;

        return { getter, conformanceConditions: conditions };
    } catch (e) {
        // Target class may not have property metadata
        return undefined;
    }
}

export interface PropertyOptions {
    /**
     * The predicate of the property. All properties must have this option.
     */
    through?: string;

    /**
     * The initial value of the property. Required if the property is marked as required.
     */
    initial?: string;

    /**
     * Indicates whether the property is required. If true, an initial value must be provided.
     */
    required?: boolean;

    /**
     * Indicates whether the property is read-only. If true, no setter will be generated.
     * Defaults to false (property is writable).
     */
    readOnly?: boolean;

    /**
     * The language used to store the property. Can be the default `Literal` Language or a custom language address.
     */
    resolveLanguage?: string;

    /**
     * Custom Prolog getter to get the value of the property. If not provided, the default getter will be used.
     */
    prologGetter?: string;

    /**
     * Custom Prolog setter to set the value of the property. Only available if the property is writable.
     */
    prologSetter?: string;

    /**
     * Custom getter to resolve the property value. Use this for custom graph traversals.
     * The expression can reference 'Base' which will be replaced with the instance's base expression.
     * Example: "SELECT ?target WHERE { <Base> <flux://has_reply> ?target . } LIMIT 1"
     */
    getter?: string;

    /**
     * Indicates whether the property is stored locally in the perspective and not in the network. Useful for properties that are not meant to be shared with the network.
     */
    local?: boolean;

    /**
     * Optional transform function to modify the property value after it is retrieved.
     * This is useful for transforming raw data into a more usable format.
     * The function takes the raw value as input and returns the transformed value.
     */
    transform?: (value: any) => any;

    /**
     * Allowed values for this property (enum constraint).
     * Maps to SHACL `sh:in`. Each entry has a `value` (the stored RDF term)
     * and an optional `label` (human-readable display name).
     *
     * @example
     * ```typescript
     * @Property({
     *   through: "task://status",
     *   options: [
     *     { value: "task://todo", label: "To Do" },
     *     { value: "task://doing", label: "Doing" },
     *     { value: "task://done", label: "Done" },
     *   ]
     * })
     * status: string = "task://todo";
     * ```
     */
    options?: Array<{ value: string; label?: string }>;
}


/**
 * Internal core implementation for registering property metadata on the prototype.
 * All property decorators (@Property, @Optional, @ReadOnly) and relation decorators
 * that create properties delegate to this function.
 * @internal
 */
function applyPropertyMetadata(opts: PropertyOptions) {
    return function <T>(target: T, key: keyof T) {
        // Map readOnly → internal writable for SDNA/SHACL compatibility
        const writable = opts.readOnly ? false : (opts.through ? true : false);
        
        if (opts.required && !opts.initial) {
            throw new Error("SubjectProperty requires an 'initial' option if 'required' is true");
        }

        if (!opts.through && !opts.prologGetter && !opts.getter) {
            throw new Error("SubjectProperty requires either 'through' or 'prologGetter' or 'getter' option")
        }

        // Write to WeakMap registry (keyed by constructor)
        const ctor = (target as any).constructor;
        if (!propertyRegistry.has(ctor)) propertyRegistry.set(ctor, {});
        propertyRegistry.get(ctor)![key as string] = { ...opts, writable } as any;

        if (writable) {
            const value = key as string
            target[`set${capitalize(value)}`] = () => {}
        }

        Object.defineProperty(target, key, {configurable: true, writable: true});
    };
}

/**
 * Convenience decorator for defining optional (not required) properties.
 *
 * @category Decorators
 *
 * @description
 * Equivalent to `@Property` but defaults `required` to `false` and does not
 * apply `resolveLanguage` or `initial` defaults.  Use this when a property
 * may or may not have a value, and you want full control over its configuration.
 *
 * @example
 * ```typescript
 * class Recipe extends Ad4mModel {
 *   @Optional({ through: "recipe://description" })
 *   description?: string;
 * }
 * ```
 *
 * @param {PropertyOptions} opts - Property configuration (same options as @Property)
 */
export function Optional(opts: PropertyOptions) {
    return applyPropertyMetadata({
        ...opts,
        required: opts.required ?? false,
        readOnly: opts.readOnly ?? false,
    });
}

export interface FlagOptions {
    /**
     * The predicate of the property. All properties must have this option.
     */
    through: string;

    /**
     * The value of the property.
     */
    value: string;
}

/**
 * Decorator for defining flags on model classes.
 * 
 * @category Decorators
 * 
 * @description
 * A specialized property decorator for defining immutable type flags or markers on model instances.
 * Flags are always required properties with a fixed value that cannot be changed after creation.
 * 
 * Common uses for flags:
 * - Type discrimination between different kinds of models
 * - Marking models with specific capabilities or features
 * - Versioning or compatibility markers
 * 
 * Note: Use of Flag is discouraged unless you specifically need type-based filtering or 
 * discrimination between different kinds of models. For most cases, regular properties
 * with @Property or @Optional are more appropriate.
 * 
 * @example
 * ```typescript
 * class Message extends Ad4mModel {
 *   // Type flag to identify message models
 *   @Flag({
 *     through: "ad4m://type",
 *     value: "ad4m://message"
 *   })
 *   type: string = "";
 * 
 *   // Version flag for compatibility
 *   @Flag({
 *     through: "ad4m://version",
 *     value: "1.0.0"
 *   })
 *   version: string = "";
 * 
 *   // Feature flag
 *   @Flag({
 *     through: "message://feature",
 *     value: "message://encrypted"
 *   })
 *   feature: string = "";
 * }
 * 
 * // Later you can query for specific types:
 * const messages = await Message.query(perspective)
 *   .where({ type: "ad4m://message" })
 *   .run();
 * ```
 * 
 * @param {FlagOptions} opts - Flag configuration
 * @param {string} opts.through - The predicate URI for the flag
 * @param {string} opts.value - The fixed value for the flag
 */
export function Flag(opts: FlagOptions) {
    return function <T>(target: T, key: keyof T) {
        if (!opts.through && !opts.value) {
            throw new Error("SubjectFlag requires a 'through' and 'value' option")
        }

        if (!opts.through) {
            throw new Error("SubjectFlag requires a 'through' option")
        }

        if (!opts.value) {
            throw new Error("SubjectFlag requires a 'value' option")
        }

        const entry = {
            through: opts.through,
            required: true,
            initial: opts.value,
            flag: true,
            readOnly: true,
            writable: false,
        };

        // Write to WeakMap registry
        const ctor = (target as any).constructor;
        if (!propertyRegistry.has(ctor)) propertyRegistry.set(ctor, {});
        propertyRegistry.get(ctor)![key as string] = entry as any;

        // @ts-ignore
        target[key] = opts.value;

        Object.defineProperty(target, key, {configurable: true, writable: true});
    };
}



export interface ModelConfig {
    /**
     * The name of the entity.
     */
    name: string;
}

/**
 * Decorator for defining model classes in AD4M.
 * 
 * @category Decorators
 * 
 * @description
 * The root decorator that must be applied to any class that represents a model in AD4M.
 * It registers the class as a Social DNA (SDNA) subject class and provides the infrastructure
 * for storing and retrieving instances.
 * 
 * This decorator:
 * - Registers the class with a unique name in the AD4M system
 * - Generates the necessary SDNA code for the model's properties and relations
 * - Enables the use of other model decorators (@Property, @HasMany, etc.)
 * - Provides static query methods through the Ad4mModel base class
 * 
 * @example
 * ```typescript
 * @Model({ name: "Recipe" })
 * class Recipe extends Ad4mModel {
 *   @Property({
 *     through: "recipe://name",
 *     resolveLanguage: "literal"
 *   })
 *   name: string = "";
 * 
 *   @HasMany({ through: "recipe://ingredient" })
 *   ingredients: string[] = [];
 * 
 *   // Static query methods from Ad4mModel:
 *   static async findByName(perspective: PerspectiveProxy, name: string) {
 *     return Recipe.query(perspective)
 *       .where({ name })
 *       .run();
 *   }
 * }
 * 
 * // Using the model:
 * const recipe = new Recipe(perspective);
 * recipe.name = "Chocolate Cake";
 * await recipe.save();
 * 
 * // Querying instances:
 * const recipes = await Recipe.query(perspective)
 *   .where({ name: "Chocolate Cake" })
 *   .run();
 * 
 * // Using with PerspectiveProxy:
 * await perspective.ensureSDNASubjectClass(Recipe);
 * ```
 * 
 * @param {ModelConfig} opts - Model configuration
 * @param {string} opts.name - Unique name for the model class in AD4M
 */
export function Model(opts: ModelConfig) {
    return function (target: any) {
        target.prototype.className = opts.name;
        target.className = opts.name;

        target.generateSDNA = function() {
            return buildSDNA(
                opts.name,
                target.prototype,
                getPropertiesMetadata(target),
                getRelationsMetadata(target),
            );
        }

        target.generateSHACL = function() {
            return getMemoizedSHACL(target, () => buildSHACL(
                opts.name,
                target,
                getPropertiesMetadata(target),
                getRelationsMetadata(target),
                buildConformanceFilter,
            ));
        }

        Object.defineProperty(target, 'type', {configurable: true});
    }
}

/**
 * The primary property decorator for AD4M model classes.
 * 
 * @category Decorators
 * 
 * @description
 * The core property decorator with smart defaults.  All other property decorators
 * (@Optional, @ReadOnly) are thin wrappers that adjust these defaults.
 * 
 * Smart defaults (all overridable):
 * - `required` → `false`
 * - `readOnly` → `false`
 * - `resolveLanguage` → `"literal"`
 * - `initial` → `undefined` (no link created until a value is explicitly set)
 * 
 * Properties are optional by default. When a model instance is created without
 * providing a value for an optional property, no link is added to the graph.
 * Set `required: true` explicitly when a property must always be present (this
 * also adds a `"literal:string:uninitialized"` sentinel as the initial value
 * so that the SDNA constructor creates a placeholder link).
 * 
 * @example
 * ```typescript
 * class User extends Ad4mModel {
 *   // Optional property (default) — no link created until a value is set
 *   @Property({
 *     through: "user://name"
 *   })
 *   name: string = "";
 * 
 *   // Explicitly required property with sentinel initial value
 *   @Property({
 *     through: "user://status",
 *     required: true
 *   })
 *   status: string = "";
 * 
 *   // Required property with custom initial value
 *   @Property({
 *     through: "user://role",
 *     required: true,
 *     initial: "user://member"
 *   })
 *   role: string = "";
 * 
 *   // Optional property with literal resolution
 *   @Property({
 *     through: "user://bio",
 *     resolveLanguage: "literal"
 *   })
 *   bio: string = "";
 * }
 * ```
 * 
 * @param {PropertyOptions} opts - Property configuration
 * @param {string} opts.through - The predicate URI for the property
 * @param {boolean} [opts.required=false] - Whether the property is required (adds query filters and sentinel initial value)
 * @param {string} [opts.initial] - Initial value (defaults to "literal:string:uninitialized" when required)
 * @param {string} [opts.resolveLanguage] - Language to use for value resolution (e.g. "literal")
 * @param {string} [opts.prologGetter] - Custom Prolog code for getting the property value
 * @param {string} [opts.prologSetter] - Custom Prolog code for setting the property value
 * @param {boolean} [opts.local] - Whether the property should only be stored locally
 */
export function Property(opts: PropertyOptions) {
    const required = opts.required ?? false;
    return applyPropertyMetadata({
        ...opts,
        required,
        readOnly: opts.readOnly ?? false,
        resolveLanguage: opts.resolveLanguage ?? "literal",
        initial: opts.initial ?? (required ? "literal:string:uninitialized" : undefined),
    });
}

/**
 * Decorator for defining read-only properties on model classes.
 * 
 * @category Decorators
 * 
 * @description
 * A convenience decorator that defines a read-only property.
 * Equivalent to `@Property` with `readOnly: true`.
 * 
 * Read-only properties are ideal for:
 * - Computed or derived values
 * - Properties that should never change after creation
 * - Properties that are set by the system
 * - Properties that represent immutable data
 * 
 * @example
 * ```typescript
 * class Post extends Ad4mModel {
 *   // Read-only property with custom getter for computed value
 *   @ReadOnly({
 *     through: "post://likes",
 *     getter: `findall(User, triple(Base, "post://liked_by", User), Users), length(Users, Value)`
 *   })
 *   likeCount: number = 0;
 * 
 *   // Read-only property for creation timestamp
 *   @ReadOnly({
 *     through: "post://created_at",
 *     initial: new Date().toISOString()
 *   })
 *   createdAt: string = "";
 * 
 *   // Read-only property that resolves to a Literal
 *   @ReadOnly({
 *     through: "post://author",
 *     resolveLanguage: "literal"
 *   })
 *   author: string = "";
 * 
 *   // Read-only property for system-managed data
 *   @ReadOnly({
 *     through: "post://version",
 *     initial: "1.0.0"
 *   })
 *   version: string = "";
 * }
 * ```
 * 
 * @param {PropertyOptions} opts - Property configuration
 * @param {string} opts.through - The predicate URI for the property
 * @param {string} [opts.initial] - Initial value (if property should have one)
 * @param {string} [opts.resolveLanguage] - Language to use for value resolution (e.g. "literal")
 * @param {string} [opts.prologGetter] - Custom Prolog code for getting the property value
 * @param {boolean} [opts.local] - Whether the property should only be stored locally
 */
export function ReadOnly(opts: PropertyOptions) {
    return Property({
        ...opts,
        readOnly: true,
    });
}

// ============================================================================
// Relation decorators
// ============================================================================

/**
 * Options for relation decorators (@HasMany, @HasOne, @BelongsToOne, @BelongsToMany).
 */
export interface RelationOptions {
    /**
     * The predicate URI used to link the two models.
     * Defaults to `'ad4m://has_child'` when omitted.
     * Cannot be combined with `getter`.
     */
    through?: string;
    /** The target model class (use a thunk to avoid circular-dependency issues). Optional for untyped string relations.
     *  Cannot be combined with `getter`. */
    target?: () => Ad4mModelLike;
    /**
     * Custom getter to resolve the relation values. Use this for custom graph traversals.
     * The expression can reference 'Base' which will be replaced with the instance's base expression.
     * Example: "SELECT ?target WHERE { ?target <flux://has_reply> <Base> . }"
     *
     * Mutually exclusive with `through` and `target`. When `getter` is provided the
     * relation is read-only (no adder/remover actions are generated).
     */
    getter?: string;
    /** Whether the link is stored locally (not shared on the network) */
    local?: boolean;
    /**
     * Whether to auto-generate a DB-level conformance filter when `target` is set.
     * Defaults to `true` when `target` is present — the query will only return linked
     * nodes whose shape matches the target model (required properties, flags, etc.).
     * Set to `false` to opt out of filtering while keeping hydration capability.
     */
    filter?: boolean;
    /**
     * Filter constraints on the linked target nodes, using the same query DSL
     * as `Model.query().where(...)`. Property names reference the **target**
     * model's properties (resolved via target metadata).
     *
     * When `target` is set and `where` is omitted, conformance conditions are
     * auto-derived from the target shape (flags + required properties).
     * Providing `where` overrides this auto-derivation.
     *
     * Mutually exclusive with `getter` and `filter: false`.
     *
     * @example
     * ```typescript
     * @HasMany(() => Message, {
     *   through: "flux://entry_type",
     *   where: { status: "active", priority: { gte: 3 } }
     * })
     * activeMessages: Message[] = []
     * ```
     */
    where?: Where;
}

/**
 * Utility type that describes the auto-generated helper methods for a HasMany
 * relation. For a property named `comments` on a class `Post`, the following
 * methods will be available on instances:
 *
 *   post.addComment(value)
 *   post.removeComment(value)
 *   post.setComment(values)
 */
export type HasManyMethods<Keys extends string> = {
    [K in Keys as `add${Capitalize<K>}`]: (value: string | { id: string }, batchId?: string) => Promise<void>;
} & {
    [K in Keys as `remove${Capitalize<K>}`]: (value: string | { id: string }, batchId?: string) => Promise<void>;
} & {
    [K in Keys as `set${Capitalize<K>}`]: (values: (string | { id: string })[], batchId?: string) => Promise<void>;
};

/**
 * Resolve overloaded relation decorator arguments.
 * Supports two calling conventions:
 *   @HasMany({ through: "...", target: () => X })       — single options object
 *   @HasMany(() => X, { through: "..." })                — target thunk + options
 * @internal
 */
function resolveRelationArgs(
    first: (() => Ad4mModelLike) | RelationOptions,
    second?: Omit<RelationOptions, 'target'>,
): RelationOptions {
    const opts = typeof first === 'function'
        ? { ...(second || {}), target: first }
        : first;

    // getter is mutually exclusive with through, target, and where
    if (opts.getter) {
        if (opts.through) {
            throw new Error(
                'Relation decorator: `getter` and `through` are mutually exclusive. ' +
                'Use `getter` alone for custom read-only relations, or `through` ' +
                '(with optional `target`) for standard link-based relations.'
            );
        }
        if (opts.target) {
            throw new Error(
                'Relation decorator: `getter` and `target` are mutually exclusive. ' +
                '`target` auto-generates a conformance getter from the model shape; ' +
                'providing both is contradictory.'
            );
        }
        if (opts.where) {
            throw new Error(
                'Relation decorator: `where` and `getter` are mutually exclusive. ' +
                'Use `where` for DSL-based filtering, or `getter` for raw getter expression.'
            );
        }
        return opts;
    }

    // Default predicate when not provided
    if (!opts.through) {
        opts.through = 'ad4m://has_child';
    }

    // where validation
    if (opts.where) {
        if (opts.filter === false) {
            throw new Error(
                'Relation decorator: `where` and `filter: false` are contradictory. ' +
                '`where` adds filtering constraints; `filter: false` disables filtering.'
            );
        }
    }

    return opts;
}

/**
 * Decorator for defining a one-to-many relation.
 *
 * @category Decorators
 *
 * @description
 * Declares that the decorated property is an array of related model instances.
 * Under the hood it registers the relation in the relation registry and also
 * creates the corresponding relation entry so that the SDNA / SHACL
 * generators continue to emit the correct subject-class code.
 *
 * Supports two calling conventions:
 * ```typescript
 * // Options-object style
 * @HasMany({ through: "post://comment", target: () => Comment })
 *
 * // Target-first shorthand
 * @HasMany(() => Comment, { through: "post://comment" })
 * ```
 *
 * @example
 * ```typescript
 * @Model({ name: "Post" })
 * class Post extends Ad4mModel {
 *   @HasMany(() => Comment, { through: "post://comment" })
 *   comments: string[] = [];
 * }
 * ```
 */
export function HasMany(opts: RelationOptions): PropertyDecorator;
export function HasMany(target: () => Ad4mModelLike, opts?: Omit<RelationOptions, 'target'>): PropertyDecorator;
export function HasMany(
    first: (() => Ad4mModelLike) | RelationOptions,
    second?: Omit<RelationOptions, 'target'>,
): PropertyDecorator {
    const explicitThrough = typeof first === 'function' ? second?.through : (first as RelationOptions).through;
    const opts = resolveRelationArgs(first, second);
    return function <T>(target: T, key: keyof T) {
        if (!explicitThrough && !opts.getter) {
            const className = ((target as any).constructor?.name) ?? 'unknown';
            console.warn(
                `@HasMany on ${className}.${String(key)} has no 'through' predicate — defaulting to 'ad4m://has_child'. ` +
                `Add { through: 'ad4m://has_child' } to silence this warning, or use a semantic predicate ` +
                `like { through: 'myns://has_${String(key)}' } for efficient targeted queries.`
            );
        }
        // --- relation registry ---
        const ctor = (target as any).constructor;
        if (!relationRegistry.has(ctor)) relationRegistry.set(ctor, {});
        const map = relationRegistry.get(ctor)!;
        map[key as string] = {
            predicate: opts.through,
            target: opts.target,
            kind: 'hasMany',
            local: opts.local,
            ...(opts.getter && { getter: opts.getter }),
            ...(opts.filter !== undefined && { filter: opts.filter }),
            ...(opts.where && { where: opts.where }),
        };

        const relKey = key as string;
        // Only add mutation methods when a predicate is available
        // (getter-only relations are read-only)
        if (opts.through) {
            (target as any)[`add${capitalize(relKey)}`] = async function(this: any, arg: any, batchId?: string) {
                return (this as any).addRelationValue(relKey, arg, batchId);
            };
            (target as any)[`remove${capitalize(relKey)}`] = async function(this: any, arg: any, batchId?: string) {
                return (this as any).removeRelationValue(relKey, arg, batchId);
            };
            (target as any)[`set${capitalize(relKey)}`] = async function(this: any, arg: any, batchId?: string) {
                return (this as any).setRelationValues(relKey, arg, batchId);
            };
        }
        Object.defineProperty(target, relKey, { configurable: true, writable: true });
    };
}

/**
 * Decorator for defining a one-to-one relation (owning side).
 *
 * @category Decorators
 *
 * @description
 * Declares that the decorated property holds a single related model instance.
 * The owning side manages the link.
 *
 * Supports two calling conventions:
 * ```typescript
 * @HasOne({ through: "post://author", target: () => Author })
 * @HasOne(() => Author, { through: "post://author" })
 * ```
 *
 * @example
 * ```typescript
 * @Model({ name: "Post" })
 * class Post extends Ad4mModel {
 *   @HasOne(() => Author, { through: "post://author" })
 *   author: string = "";
 * }
 * ```
 */
export function HasOne(opts: RelationOptions): PropertyDecorator;
export function HasOne(target: () => Ad4mModelLike, opts?: Omit<RelationOptions, 'target'>): PropertyDecorator;
export function HasOne(
    first: (() => Ad4mModelLike) | RelationOptions,
    second?: Omit<RelationOptions, 'target'>,
): PropertyDecorator {
    const opts = resolveRelationArgs(first, second);
    return function <T>(target: T, key: keyof T) {
        const ctor = (target as any).constructor;
        if (!relationRegistry.has(ctor)) relationRegistry.set(ctor, {});
        const map = relationRegistry.get(ctor)!;
        map[key as string] = {
            predicate: opts.through,
            target: opts.target,
            kind: 'hasOne',
            maxCount: 1,
            local: opts.local,
            ...(opts.filter !== undefined && { filter: opts.filter }),
            ...(opts.where && { where: opts.where }),
        };

        const relKey = key as string;
        if (opts.through) {
            // Register as a writable property
            applyPropertyMetadata({
                through: opts.through,
                readOnly: false,
                local: opts.local,
            })(target, key);

            // Add prototype methods for add/remove/set (mirroring @HasMany)
            (target as any)[`add${capitalize(relKey)}`] = async function(this: any, arg: any) {
                return (this as any).addRelationValue(relKey, arg);
            };
            (target as any)[`remove${capitalize(relKey)}`] = async function(this: any, arg: any) {
                return (this as any).removeRelationValue(relKey, arg);
            };
            (target as any)[`set${capitalize(relKey)}`] = async function(this: any, arg: any) {
                return (this as any).setRelationValues(relKey, arg);
            };
        } else {
            Object.defineProperty(target, relKey, { configurable: true, writable: true });
        }
    };
}

/**
 * Decorator for defining the inverse side of a one-to-one relation.
 *
 * @category Decorators
 *
 * @description
 * Declares the non-owning (inverse) side of a one-to-one relationship.
 * The property is read-only since the owning side manages the link.
 *
 * Supports two calling conventions:
 * ```typescript
 * @BelongsToOne({ through: "post://author", target: () => Post })
 * @BelongsToOne(() => Post, { through: "post://author" })
 * ```
 *
 * @example
 * ```typescript
 * @Model({ name: "Author" })
 * class Author extends Ad4mModel {
 *   @BelongsToOne(() => Post, { through: "post://author" })
 *   post: string = "";
 * }
 * ```
 */
export function BelongsToOne(opts: RelationOptions): PropertyDecorator;
export function BelongsToOne(target: () => Ad4mModelLike, opts?: Omit<RelationOptions, 'target'>): PropertyDecorator;
export function BelongsToOne(
    first: (() => Ad4mModelLike) | RelationOptions,
    second?: Omit<RelationOptions, 'target'>,
): PropertyDecorator {
    const opts = resolveRelationArgs(first, second);
    return function <T>(target: T, key: keyof T) {
        const ctor = (target as any).constructor;
        if (!relationRegistry.has(ctor)) relationRegistry.set(ctor, {});
        const map = relationRegistry.get(ctor)!;
        map[key as string] = {
            predicate: opts.through,
            target: opts.target,
            kind: 'belongsToOne',
            maxCount: 1,
            local: opts.local,
            ...(opts.filter !== undefined && { filter: opts.filter }),
            ...(opts.where && { where: opts.where }),
        };

        if (opts.through) {
            // Read-only property (the owning side manages the link)
            applyPropertyMetadata({
                through: opts.through,
                readOnly: true,
                local: opts.local,
            })(target, key);
        } else {
            Object.defineProperty(target, key, { configurable: true, writable: true });
        }
    };
}

/**
 * Decorator for defining the inverse side of a many-to-many relation.
 *
 * @category Decorators
 *
 * @description
 * Declares the non-owning (inverse) side of a many-to-many relationship.
 * The property is a read-only relation since the owning side manages links.
 *
 * Supports two calling conventions:
 * ```typescript
 * @BelongsToMany({ through: "post://tag", target: () => Post })
 * @BelongsToMany(() => Post, { through: "post://tag" })
 * ```
 *
 * @example
 * ```typescript
 * @Model({ name: "Tag" })
 * class Tag extends Ad4mModel {
 *   @BelongsToMany(() => Post, { through: "post://tag" })
 *   posts: string[] = [];
 * }
 * ```
 */
export function BelongsToMany(opts: RelationOptions): PropertyDecorator;
export function BelongsToMany(target: () => Ad4mModelLike, opts?: Omit<RelationOptions, 'target'>): PropertyDecorator;
export function BelongsToMany(
    first: (() => Ad4mModelLike) | RelationOptions,
    second?: Omit<RelationOptions, 'target'>,
): PropertyDecorator {
    const opts = resolveRelationArgs(first, second);
    return function <T>(target: T, key: keyof T) {
        const ctor = (target as any).constructor;
        if (!relationRegistry.has(ctor)) relationRegistry.set(ctor, {});
        const map = relationRegistry.get(ctor)!;
        map[key as string] = {
            predicate: opts.through,
            target: opts.target,
            kind: 'belongsToMany',
            local: opts.local,
            ...(opts.getter && { getter: opts.getter }),
            ...(opts.filter !== undefined && { filter: opts.filter }),
            ...(opts.where && { where: opts.where }),
        };

        // @BelongsToMany is the inverse/read-only side — do NOT generate add*/remove*/set*
        // prototype methods.  Mutation must go through the owning side's @HasMany decorator.
        const relKey = key as string;
        Object.defineProperty(target, relKey, { configurable: true, writable: true });
    };
}

