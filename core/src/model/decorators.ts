import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { Subject } from "./Subject";
import { capitalize, propertyNameToSetterName, singularToPlural, stringifyObjectLiteral } from "./util";
import { SHACLShape, SHACLPropertyShape } from "../shacl/SHACLShape";

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
    target: () => Ad4mModelLike;
    kind: 'hasMany' | 'hasOne' | 'belongsToOne' | 'belongsToMany';
    /** Optional SurrealDB WHERE clause for filtering */
    condition?: string;
    /** Optional Prolog condition for filtering */
    prologCondition?: string;
    local?: boolean;
}

/** Registry of property metadata keyed by constructor → { propName → metadata } */
const propertyRegistry = new WeakMap<Function, Record<string, PropertyMetadataEntry>>();

/** Registry of relation metadata keyed by constructor → { propName → metadata } */
const relationRegistry = new WeakMap<Function, Record<string, RelationMetadataEntry>>();

/** Registry of collection metadata keyed by constructor → { propName → CollectionOptions } */
const collectionRegistry = new WeakMap<Function, Record<string, CollectionOptions>>();

/**
 * Retrieve property metadata for a given class constructor.
 * Walks the prototype chain so subclass decorators compose with parent decorators.
 */
export function getPropertiesMetadata(ctor: Function): Record<string, PropertyMetadataEntry> {
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
    return result;
}

/**
 * Retrieve relation metadata for a given class constructor.
 * Walks the prototype chain so subclass decorators compose with parent decorators.
 */
export function getRelationsMetadata(ctor: Function): Record<string, RelationMetadataEntry> {
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
    return result;
}

/**
 * Retrieve collection metadata for a given class constructor.
 * Walks the prototype chain so subclass decorators compose with parent decorators.
 * @deprecated Use getRelationsMetadata() for typed relation info. This function
 *             returns raw CollectionOptions used internally by SDNA/SHACL generators.
 */
export function getCollectionsMetadata(ctor: Function): Record<string, CollectionOptions> {
    const result: Record<string, CollectionOptions> = {};
    const chain: Function[] = [];
    let current = ctor;
    while (current && current !== Object) {
        chain.unshift(current);
        current = Object.getPrototypeOf(current);
    }
    for (const c of chain) {
        const meta = collectionRegistry.get(c);
        if (meta) Object.assign(result, meta);
    }
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
 * Programmatically register collection metadata for a given constructor.
 * Used by `fromJSONSchema()` and other dynamic model builders.
 * @deprecated Prefer registering via relation decorators.
 */
export function setCollectionRegistryEntry(
    ctor: Function,
    collName: string,
    meta: CollectionOptions,
): void {
    if (!collectionRegistry.has(ctor)) collectionRegistry.set(ctor, {});
    collectionRegistry.get(ctor)![collName] = meta;
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
 * Convert a model instance to a plain serializable object.
 * Reads the property metadata and extracts values from the instance.
 */
export function instanceToSerializable(instance: any): Record<string, any> {
    const ctor = instance.constructor;
    const props = getPropertiesMetadata(ctor);
    const result: Record<string, any> = {};
    for (const [key, _meta] of Object.entries(props)) {
        result[key] = instance[key];
    }
    return result;
}

/**
 * Generate a random identifier string (lowercase alpha).
 * Generate a random identifier string of the given length (lowercase alpha).
 */
export function makeRandomId(length: number): string {
    let result = '';
    const characters = 'abcdefghijklmnopqrstuvwxyz';
    const charactersLength = characters.length;
    for (let i = 0; i < length; i++) {
        result += characters.charAt(Math.floor(Math.random() * charactersLength));
    }
    return result;
}

export class PerspectiveAction {
    action: string
    source: string
    predicate: string
    target: string
}

export function addLink(source: string, predicate: string, target: string): PerspectiveAction {
    return {
        action: "addLink",
        source,
        predicate,
        target,
    };
}

export function hasLink(predicate: string): string {
    return `triple(this, "${predicate}", _)`
}

export interface InstanceQueryParams {
/**
 * An object representing the WHERE clause of the query.
 */
where?: object;

/**
 * A string representing the Prolog condition clause of the query.
 */
prologCondition?: string;
}

/**
 * Decorator for querying instances of a model class.
 * 
 * @category Decorators
 * 
 * @description
 * Allows you to define static query methods on your model class to retrieve instances based on custom conditions.
 * This decorator can only be applied to static async methods that return a Promise of an array of model instances.
 * 
 * The query can be constrained using either:
 * - A `where` clause that matches property values
 * - A custom Prolog `condition` for more complex queries
 * 
 * @example
 * ```typescript
 * class Recipe extends Ad4mModel {
 *   @Property({ through: "recipe://name" })
 *   name: string = "";
 * 
 *   @Property({ through: "recipe://rating" })
 *   rating: number = 0;
 * 
 *   // Get all recipes
 *   @InstanceQuery()
 *   static async all(perspective: PerspectiveProxy): Promise<Recipe[]> { return [] }
 * 
 *   // Get recipes by name
 *   @InstanceQuery({ where: { name: "Chocolate Cake" }})
 *   static async findByName(perspective: PerspectiveProxy): Promise<Recipe[]> { return [] }
 * 
 *   // Get highly rated recipes using a custom condition
 *   @InstanceQuery({ prologCondition: "triple(Instance, 'recipe://rating', Rating), Rating > 4" })
 *   static async topRated(perspective: PerspectiveProxy): Promise<Recipe[]> { return [] }
 * }
 * ```
 * 
 * @param {Object} [options] - Query options
 * @param {object} [options.where] - Object with property-value pairs to match
 * @param {string} [options.prologCondition] - Custom Prolog condition for more complex queries
 */
export function InstanceQuery(options?: InstanceQueryParams) {
    return function <T>(target: T, key: keyof T, descriptor: PropertyDescriptor) {
        const originalMethod = descriptor.value;
        if(typeof originalMethod !== "function") {
            throw new Error("InstanceQuery decorator can only be applied to methods");
        }

        descriptor.value = async function(perspective: PerspectiveProxy): Promise<T[]> {
            let instances: T[] = []
            //@ts-ignore
            let subjectClassName = target.name
            let query = `subject_class("${subjectClassName}", C), instance(C, Instance)`
            if(options && options.where) {
                for(let prop in options.where) {
                    let value = options.where[prop]
                    query += `, property_getter(C, Instance, "${prop}", "${value}")`
                }
            }

            if(options && options.prologCondition) {
                query += ', ' + options.prologCondition
            }

            // Try Prolog first
            try {
                let results = await perspective.infer(query)
                if(results && results !== false && typeof results !== "string" && results.length > 0) {
                    for(let result of results) {
                        let instance = result.Instance
                        let subject = new Subject(perspective, instance, subjectClassName)
                        await subject.init()
                        instances.push(subject as T)
                    }
                    return instances
                }
            } catch (e) {
                // Prolog failed, fall through to SurrealDB
            }

            // Fallback to SurrealDB (SdnaOnly mode)
            // Get all instances first - pass the class constructor, not just the name
            let allInstances = await perspective.getAllSubjectInstances(target)

            // Filter by where clause if provided
            if(options && options.where) {
                let filtered = []
                for(let instance of allInstances) {
                    let matches = true
                    for(let prop in options.where) {
                        let expectedValue = options.where[prop]
                        //@ts-ignore
                        let actualValue = await instance[prop]
                        if(actualValue !== expectedValue) {
                            matches = false
                            break
                        }
                    }
                    if(matches) {
                        filtered.push(instance as T)
                    }
                }
                return filtered
            }

            return allInstances as T[]
        }
    };
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
     * Custom SurrealQL getter to resolve the property value. Use this for custom graph traversals.
     * The expression can reference 'Base' which will be replaced with the instance's base expression.
     * Example: "(<-link[WHERE predicate = 'flux://has_reply'].in.uri)[0]"
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

        if (!opts.through && !opts.prologGetter) {
            throw new Error("SubjectProperty requires either 'through' or 'prologGetter' option")
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

interface WhereOptions {
    isInstance?: any
    prologCondition?: string
    condition?: string
}

export interface CollectionOptions {
    /**
     * The predicate of the property. All properties must have this option.
     */
    through: string;

    /**
     * An object representing the WHERE clause of the query.
     */
    where?: WhereOptions;

    /**
     * Custom SurrealQL getter to resolve the collection values. Use this for custom graph traversals.
     * The expression can reference 'Base' which will be replaced with the instance's base expression.
     * Example: "(<-link[WHERE predicate = 'flux://has_reply'].in.uri)"
     */
    getter?: string;

    /**
     * Indicates whether the property is stored locally in the perspective and not in the network. Useful for properties that are not meant to be shared with the network.
     */
    local?: boolean;
}

/**
 * Decorator for defining collections on model classes.
 * 
 * @category Decorators
 * 
 * @description
 * Defines a property that represents a collection of values linked to the model instance.
 * Collections are always arrays and support operations for adding, removing, and setting values.
 * 
 * For each collection property, the following methods are automatically generated:
 * - `addX(value)` - Add a value to the collection
 * - `removeX(value)` - Remove a value from the collection
 * - `setX(values)` - Replace all values in the collection
 * 
 * Where X is the capitalized property name.
 * 
 * Collections can be filtered using the `where` option to only include values that:
 * - Are instances of a specific model class
 * - Match a custom Prolog condition
 * 
 * @example
 * ```typescript
 * class Recipe extends Ad4mModel {
 *   // Basic collection of ingredients
 *   @Collection({ 
 *     through: "recipe://ingredient" 
 *   })
 *   ingredients: string[] = [];
 * 
 *   // Collection that only includes instances of another model
 *   @Collection({
 *     through: "recipe://comment",
 *     where: { isInstance: Comment }
 *   })
 *   comments: string[] = [];
 * 
 *   // Collection with custom Prolog filter condition
 *   @Collection({
 *     through: "recipe://step",
 *     where: { prologCondition: `triple(Target, "step://order", Order), Order < 3` }
 *   })
 *   firstSteps: string[] = [];
 * 
 *   // Collection with custom SurrealDB filter condition
 *   @Collection({
 *     through: "recipe://entries",
 *     where: { condition: `WHERE in.uri = Target AND predicate = 'recipe://has_ingredient' AND out.uri = 'recipe://test')`
 *   })
 *   ingredients: string[] = [];
 * 
 *   // Local-only collection not shared with network
 *   @Collection({
 *     through: "recipe://note",
 *     local: true
 *   })
 *   privateNotes: string[] = [];
 * }
 * 
 * // Using the generated methods:
 * const recipe = new Recipe(perspective);
 * await recipe.addIngredients("ingredient://flour");
 * await recipe.removeIngredients("ingredient://sugar");
 * await recipe.setIngredients(["ingredient://butter", "ingredient://eggs"]);
 * ```
 * 
 * @param {CollectionOptions} opts - Collection configuration
 * @param {string} opts.through - The predicate URI for collection links
 * @param {WhereOptions} [opts.where] - Filter conditions for collection values
 * @param {any} [opts.where.isInstance] - Model class to filter instances by
 * @param {string} [opts.where.prologCondition] - Custom Prolog condition for filtering
 * @param {boolean} [opts.local] - Whether collection links are stored locally only
 */
function Collection(opts: CollectionOptions) {
    return function <T>(target: T, key: keyof T) {
        // Write to WeakMap registry
        const ctor = (target as any).constructor;
        if (!collectionRegistry.has(ctor)) collectionRegistry.set(ctor, {});
        collectionRegistry.get(ctor)![key as string] = opts;

        const value = key as string
        target[`add${capitalize(value)}`] = () => {}
        target[`remove${capitalize(value)}`] = () => {}
        target[`set${capitalize(value)}`] = () => {}

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
 * - Generates the necessary SDNA code for the model's properties and collections
 * - Enables the use of other model decorators (@Property, @Collection, etc.)
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
 *   @Collection({ through: "recipe://ingredient" })
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
            let sdna = ""
            let subjectName = opts.name
            let obj = target.prototype;

            let uuid = makeRandomId(8)

            sdna += `subject_class("${subjectName}", ${uuid}).\n`


            let classRemoverActions = []

            let constructorActions = []
            if(obj.subjectConstructor && obj.subjectConstructor.length) {
                constructorActions = constructorActions.concat(obj.subjectConstructor)
            }

            let instanceConditions = []
            if(obj.isSubjectInstance && obj.isSubjectInstance.length) {
                instanceConditions = instanceConditions.concat(obj.isSubjectInstance)
            }

            let propertiesCode = []
            let properties = getPropertiesMetadata(target)
            for(let property in properties) {
                let propertyCode = `property(${uuid}, "${property}").\n`

                let { through, initial, required, resolveLanguage, writable, flag, prologGetter, prologSetter, local } = properties[property]

                if(resolveLanguage) {
                    propertyCode += `property_resolve(${uuid}, "${property}").\n`
                    propertyCode += `property_resolve_language(${uuid}, "${property}", "${resolveLanguage}").\n`
                }

                if(prologGetter) {
                    propertyCode += `property_getter(${uuid}, Base, "${property}", Value) :- ${prologGetter}.\n`
                } else if(through) {
                    propertyCode += `property_getter(${uuid}, Base, "${property}", Value) :- triple(Base, "${through}", Value).\n`

                    if(required) {
                        if(flag) {
                            instanceConditions.push(`triple(Base, "${through}", "${initial}")`)
                        } else {
                            instanceConditions.push(`triple(Base, "${through}", _)`)
                        }
                    }
                }

                if(prologSetter) {
                    propertyCode += `property_setter(${uuid}, "${property}", Actions) :- ${prologSetter}.\n`
                } else if (writable && through) {
                    let setter = obj[propertyNameToSetterName(property)]
                    if(typeof setter === "function") {
                        let action = [{
                            action: "setSingleTarget",
                            source: "this",
                            predicate: through,
                            target: "value",
                            ...(local && { local: true })
                        }]
                        propertyCode += `property_setter(${uuid}, "${property}", '${stringifyObjectLiteral(action)}').\n`
                    }
                }

                propertiesCode.push(propertyCode)

                // Auto-derive effectiveInitial for writable, non-flag properties
                // so constructor/destructor always handle the property link
                const effectiveInitial = initial
                    ?? (writable && !flag && through ? "literal://string:" : undefined);

                if(effectiveInitial) {
                    constructorActions.push({
                        action: "addLink",
                        source: "this",
                        predicate: through,
                        target: effectiveInitial,
                    })

                    classRemoverActions.push({
                        action: "removeLink",
                        source: "this",
                        predicate: through,
                        target: "*",
                    })
                }
            }

            let collectionsCode = []
            let collections = getCollectionsMetadata(target)
            for(let collection in collections) {
                let collectionCode = `collection(${uuid}, "${collection}").\n`

                let { through, where, local} = collections[collection]

                if(through) {
                    if(where) {
                        if(!where.isInstance && !where.prologCondition && !where.condition) {
                            throw "'where' needs one of 'isInstance', 'prologCondition', or 'condition'"
                        }

                        let conditions = []

                        if(where.isInstance) {
                            let otherClass
                            if(where.isInstance.name) {
                                otherClass = where.isInstance.name
                            } else {
                                otherClass = where.isInstance
                            }
                            conditions.push(`instance(OtherClass, Target), subject_class("${otherClass}", OtherClass)`)
                        }

                        if(where.prologCondition) {
                            conditions.push(where.prologCondition)
                        }

                        // If there are Prolog conditions (isInstance or prologCondition), use setof with conditions
                        // If only condition is present, use simple findall (SurrealDB will filter later)
                        if(conditions.length > 0) {
                            const conditionString = conditions.join(", ")
                            collectionCode += `collection_getter(${uuid}, Base, "${collection}", List) :- setof(Target, (triple(Base, "${through}", Target), ${conditionString}), List).\n`
                        } else {
                            // Only SurrealDB condition present (no Prolog filtering)
                            collectionCode += `collection_getter(${uuid}, Base, "${collection}", List) :- findall(C, triple(Base, "${through}", C), List).\n`
                        }
                    } else {
                        collectionCode += `collection_getter(${uuid}, Base, "${collection}", List) :- findall(C, triple(Base, "${through}", C), List).\n`
                    }

                    let collectionAdderAction = [{
                        action: "addLink",
                        source: "this",
                        predicate: through,
                        target: "value",
                        ...(local && { local: true })
                    }]

                    let collectionRemoverAction = [{
                        action: "removeLink",
                        source: "this",
                        predicate: through,
                        target: "value",
                    }]

                    let collectionSetterAction = [{
                        action: "collectionSetter",
                        source: "this",
                        predicate: through,
                        target: "value",
                        ...(local && { local: true })
                    }]
                    collectionCode += `collection_adder(${uuid}, "${collection}", '${stringifyObjectLiteral(collectionAdderAction)}').\n`
                    collectionCode += `collection_remover(${uuid}, "${collection}", '${stringifyObjectLiteral(collectionRemoverAction)}').\n`
                    collectionCode += `collection_setter(${uuid}, "${collection}", '${stringifyObjectLiteral(collectionSetterAction)}').\n`
                }

                collectionsCode.push(collectionCode)
            }

            let subjectContructorJSONString = stringifyObjectLiteral(constructorActions)
            sdna += `constructor(${uuid}, '${subjectContructorJSONString}').\n`
            if(instanceConditions.length > 0) {
                let instanceConditionProlog = instanceConditions.join(", ")
                sdna += `instance(${uuid}, Base) :- ${instanceConditionProlog}.\n`
                sdna += "\n"
            }
            sdna += `destructor(${uuid}, '${stringifyObjectLiteral(classRemoverActions)}').\n`
            sdna += "\n"
            sdna += propertiesCode.join("\n")
            sdna += "\n"
            sdna += collectionsCode.join("\n")

            return {
                sdna,
                name: subjectName
            }
        }

        // Generate SHACL shape (W3C standard + AD4M action definitions)
        target.generateSHACL = function() {
            const subjectName = opts.name;
            const obj = target.prototype;

            // Determine namespace from first property or collection, or use default
            let namespace = "ad4m://";
            const properties = getPropertiesMetadata(target);
            const collections = getCollectionsMetadata(target);
            
            // Try properties first
            if (Object.keys(properties).length > 0) {
                const firstProp = properties[Object.keys(properties)[0]];
                if (firstProp.through) {
                    // Extract namespace from through predicate (e.g., "recipe://name" -> "recipe://")
                    const match = firstProp.through.match(/^([^:]+:\/\/)/);
                    if (match) {
                        namespace = match[1];
                    }
                }
            } 
            // Fall back to collections if no properties
            else if (Object.keys(collections).length > 0) {
                const firstColl = collections[Object.keys(collections)[0]];
                if (firstColl.through) {
                    const match = firstColl.through.match(/^([^:]+:\/\/)/);
                    if (match) {
                        namespace = match[1];
                    }
                }
            }

            // Create SHACL shape
            const shapeUri = `${namespace}${subjectName}Shape`;
            const targetClass = `${namespace}${subjectName}`;
            const shape = new SHACLShape(shapeUri, targetClass);

            // Detect @Model inheritance — if the parent class also has
            // generateSHACL it is itself a @Model and we reference its shape
            // via sh:node so SHACL validators can walk the hierarchy.
            const parentCtor = Object.getPrototypeOf(target);
            if (parentCtor && typeof parentCtor.generateSHACL === 'function') {
                const parentSHACL = parentCtor.generateSHACL();
                if (parentSHACL?.shape?.nodeShapeUri) {
                    shape.addParentShape(parentSHACL.shape.nodeShapeUri);
                }
            }

            // === Extract Constructor Actions (same logic as generateSDNA) ===
            let constructorActions = [];
            if(obj.subjectConstructor && obj.subjectConstructor.length) {
                constructorActions = constructorActions.concat(obj.subjectConstructor);
            }

            // === Extract Destructor Actions ===
            let destructorActions = [];
            
            // Convert properties to SHACL property shapes
            for (const propName in properties) {
                const propMeta = properties[propName];

                if (!propMeta.through) continue; // Skip properties without predicates

                const propShape: SHACLPropertyShape = {
                    name: propName, // Property name for generating named URIs
                    path: propMeta.through,
                };

                // Determine datatype from initial value or resolveLanguage
                if (propMeta.resolveLanguage === "literal") {
                    // If it resolves via literal language, it's likely a string
                    propShape.datatype = "xsd://string";
                } else if (propMeta.initial) {
                    // Try to infer from initial value type
                    const initialType = typeof obj[propName];
                    if (initialType === "number") {
                        propShape.datatype = "xsd://integer";
                    } else if (initialType === "boolean") {
                        propShape.datatype = "xsd://boolean";
                    } else if (initialType === "string") {
                        propShape.datatype = "xsd://string";
                    }
                }

                // Cardinality constraints
                if (propMeta.required) {
                    propShape.minCount = 1;
                }

                // Single-valued properties get maxCount 1
                // (collections are handled separately below)
                propShape.maxCount = 1;

                // Flag properties have fixed value
                if (propMeta.flag && propMeta.initial) {
                    propShape.hasValue = propMeta.initial;
                }

                // AD4M-specific metadata
                if (propMeta.local !== undefined) {
                    propShape.local = propMeta.local;
                }

                if (propMeta.writable !== undefined) {
                    propShape.writable = propMeta.writable;
                }

                if (propMeta.resolveLanguage) {
                    propShape.resolveLanguage = propMeta.resolveLanguage;
                }

                // === Extract Setter Actions (same logic as generateSDNA) ===
                if (propMeta.prologSetter) {
                    // Custom Prolog setter defined - not yet supported in SHACL
                    console.warn(
                        `[SHACL Generation] Custom Prolog setter for property '${propName}' in class '${subjectName}' is not yet supported. ` +
                        `The property will be created without setter actions. Consider using standard writable properties or provide explicit SHACL JSON.`
                    );
                    // TODO: Parse custom Prolog setter to extract actions
                } else if (propMeta.writable && propMeta.through) {
                    let setter = obj[propertyNameToSetterName(propName)];
                    if (typeof setter === "function") {
                        propShape.setter = [{
                            action: "setSingleTarget",
                            source: "this",
                            predicate: propMeta.through,
                            target: "value",
                            ...(propMeta.local && { local: true })
                        }];
                    }
                }

                // Add to constructor actions if property has initial value
                // Auto-derive effectiveInitial for writable, non-flag properties
                const effectiveInitial = propMeta.initial
                    ?? (propMeta.writable && !propMeta.flag && propMeta.through
                        ? "literal://string:" : undefined);

                if (effectiveInitial) {
                    constructorActions.push({
                        action: "addLink",
                        source: "this",
                        predicate: propMeta.through,
                        target: effectiveInitial,
                    });

                    // Add to destructor actions
                    destructorActions.push({
                        action: "removeLink",
                        source: "this",
                        predicate: propMeta.through,
                        target: "*",
                    });
                }

                shape.addProperty(propShape);
            }
            
            // Convert collections to SHACL property shapes
            // (collections variable already declared above for namespace inference)
            for (const collName in collections) {
                const collMeta = collections[collName];
                
                if (!collMeta.through) continue;
                
                const collShape: SHACLPropertyShape = {
                    name: collName, // Collection name for generating named URIs
                    path: collMeta.through,
                    // Collections have no maxCount (unlimited)
                    // minCount defaults to 0 (optional)
                };
                
                // Determine if it's a reference (IRI) or literal
                // Collections typically contain references (IRIs) to other entities
                // They're literals only if explicitly marked or contain primitive values
                if (collMeta.where?.isInstance) {
                    // Collection of typed entities - definitely IRIs
                    collShape.nodeKind = 'IRI';
                } else {
                    // Default to IRI for collections (most common case)
                    // Literal collections are rare and would need explicit marking
                    collShape.nodeKind = 'IRI';
                }
                
                // AD4M-specific metadata
                if (collMeta.local !== undefined) {
                    collShape.local = collMeta.local;
                }

                // === Extract Collection Actions (adder/remover) ===
                // Adder action - adds a link to the collection
                collShape.adder = [{
                    action: "addLink",
                    source: "this",
                    predicate: collMeta.through,
                    target: "value",
                    ...(collMeta.local && { local: true })
                }];

                // Remover action - removes a link from the collection
                collShape.remover = [{
                    action: "removeLink",
                    source: "this",
                    predicate: collMeta.through,
                    target: "value",
                    ...(collMeta.local && { local: true })
                }];

                shape.addProperty(collShape);
            }

            // Set constructor and destructor actions on the shape
            if (constructorActions.length > 0) {
                shape.setConstructorActions(constructorActions);
            }
            if (destructorActions.length > 0) {
                shape.setDestructorActions(destructorActions);
            }

            return {
                shape,
                name: subjectName
            };
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
 * - `required` → `true`
 * - `readOnly` → `false`
 * - `resolveLanguage` → `"literal"`
 * - `initial` → `"literal://string:uninitialized"` (when required)
 * 
 * @example
 * ```typescript
 * class User extends Ad4mModel {
 *   // Basic required property with default initial value
 *   @Property({
 *     through: "user://name"
 *   })
 *   name: string = "";
 * 
 *   // Required property with custom initial value
 *   @Property({
 *     through: "user://status",
 *     initial: "user://active"
 *   })
 *   status: string = "";
 * 
 *   // Required property with literal resolution
 *   @Property({
 *     through: "user://bio",
 *     resolveLanguage: "literal"
 *   })
 *   bio: string = "";
 * 
 *   // Required property with custom getter/setter
 *   @Property({
 *     through: "user://age",
 *     getter: `triple(Base, "user://birthYear", Year), Value is 2024 - Year`,
 *     setter: `Year is 2024 - Value, Actions = [{"action": "setSingleTarget", "source": "this", "predicate": "user://birthYear", "target": Year}]`
 *   })
 *   age: number = 0;
 * }
 * ```
 * 
 * @param {PropertyOptions} opts - Property configuration
 * @param {string} opts.through - The predicate URI for the property
 * @param {string} [opts.initial] - Initial value (defaults to "literal://string:uninitialized")
 * @param {string} [opts.resolveLanguage] - Language to use for value resolution (e.g. "literal")
 * @param {string} [opts.prologGetter] - Custom Prolog code for getting the property value
 * @param {string} [opts.prologSetter] - Custom Prolog code for setting the property value
 * @param {boolean} [opts.local] - Whether the property should only be stored locally
 */
export function Property(opts: PropertyOptions) {
    const required = opts.required ?? true;
    return applyPropertyMetadata({
        ...opts,
        required,
        readOnly: opts.readOnly ?? false,
        resolveLanguage: opts.resolveLanguage ?? "literal",
        initial: opts.initial ?? (required ? "literal://string:uninitialized" : undefined),
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
    return applyPropertyMetadata({
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
    /** The predicate URI used to link the two models */
    through: string;
    /** The target model class (use a thunk to avoid circular-dependency issues). Optional for untyped string collections. */
    target?: () => Ad4mModelLike;
    /** Optional SurrealDB WHERE clause for filtering */
    condition?: string;
    /** Optional Prolog condition for filtering */
    prologCondition?: string;
    /** Whether the link is stored locally (not shared on the network) */
    local?: boolean;
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
    [K in Keys as `add${Capitalize<K>}`]: (value: string) => Promise<void>;
} & {
    [K in Keys as `remove${Capitalize<K>}`]: (value: string) => Promise<void>;
} & {
    [K in Keys as `set${Capitalize<K>}`]: (values: string[]) => Promise<void>;
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
        ? { ...second!, target: first }
        : first;

    if (!opts.through) {
        throw new Error(
            `Relation decorator requires a { through: '...' } option specifying the predicate URI.`
        );
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
 * creates the corresponding `@Collection` entry so that the SDNA / SHACL
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
export function HasMany(target: () => Ad4mModelLike, opts: Omit<RelationOptions, 'target'>): PropertyDecorator;
export function HasMany(
    first: (() => Ad4mModelLike) | RelationOptions,
    second?: Omit<RelationOptions, 'target'>,
): PropertyDecorator {
    const opts = resolveRelationArgs(first, second);
    return function <T>(target: T, key: keyof T) {
        // --- relation registry ---
        const ctor = (target as any).constructor;
        if (!relationRegistry.has(ctor)) relationRegistry.set(ctor, {});
        const map = relationRegistry.get(ctor)!;
        map[key as string] = {
            predicate: opts.through,
            target: opts.target,
            kind: 'hasMany',
            condition: opts.condition,
            prologCondition: opts.prologCondition,
            local: opts.local,
        };

        // --- also register as a collection so existing SDNA/SHACL generators work ---
        const collectionOpts: CollectionOptions = {
            through: opts.through,
            local: opts.local,
        };
        if (opts.prologCondition || opts.condition) {
            collectionOpts.where = {};
            if (opts.prologCondition) collectionOpts.where.prologCondition = opts.prologCondition;
            if (opts.condition) collectionOpts.where.condition = opts.condition;
        }
        // Delegate to Collection decorator logic
        Collection(collectionOpts)(target, key);
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
export function HasOne(target: () => Ad4mModelLike, opts: Omit<RelationOptions, 'target'>): PropertyDecorator;
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
            local: opts.local,
        };

        // Register as a writable property
        applyPropertyMetadata({
            through: opts.through,
            readOnly: false,
            local: opts.local,
        })(target, key);
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
export function BelongsToOne(target: () => Ad4mModelLike, opts: Omit<RelationOptions, 'target'>): PropertyDecorator;
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
            local: opts.local,
        };

        // Read-only property (the owning side manages the link)
        applyPropertyMetadata({
            through: opts.through,
            readOnly: true,
            local: opts.local,
        })(target, key);
    };
}

/**
 * Decorator for defining the inverse side of a many-to-many relation.
 *
 * @category Decorators
 *
 * @description
 * Declares the non-owning (inverse) side of a many-to-many relationship.
 * The property is a read-only collection since the owning side manages links.
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
export function BelongsToMany(target: () => Ad4mModelLike, opts: Omit<RelationOptions, 'target'>): PropertyDecorator;
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
            condition: opts.condition,
            prologCondition: opts.prologCondition,
            local: opts.local,
        };

        // Register as a collection (read-only — owning side manages links)
        const collectionOpts: CollectionOptions = {
            through: opts.through,
            local: opts.local,
        };
        if (opts.prologCondition || opts.condition) {
            collectionOpts.where = {};
            if (opts.prologCondition) collectionOpts.where.prologCondition = opts.prologCondition;
            if (opts.condition) collectionOpts.where.condition = opts.condition;
        }
        Collection(collectionOpts)(target, key);
    };
}

