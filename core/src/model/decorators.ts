import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { Subject } from "./Subject";
import { capitalize, propertyNameToSetterName, singularToPlural, stringifyObjectLiteral } from "./util";

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
 * A string representing the condition clause of the query.
 */
condition?: string;
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
 *   @InstanceQuery({ condition: "triple(Instance, 'recipe://rating', Rating), Rating > 4" })
 *   static async topRated(perspective: PerspectiveProxy): Promise<Recipe[]> { return [] }
 * }
 * ```
 * 
 * @param {Object} [options] - Query options
 * @param {object} [options.where] - Object with property-value pairs to match
 * @param {string} [options.condition] - Custom Prolog condition for more complex queries
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

            if(options && options.condition) {
                query += ', ' + options.condition
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
            // Get all instances first
            let allInstances = await perspective.getAllSubjectInstances(subjectClassName)

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
     * Indicates whether the property is writable. If true, a setter will be available in the prolog engine.
     */
    writable?: boolean;

    /**
     * The language used to store the property. Can be the default `Literal` Language or a custom language address.
     */
    resolveLanguage?: string;

    /**
     * Custom getter to get the value of the property in the prolog engine. If not provided, the default getter will be used.
     */
    getter?: string;

    /**
     * Custom setter to set the value of the property in the prolog engine. Only available if the property is writable.
     */
    setter?: string;

    /**
     * Custom SurrealQL getter to resolve the property value. Use this for custom graph traversals.
     * The expression can reference 'Base' which will be replaced with the instance's base expression.
     * Example: "(<-link[WHERE predicate = 'flux://has_reply'].in.uri)[0]"
     */
    surrealGetter?: string;

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
 * Decorator for defining optional properties on model classes.
 * 
 * @category Decorators
 * 
 * @description
 * The most flexible property decorator that allows you to define properties with full control over:
 * - Whether the property is required
 * - Whether the property is writable
 * - How values are stored and retrieved
 * - Custom getter/setter logic
 * - Local vs network storage
 * 
 * Both @Property and @ReadOnly are specialized versions of @Optional with preset configurations.
 * 
 * @example
 * ```typescript
 * class Recipe extends Ad4mModel {
 *   // Basic optional property
 *   @Optional({
 *     through: "recipe://description"
 *   })
 *   description?: string;
 * 
 *   // Optional property with custom initial value
 *   @Optional({
 *     through: "recipe://status",
 *     initial: "recipe://draft",
 *     required: true
 *   })
 *   status: string = "";
 * 
 *   // Read-only property with custom getter
 *   @Optional({
 *     through: "recipe://rating",
 *     writable: false,
 *     getter: `
 *       findall(Rating, triple(Base, "recipe://user_rating", Rating), Ratings),
 *       sum_list(Ratings, Sum),
 *       length(Ratings, Count),
 *       Value is Sum / Count
 *     `
 *   })
 *   averageRating: number = 0;
 * 
 *   // Property that resolves to a Literal and is stored locally
 *   @Optional({
 *     through: "recipe://notes",
 *     resolveLanguage: "literal",
 *     local: true
 *   })
 *   notes?: string;
 * 
 *   // Property with custom getter and setter logic
 *   @Optional({
 *     through: "recipe://ingredients",
 *     getter: `
 *       triple(Base, "recipe://ingredients", RawValue),
 *       atom_json_term(RawValue, Value)
 *     `,
 *     setter: `
 *       atom_json_term(Value, JsonValue),
 *       Actions = [{"action": "setSingleTarget", "source": "this", "predicate": "recipe://ingredients", "target": JsonValue}]
 *     `
 *   })
 *   ingredients: string[] = [];
 * }
 * ```
 * 
 * @param {PropertyOptions} opts - Property configuration options
 * @param {string} opts.through - The predicate URI for the property
 * @param {string} [opts.initial] - Initial value (required if property is required)
 * @param {boolean} [opts.required] - Whether the property must have a value
 * @param {boolean} [opts.writable=true] - Whether the property can be modified
 * @param {string} [opts.resolveLanguage] - Language to use for value resolution (e.g. "literal")
 * @param {string} [opts.getter] - Custom Prolog code for getting the property value
 * @param {string} [opts.setter] - Custom Prolog code for setting the property value
 * @param {boolean} [opts.local] - Whether the property should only be stored locally
 */
export function Optional(opts: PropertyOptions) {
    return function <T>(target: T, key: keyof T) {
        if(typeof opts.writable === "undefined" && opts.through) {
            opts.writable = true
        }
        
        if (opts.required && !opts.initial) {
            throw new Error("SubjectProperty requires an 'initial' option if 'required' is true");
        }

        if (!opts.through && !opts.getter) {
            throw new Error("SubjectProperty requires either 'through' or 'getter' option")
        }

        target["__properties"] = target["__properties"] || {};
        target["__properties"][key] = target["__properties"][key] || {};
        target["__properties"][key] = { ...target["__properties"][key], ...opts }

        if (opts.writable) {
            const value = key as string
            target[`set${capitalize(value)}`] = () => {}
        }

        Object.defineProperty(target, key, {configurable: true, writable: true});
    };
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

        target["__properties"] = target["__properties"] || {};
        target["__properties"][key] = target["__properties"][key] || {};
        target["__properties"][key] = {
            ...target["__properties"][key],
            through: opts.through,
            required: true,
            initial: opts.value,
            flag: true
        }

        // @ts-ignore
        target[key] = opts.value;

        Object.defineProperty(target, key, {configurable: true, writable: true});
    };
}

interface WhereOptions {
    isInstance?: any
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
    surrealGetter?: string;

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
 * - `setCollectionX(values)` - Replace all values in the collection
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
 *   // Collection with custom filter condition
 *   @Collection({
 *     through: "recipe://step",
 *     where: { condition: `triple(Target, "step://order", Order), Order < 3` }
 *   })
 *   firstSteps: string[] = [];
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
 * await recipe.setCollectionIngredients(["ingredient://butter", "ingredient://eggs"]);
 * ```
 * 
 * @param {CollectionOptions} opts - Collection configuration
 * @param {string} opts.through - The predicate URI for collection links
 * @param {WhereOptions} [opts.where] - Filter conditions for collection values
 * @param {any} [opts.where.isInstance] - Model class to filter instances by
 * @param {string} [opts.where.condition] - Custom Prolog condition for filtering
 * @param {boolean} [opts.local] - Whether collection links are stored locally only
 */
export function Collection(opts: CollectionOptions) {
    return function <T>(target: T, key: keyof T) {
        target["__collections"] = target["__collections"] || {};
        target["__collections"][key] = opts;

        const value = key as string
        target[`add${capitalize(value)}`] = () => {}
        target[`remove${capitalize(value)}`] = () => {}
        target[`setCollection${capitalize(value)}`] = () => {}

        Object.defineProperty(target, key, {configurable: true, writable: true});
    };
}

export function makeRandomPrologAtom(length: number): string {
    let result = '';
    let characters = 'abcdefghijklmnopqrstuvwxyz';
    let charactersLength = characters.length;
    for (let i = 0; i < length; i++) {
       result += characters.charAt(Math.floor(Math.random() * charactersLength));
    }
    return result;
 }

export interface ModelOptionsOptions {
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
 * @ModelOptions({ name: "Recipe" })
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
 * @param {ModelOptionsOptions} opts - Model configuration
 * @param {string} opts.name - Unique name for the model class in AD4M
 */
export function ModelOptions(opts: ModelOptionsOptions) {
    return function (target: any) {
        target.prototype.className = opts.name;
        target.className = opts.name;

        target.generateSDNA = function() {
            let sdna = ""
            let subjectName = opts.name
            let obj = target.prototype;

            let uuid = makeRandomPrologAtom(8)

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
            let properties = obj.__properties || {}
            for(let property in properties) {
                let propertyCode = `property(${uuid}, "${property}").\n`

                let { through, initial, required, resolveLanguage, writable, flag, getter, setter, local } = properties[property]

                if(resolveLanguage) {
                    propertyCode += `property_resolve(${uuid}, "${property}").\n`
                    propertyCode += `property_resolve_language(${uuid}, "${property}", "${resolveLanguage}").\n`
                }

                if(getter) {
                    propertyCode += `property_getter(${uuid}, Base, "${property}", Value) :- ${getter}.\n`
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

                if(setter) {
                    propertyCode += `property_setter(${uuid}, "${property}", Actions) :- ${setter}.\n`
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

                if(initial) {
                    constructorActions.push({
                        action: "addLink",
                        source: "this",
                        predicate: through,
                        target: initial,
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
            let collections = obj.__collections || {}
            for(let collection in collections) {
                let collectionCode = `collection(${uuid}, "${collection}").\n`

                let { through, where, local} = collections[collection]

                if(through) {
                    if(where) {
                        if(!where.isInstance && !where.condition) {
                            throw "'where' needs one of 'isInstance' or 'condition'"
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

                        if(where.condition) {
                            conditions.push(where.condition)
                        }

                        const conditionString = conditions.join(", ")

                        collectionCode += `collection_getter(${uuid}, Base, "${collection}", List) :- setof(Target, (triple(Base, "${through}", Target), ${conditionString}), List).\n`
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
                    collectionCode += `collection_adder(${uuid}, "${singularToPlural(collection)}", '${stringifyObjectLiteral(collectionAdderAction)}').\n`
                    collectionCode += `collection_remover(${uuid}, "${singularToPlural(collection)}", '${stringifyObjectLiteral(collectionRemoverAction)}').\n`
                    collectionCode += `collection_setter(${uuid}, "${singularToPlural(collection)}", '${stringifyObjectLiteral(collectionSetterAction)}').\n`
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

        Object.defineProperty(target, 'type', {configurable: true});
    }
}

/**
 * Decorator for defining required and writable properties on model classes.
 * 
 * @category Decorators
 * 
 * @description
 * A convenience decorator that defines a required property that must have an initial value and is writable by default.
 * This is equivalent to using @Optional with `required: true` and `writable: true`.
 * 
 * Properties defined with this decorator:
 * - Must have a value (required)
 * - Can be modified after creation (writable)
 * - Default to "literal://string:uninitialized" if no initial value is provided
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
 * @param {string} [opts.getter] - Custom Prolog code for getting the property value
 * @param {string} [opts.setter] - Custom Prolog code for setting the property value
 * @param {boolean} [opts.local] - Whether the property should only be stored locally
 */
export function Property(opts: PropertyOptions) {
    return Optional({
        ...opts,
        required: true,
        writable: true,
        initial: opts.initial || "literal://string:uninitialized"
    });
}

/**
 * Decorator for defining read-only properties on model classes.
 * 
 * @category Decorators
 * 
 * @description
 * A convenience decorator that defines a property that can only be read and cannot be modified after initialization.
 * This is equivalent to using @Optional with `writable: false`.
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
 * @param {string} [opts.getter] - Custom Prolog code for getting the property value
 * @param {boolean} [opts.local] - Whether the property should only be stored locally
 */
export function ReadOnly(opts: PropertyOptions) {
    return Optional({
        ...opts,
        writable: false
    });
}