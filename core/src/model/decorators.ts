import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import {
  capitalize,
  propertyNameToSetterName,
  stringifyObjectLiteral,
} from "./util";
import { SHACLShape, SHACLPropertyShape } from "../shacl/SHACLShape";

export class PerspectiveAction {
  action: string;
  source: string;
  predicate: string;
  target: string;
}

export function addLink(
  source: string,
  predicate: string,
  target: string,
): PerspectiveAction {
  return {
    action: "addLink",
    source,
    predicate,
    target,
  };
}

export function hasLink(predicate: string): string {
  return `triple(this, "${predicate}", _)`;
}

export interface FieldOptions {
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
 * @param {FieldOptions} opts - Property configuration options
 * @param {string} opts.through - The predicate URI for the property
 * @param {string} [opts.initial] - Initial value (required if property is required)
 * @param {boolean} [opts.required] - Whether the property must have a value
 * @param {boolean} [opts.writable=true] - Whether the property can be modified
 * @param {string} [opts.resolveLanguage] - Language to use for value resolution (e.g. "literal")
 * @param {boolean} [opts.local] - Whether the property should only be stored locally
 */
export function Field(opts: FieldOptions) {
  return function <T>(target: T, key: keyof T) {
    if (typeof opts.writable === "undefined" && opts.through) {
      opts.writable = true;
    }

    if (!opts.through) {
      throw new Error("@Field requires a 'through' option");
    }

    target["__properties"] = target["__properties"] || {};
    target["__properties"][key] = target["__properties"][key] || {};
    target["__properties"][key] = { ...target["__properties"][key], ...opts };

    if (opts.writable) {
      const value = key as string;
      target[`set${capitalize(value)}`] = () => {};
    }

    Object.defineProperty(target, key, { configurable: true, writable: true });
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
      throw new Error("SubjectFlag requires a 'through' and 'value' option");
    }

    if (!opts.through) {
      throw new Error("SubjectFlag requires a 'through' option");
    }

    if (!opts.value) {
      throw new Error("SubjectFlag requires a 'value' option");
    }

    target["__properties"] = target["__properties"] || {};
    target["__properties"][key] = target["__properties"][key] || {};
    target["__properties"][key] = {
      ...target["__properties"][key],
      through: opts.through,
      required: true,
      initial: opts.value,
      flag: true,
    };

    // @ts-ignore
    target[key] = opts.value;

    Object.defineProperty(target, key, { configurable: true, writable: true });
  };
}

interface WhereOptions {
  isInstance?: any;
  prologCondition?: string;
  condition?: string;
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
 * await recipe.setCollectionIngredients(["ingredient://butter", "ingredient://eggs"]);
 * ```
 *
 * @param {CollectionOptions} opts - Collection configuration
 * @param {string} opts.through - The predicate URI for collection links
 * @param {WhereOptions} [opts.where] - Filter conditions for collection values
 * @param {any} [opts.where.isInstance] - Model class to filter instances by
 * @param {string} [opts.where.prologCondition] - Custom Prolog condition for filtering
 * @param {boolean} [opts.local] - Whether collection links are stored locally only
 */
/**
 * Utility type that generates the runtime methods produced by \@HasMany / \@Collection decorators.
 *
 * For each collection property `foo`, the decorator generates:
 * - `addFoo(value: string): Promise<void>`
 * - `removeFoo(value: string): Promise<void>`
 * - `setCollectionFoo(values: string[]): Promise<void>`
 *
 * Pass a string union of your \@HasMany property names and use interface merging —
 * this avoids the circular reference you'd get by passing the class itself:
 * @example
 * ```typescript
 * \@Model({ name: 'Post' })
 * export class Post extends Ad4mModel {
 *   \@HasMany({ through: 'post://comment' })
 *   comments: string[] = [];
 * }
 * export interface Post extends HasManyMethods<'comments'> {}
 * ```
 */
export type HasManyMethods<Keys extends string> = {
  [K in Keys as `add${Capitalize<K>}`]: (value: string) => Promise<void>;
} & {
  [K in Keys as `remove${Capitalize<K>}`]: (value: string) => Promise<void>;
} & {
  [K in Keys as `setCollection${Capitalize<K>}`]: (
    values: string[],
  ) => Promise<void>;
};

export function HasMany(opts: CollectionOptions) {
  return function <T>(target: T, key: keyof T) {
    target["__collections"] = target["__collections"] || {};
    target["__collections"][key] = { ...opts, direction: "forward" as const };

    const value = key as string;
    target[`add${capitalize(value)}`] = () => {};
    target[`remove${capitalize(value)}`] = () => {};
    target[`setCollection${capitalize(value)}`] = () => {};

    Object.defineProperty(target, key, { configurable: true, writable: true });
  };
}

export function HasOne(opts: CollectionOptions) {
  return function <T>(target: T, key: keyof T) {
    target["__collections"] = target["__collections"] || {};
    target["__collections"][key] = {
      ...opts,
      direction: "forward" as const,
      maxCount: 1,
    };

    const value = key as string;
    target[`add${capitalize(value)}`] = () => {};
    target[`remove${capitalize(value)}`] = () => {};
    target[`setCollection${capitalize(value)}`] = () => {};

    Object.defineProperty(target, key, { configurable: true, writable: true });
  };
}

export function BelongsToOne(relatedModel: () => any, opts: CollectionOptions) {
  return function <T>(target: T, key: keyof T) {
    target["__collections"] = target["__collections"] || {};
    target["__collections"][key] = {
      ...opts,
      direction: "reverse" as const,
      maxCount: 1,
      relatedModel,
    };

    Object.defineProperty(target, key, { configurable: true, writable: true });
  };
}

export function BelongsToMany(
  relatedModel: () => any,
  opts: CollectionOptions,
) {
  return function <T>(target: T, key: keyof T) {
    target["__collections"] = target["__collections"] || {};
    target["__collections"][key] = {
      ...opts,
      direction: "reverse" as const,
      relatedModel,
    };

    Object.defineProperty(target, key, { configurable: true, writable: true });
  };
}

export function makeRandomId(length: number): string {
  let result = "";
  let characters = "abcdefghijklmnopqrstuvwxyz";
  let charactersLength = characters.length;
  for (let i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
  return result;
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
 * @param {ModelConfig} opts - Model configuration
 * @param {string} opts.name - Unique name for the model class in AD4M
 */
export function Model(opts: ModelConfig) {
  return function (target: any) {
    target.prototype.className = opts.name;
    target.className = opts.name;

    // Generate SHACL shape (W3C standard + AD4M action definitions)
    target.generateSHACL = function () {
      const subjectName = opts.name;
      const obj = target.prototype;

      // Determine namespace from first property or collection, or use default
      let namespace = "ad4m://";
      const properties = obj.__properties || {};
      const collections = obj.__collections || {};

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

      // === Extract Constructor Actions (same logic as generateSDNA) ===
      let constructorActions = [];
      if (obj.subjectConstructor && obj.subjectConstructor.length) {
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
        if (!propMeta.collection) {
          propShape.maxCount = 1;
        }

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
        if (propMeta.setter) {
          // Custom setter defined - not yet supported in SHACL
          console.warn(
            `[SHACL Generation] Custom Prolog setter for property '${propName}' in class '${subjectName}' is not yet supported. ` +
              `The property will be created without setter actions. Consider using standard writable properties or provide explicit SHACL JSON.`,
          );
          // TODO: Parse custom Prolog setter to extract actions
        } else if (propMeta.writable && propMeta.through) {
          let setter = obj[propertyNameToSetterName(propName)];
          if (typeof setter === "function") {
            propShape.setter = [
              {
                action: "setSingleTarget",
                source: "this",
                predicate: propMeta.through,
                target: "value",
                ...(propMeta.local && { local: true }),
              },
            ];
          }
        }

        // Add to constructor actions if property has initial value
        if (propMeta.initial) {
          constructorActions.push({
            action: "addLink",
            source: "this",
            predicate: propMeta.through,
            target: propMeta.initial,
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
          collShape.nodeKind = "IRI";
        } else {
          // Default to IRI for collections (most common case)
          // Literal collections are rare and would need explicit marking
          collShape.nodeKind = "IRI";
        }

        // AD4M-specific metadata
        if (collMeta.local !== undefined) {
          collShape.local = collMeta.local;
        }

        if (collMeta.writable !== undefined) {
          collShape.writable = collMeta.writable;
        }

        // Relationship metadata
        if (collMeta.maxCount !== undefined) {
          collShape.maxCount = collMeta.maxCount;
        }

        if (collMeta.direction === "reverse") {
          collShape.inversePath = true;
        }

        // === Extract Collection Actions (adder/remover) ===
        // Adder action - adds a link to the collection
        collShape.adder = [
          {
            action: "addLink",
            source: "this",
            predicate: collMeta.through,
            target: "value",
            ...(collMeta.local && { local: true }),
          },
        ];

        // Remover action - removes a link from the collection
        collShape.remover = [
          {
            action: "removeLink",
            source: "this",
            predicate: collMeta.through,
            target: "value",
            ...(collMeta.local && { local: true }),
          },
        ];

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
        name: subjectName,
      };
    };

    Object.defineProperty(target, "type", { configurable: true });
  };
}
