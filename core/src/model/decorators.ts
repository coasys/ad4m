import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import {
  capitalize,
  propertyNameToSetterName,
  stringifyObjectLiteral,
} from "./util";
import { SHACLShape, SHACLPropertyShape } from "../shacl/SHACLShape";

// Module-level WeakMaps keyed on the constructor function (not the prototype).
// Each class constructor is a unique key, so subclass decorators write into their
// own entry rather than the parent prototype's — preventing silent metadata corruption
// across inherited classes (e.g. BaseBlock and PollBlock stay independent).
export const propertyRegistry = new WeakMap<Function, Record<string, any>>();
export const relationRegistry = new WeakMap<Function, Record<string, any>>();

/** Returns the own + inherited property metadata for a given constructor, with own values winning. */
export function getPropertiesMetadata(ctor: Function): Record<string, any> {
  if (!ctor) return {};
  const own = propertyRegistry.get(ctor) ?? {};
  const parent = Object.getPrototypeOf(ctor);
  if (!parent || parent === Function.prototype) return own;
  return { ...getPropertiesMetadata(parent), ...own };
}

/** Returns the own + inherited relation metadata for a given constructor, with own values winning. */
export function getRelationsMetadata(ctor: Function): Record<string, any> {
  if (!ctor) return {};
  const own = relationRegistry.get(ctor) ?? {};
  const parent = Object.getPrototypeOf(ctor);
  if (!parent || parent === Function.prototype) return own;
  return { ...getRelationsMetadata(parent), ...own };
}

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

export interface PropertyOptions {
  /**
   * The predicate of the property. All properties must have this option.
   */
  through?: string;

  /**
   * The initial value written by the SHACL constructor action.
   *
   * For non-readOnly properties this is **optional** — Ad4mModel
   * automatically derives a placeholder and overwrites it with the real
   * instance field value when `save()` is called. Only set this explicitly
   * when you need a specific non-literal default URI (e.g. a custom
   * `resolveLanguage` address) or want a sentinel value if the property
   * is never set.
   */
  initial?: string;

  /**
   * Indicates whether the property is required. If true, an initial value must be provided.
   */
  required?: boolean;

  /**
   * Marks the property as read-only — no setter action will be generated and
   * the property cannot be updated via the model layer. Defaults to false
   * (writable) when `through` is set.
   */
  readOnly?: boolean;

  /**
   * The language used to resolve the stored expression into a JS value.
   *
   * Omitting this (the common case) is equivalent to `"literal"` — scalar values
   * (string / number / boolean) are encoded as `literal://` URIs automatically.
   * Only specify this when you need a non-literal language (e.g. a custom IPFS
   * language address) or want to be explicit for documentation purposes.
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

  /**
   * Indicates that this property is a @Flag — a fixed predicate/value pair written
   * once by the createSubject constructor action and never changed. Flag properties
   * are immutable and will be skipped during updates.
   */
  flag?: boolean;
}

/**
 * Declares a typed property backed by a single link triple in the perspective.
 *
 * @param opts.through        - Predicate URI (required)
 * @param opts.initial        - Default value written by the constructor action (auto-derived for non-readOnly properties; only needed for custom URIs)
 * @param opts.required       - Adds `sh:minCount 1` to the SHACL shape
 * @param opts.readOnly       - Skips setter generation; property cannot be updated after creation (default: false)
 * @param opts.resolveLanguage - Language for value resolution (`"literal"`, etc.)
 * @param opts.local          - Store only in local perspective, not shared with the network
 * @param opts.getter         - Custom SurrealQL expression for computed / read-only properties
 * @param opts.transform      - Post-fetch transform applied to the raw value
 */
export function Property(opts: PropertyOptions) {
  return function <T>(target: T, key: keyof T) {
    if (!opts.through) {
      throw new Error("@Property requires a 'through' option");
    }

    const _propertyExisting =
      propertyRegistry.get((target as any).constructor) ?? {};
    const _propertyExistingKey = _propertyExisting[key as string] ?? {};
    propertyRegistry.set((target as any).constructor, {
      ..._propertyExisting,
      [key as string]: { ..._propertyExistingKey, ...opts },
    });

    if (!opts.readOnly) {
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
 * Immutable type-marker property: written once at creation and never modified.
 *
 * Use for type-discrimination predicates (e.g. `ad4m://type = "ad4m://message"`).
 * For mutable data prefer `@Property`.
 *
 * @example
 * ```typescript
 * @Flag({ through: "ad4m://type", value: "ad4m://message" })
 * type: string = "";
 * ```
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

    const _flagExisting =
      propertyRegistry.get((target as any).constructor) ?? {};
    propertyRegistry.set((target as any).constructor, {
      ..._flagExisting,
      [key as string]: {
        ...(_flagExisting[key as string] ?? {}),
        through: opts.through,
        required: true,
        initial: opts.value,
        flag: true,
        readOnly: true, // Flags are always immutable after creation
      },
    });

    // @ts-ignore
    target[key] = opts.value;

    Object.defineProperty(target, key, { configurable: true, writable: true });
  };
}

export interface RelationOptions {
  /**
   * The predicate of the property. All properties must have this option.
   */
  through: string;

  /**
   * Custom SurrealQL getter to resolve the related values. Use this for custom graph traversals.
   * The expression can reference 'Base' which will be replaced with the instance's base expression.
   * Example: "(<-link[WHERE predicate = 'flux://has_reply'].in.uri)"
   */
  getter?: string;

  /**
   * Indicates whether the property is stored locally in the perspective and not in the network. Useful for properties that are not meant to be shared with the network.
   */
  local?: boolean;
}

/** Minimal structural type for Ad4mModel instances — used in mutator signatures to avoid circular imports. */
export interface Ad4mModelLike {
  readonly id: string;
}

/**
 * Utility type that generates the runtime methods produced by \@HasMany / \@HasOne decorators.
 *
 * For each relation property `foo`, the decorator generates:
 * - `addFoo(value)` — Add a value (string ID or model instance)
 * - `removeFoo(value)` — Remove a value
 * - `setFoo(values)` — Replace all values
 *
 * Pass a string union of your \@HasMany/\@HasOne property names and use interface merging:
 * @example
 * ```typescript
 * \@Model({ name: 'Post' })
 * export class Post extends Ad4mModel {
 *   \@HasMany(() => Comment, { through: 'post://comment' })
 *   comments: Comment[] = [];
 * }
 * export interface Post extends HasManyMethods<'comments'> {}
 * ```
 */
export type HasManyMethods<Keys extends string> = {
  [K in Keys as `add${Capitalize<K>}`]: (
    value: string | Ad4mModelLike,
  ) => Promise<void>;
} & {
  [K in Keys as `remove${Capitalize<K>}`]: (
    value: string | Ad4mModelLike,
  ) => Promise<void>;
} & {
  [K in Keys as `set${Capitalize<K>}`]: (
    values: (string | Ad4mModelLike)[],
  ) => Promise<void>;
};

export function HasMany(
  relatedModelOrOpts: (() => any) | RelationOptions,
  opts?: RelationOptions,
) {
  const resolvedOpts: RelationOptions =
    typeof relatedModelOrOpts === "function" ? opts! : relatedModelOrOpts;
  const relatedModel: (() => any) | undefined =
    typeof relatedModelOrOpts === "function" ? relatedModelOrOpts : undefined;
  return function <T>(target: T, key: keyof T) {
    const _hasManyExisting =
      relationRegistry.get((target as any).constructor) ?? {};
    relationRegistry.set((target as any).constructor, {
      ..._hasManyExisting,
      [key as string]: {
        ...resolvedOpts,
        direction: "forward" as const,
        ...(relatedModel ? { relatedModel } : {}),
      },
    });

    const value = key as string;
    target[`add${capitalize(value)}`] = () => {};
    target[`remove${capitalize(value)}`] = () => {};
    target[`set${capitalize(value)}`] = () => {};

    Object.defineProperty(target, key, { configurable: true, writable: true });
  };
}

export function HasOne(
  relatedModelOrOpts: (() => any) | RelationOptions,
  opts?: RelationOptions,
) {
  const resolvedOpts: RelationOptions =
    typeof relatedModelOrOpts === "function" ? opts! : relatedModelOrOpts;
  const relatedModel: (() => any) | undefined =
    typeof relatedModelOrOpts === "function" ? relatedModelOrOpts : undefined;
  return function <T>(target: T, key: keyof T) {
    const _hasOneExisting =
      relationRegistry.get((target as any).constructor) ?? {};
    relationRegistry.set((target as any).constructor, {
      ..._hasOneExisting,
      [key as string]: {
        ...resolvedOpts,
        direction: "forward" as const,
        maxCount: 1,
        ...(relatedModel ? { relatedModel } : {}),
      },
    });

    const value = key as string;
    target[`add${capitalize(value)}`] = () => {};
    target[`remove${capitalize(value)}`] = () => {};
    target[`set${capitalize(value)}`] = () => {};

    Object.defineProperty(target, key, { configurable: true, writable: true });
  };
}

export function BelongsToOne(relatedModel: () => any, opts: RelationOptions) {
  return function <T>(target: T, key: keyof T) {
    const _b2oExisting =
      relationRegistry.get((target as any).constructor) ?? {};
    relationRegistry.set((target as any).constructor, {
      ..._b2oExisting,
      [key as string]: {
        ...opts,
        direction: "reverse" as const,
        maxCount: 1,
        relatedModel,
      },
    });

    Object.defineProperty(target, key, { configurable: true, writable: true });
  };
}

export function BelongsToMany(relatedModel: () => any, opts: RelationOptions) {
  return function <T>(target: T, key: keyof T) {
    const _b2mExisting =
      relationRegistry.get((target as any).constructor) ?? {};
    relationRegistry.set((target as any).constructor, {
      ..._b2mExisting,
      [key as string]: { ...opts, direction: "reverse" as const, relatedModel },
    });

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
 * Registers the class as an AD4M SDNA subject, enabling `Ad4mModel` static query methods.
 *
 * Must be applied to every class that extends `Ad4mModel`.
 *
 * @example
 * ```typescript
 * @Model({ name: "Recipe" })
 * class Recipe extends Ad4mModel { ... }
 * ```
 */
export function Model(opts: ModelConfig) {
  return function (target: any) {
    target.prototype.className = opts.name;
    target.className = opts.name;

    // Generate SHACL shape (W3C standard + AD4M action definitions)
    target.generateSHACL = function () {
      const subjectName = opts.name;
      const obj = target.prototype;

      // Determine namespace from first property or relation, or use default
      let namespace = "ad4m://";
      const fields = getPropertiesMetadata(target);
      const relations = getRelationsMetadata(target);

      // Try fields first
      if (Object.keys(fields).length > 0) {
        const firstProp = fields[Object.keys(fields)[0]];
        if (firstProp.through) {
          const match = firstProp.through.match(/^([^:]+:\/\/)/);
          if (match) {
            namespace = match[1];
          }
        }
      }
      // Fall back to relations if no fields
      else if (Object.keys(relations).length > 0) {
        const firstRelation = relations[Object.keys(relations)[0]];
        if (firstRelation.through) {
          const match = firstRelation.through.match(/^([^:]+:\/\/)/);
          if (match) {
            namespace = match[1];
          }
        }
      }

      // Create SHACL shape
      const shapeUri = `${namespace}${subjectName}Shape`;
      const targetClass = `${namespace}${subjectName}`;
      const shape = new SHACLShape(shapeUri, targetClass);

      // ── Detect @Model parent for sh:node inheritance ──────────────────────
      // If the immediate prototype constructor is also @Model-decorated,
      // emit sh:node <ParentShape> and use only OWN properties/relations
      // rather than duplicating the parent's properties in the child shape.
      const parentCtor = Object.getPrototypeOf(target);
      const parentClassName: string | undefined =
        parentCtor?.prototype?.className;
      const isParentModel =
        parentClassName &&
        parentClassName !== "Ad4mModel" &&
        (propertyRegistry.has(parentCtor) || relationRegistry.has(parentCtor));

      let shapeFields: Record<string, any>;
      let shapeRelations: Record<string, any>;
      if (isParentModel) {
        // Own-only fields/relations — parent's are covered by sh:node
        shapeFields = propertyRegistry.get(target) ?? {};
        shapeRelations = relationRegistry.get(target) ?? {};

        // Derive parent shape URI from parent's own properties' namespace
        const parentOwnFields = Object.values(
          propertyRegistry.get(parentCtor) ?? {},
        ) as any[];
        const parentOwnRelations = Object.values(
          relationRegistry.get(parentCtor) ?? {},
        ) as any[];
        let parentNamespace = "ad4m://";
        if (parentOwnFields.length > 0 && parentOwnFields[0].through) {
          const m = (parentOwnFields[0].through as string).match(
            /^([^:]+:\/\/)/,
          );
          if (m) parentNamespace = m[1];
        } else if (
          parentOwnRelations.length > 0 &&
          parentOwnRelations[0].through
        ) {
          const m = (parentOwnRelations[0].through as string).match(
            /^([^:]+:\/\/)/,
          );
          if (m) parentNamespace = m[1];
        }
        const parentShapeUri = `${parentNamespace}${parentClassName}Shape`;
        shape.addParentShape(parentShapeUri);
      } else {
        // No @Model parent — include all inherited properties directly
        shapeFields = fields;
        shapeRelations = relations;
      }
      // ──────────────────────────────────────────────────────────────────────

      // === Extract Constructor Actions (same logic as generateSDNA) ===
      let constructorActions = [];
      if (obj.subjectConstructor && obj.subjectConstructor.length) {
        constructorActions = constructorActions.concat(obj.subjectConstructor);
      }

      // === Extract Destructor Actions ===
      let destructorActions = [];

      // Convert fields to SHACL property shapes
      for (const propName in shapeFields) {
        const propMeta = shapeFields[propName];

        if (!propMeta.through) continue; // Skip properties without predicates

        const propShape: SHACLPropertyShape = {
          name: propName, // Property name for generating named URIs
          path: propMeta.through,
        };

        // Auto-derive a constructor placeholder for non-readOnly, non-flag properties
        // that don't have an explicit initial. Only do this when the prototype
        // has a defined default value — optional fields (title?: string) have
        // no prototype default (undefined), and we must NOT write an empty
        // literal link for them. The specific placeholder value doesn't matter
        // for fields with an actual default — createSubject's initialValues
        // mechanism overwrites it with the real value via the setter actions.
        const protoDefault = obj[propName];
        const effectiveInitial: string | undefined =
          propMeta.initial ??
          (!propMeta.readOnly && !propMeta.flag && protoDefault !== undefined
            ? "literal://string:"
            : undefined);

        // Determine datatype from the TypeScript default value type.
        // resolveLanguage: "literal" is now the implicit default — literal://
        // URIs can carry any type (string/number/boolean), so always infer
        // from the prototype's default value rather than forcing xsd://string.
        const initialType = typeof obj[propName];
        if (initialType === "number") {
          propShape.datatype = "xsd://integer";
        } else if (initialType === "boolean") {
          propShape.datatype = "xsd://boolean";
        } else if (initialType === "string") {
          propShape.datatype = "xsd://string";
        }

        // Cardinality constraints
        if (propMeta.required) {
          propShape.minCount = 1;
        }

        // @Property fields are always single-valued; maxCount 1 is unconditional.
        // Multi-valued relations live in relationRegistry and are handled below.
        propShape.maxCount = 1;

        // Flag properties have fixed value
        if (propMeta.flag && propMeta.initial) {
          propShape.hasValue = propMeta.initial;
        }

        // AD4M-specific metadata
        if (propMeta.local !== undefined) {
          propShape.local = propMeta.local;
        }

        if (propMeta.readOnly) {
          propShape.readOnly = true;
        }

        if (propMeta.resolveLanguage) {
          propShape.resolveLanguage = propMeta.resolveLanguage;
        } else if (!propMeta.flag) {
          // resolveLanguage: "literal" is the implicit default for scalar properties.
          // The Rust executor requires this to be explicit in the SHACL shape to
          // configure fn::parse_literal correctly in SurrealDB. The user doesn't
          // need to write it in their @Property decorator, but the shape must have it.
          propShape.resolveLanguage = "literal";
        }

        // === Extract Setter Actions (same logic as generateSDNA) ===
        if (propMeta.setter) {
          // Custom setter defined - not yet supported in SHACL
          console.warn(
            `[SHACL Generation] Custom Prolog setter for property '${propName}' in class '${subjectName}' is not yet supported. ` +
              `The property will be created without setter actions. Consider using standard writable properties or provide explicit SHACL JSON.`,
          );
          // TODO: Parse custom Prolog setter to extract actions
        } else if (!propMeta.readOnly && propMeta.through) {
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

        // Add to constructor actions (always for non-readOnly, non-flag, using the
        // effective placeholder — createSubject's initialValues will override
        // the target with the real instance value via the setter actions).
        if (effectiveInitial) {
          constructorActions.push({
            action: "addLink",
            source: "this",
            predicate: propMeta.through,
            target: effectiveInitial,
          });
        }

        // Always add destructor action for non-readOnly, non-flag properties so
        // delete() cleans them up regardless of whether initial was explicit.
        if (!propMeta.readOnly && !propMeta.flag) {
          destructorActions.push({
            action: "removeLink",
            source: "this",
            predicate: propMeta.through,
            target: "*",
          });
        }

        shape.addProperty(propShape);
      }

      // Convert relations to SHACL property shapes
      for (const relName in shapeRelations) {
        const relMeta = shapeRelations[relName];

        if (!relMeta.through) continue;

        const relShape: SHACLPropertyShape = {
          name: relName, // Relation name for generating named URIs
          path: relMeta.through,
          // Relations have no maxCount (unlimited)
          // minCount defaults to 0 (optional)
        };

        // Relations contain references (IRIs) to other entities
        relShape.nodeKind = "IRI";

        // AD4M-specific metadata
        if (relMeta.local !== undefined) {
          relShape.local = relMeta.local;
        }

        // Relationship metadata
        if (relMeta.maxCount !== undefined) {
          relShape.maxCount = relMeta.maxCount;
        }

        if (relMeta.direction === "reverse") {
          relShape.inversePath = true;
        }

        // === Extract Relation Actions (adder/remover) ===
        // Adder action - adds a link to the relation
        relShape.adder = [
          {
            action: "addLink",
            source: "this",
            predicate: relMeta.through,
            target: "value",
            ...(relMeta.local && { local: true }),
          },
        ];

        // Remover action - removes a link from the relation
        relShape.remover = [
          {
            action: "removeLink",
            source: "this",
            predicate: relMeta.through,
            target: "value",
            ...(relMeta.local && { local: true }),
          },
        ];

        shape.addProperty(relShape);
      }

      // Set constructor and destructor actions on the shape.
      // Always set constructor actions (even if empty) so the Rust SHACL parser
      // emits an ad4m://constructor link — without it get_constructor_actions()
      // throws "No SHACL constructor found" for classes with no @Flag or initial
      // properties. An empty constructor is valid: it means "do nothing on creation".
      shape.setConstructorActions(constructorActions);
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
