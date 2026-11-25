import { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
import { CallOptions } from "../apiClient";
import { makeRandomId } from "./util";
import { getPropertiesMetadata, getRelationsMetadata, setPropertyRegistryEntry, setRelationRegistryEntry, Model } from "./decorators";
import type { PropertyOptions, PropertyMetadataEntry, RelationMetadataEntry } from "./decorators";
import { requireSingleParent, requireTotalCount, resolveParentPredicate, resolveParentRelation } from "./query-common";
import { isArrayType, determinePredicate, determineNamespace, buildModelFromJSONSchema } from "./json-schema";
import type { SHACLShape } from "../shacl/SHACLShape";
import type { RegisterResult } from "../generated/api/RegisterResult";
import type { MutateOp, ParentRef } from "../generated/api";
import type { JSONSchemaProperty, JSONSchema, JSONSchemaToModelOptions } from "./json-schema";

import { LiveQuery } from "../perspectives/LiveQuery";
import type { LiveOptions } from "../perspectives/LiveQuery";
import {
  normalizeValue,
  sameValue,
} from "./hydration";
import type {
  Scope, IncludeMap, Query,
  GetOptions, Page, ResultsWithTotalCount,
  PaginationResult, CursorPage, PropertyMetadata, RelationMetadata, ModelMetadata,
  IncludeProjection,
  TypedQuery, IncludeExtras, IncludeOf, LinksMap,
} from "./types";
import { isContainedScope, isContainmentTraversal, isTraverseScope } from "./types";



// ---------------------------------------------------------------------------
// Helpers for Rust-side include resolution
// ---------------------------------------------------------------------------

/**
 * Construct a model class instance from a plain JSON object returned by the
 * Rust endpoint.  Recursively converts included relation values (which come
 * back as plain JSON objects) into proper model class instances.
 */
/**
 * Key under which the executor returns a polymorphically-hydrated instance's
 * concrete class.
 *
 * Half of a wire contract: the writer is `SUBJECT_CLASS_KEY` in
 * `rust-executor/src/perspectives/model_query/relations.rs`, and the two are
 * separate literals in separate languages. Renaming one alone does not fail to
 * compile — it degrades to every instance staying plain JSON, which looks
 * identical to a relation that declared no classes to build.
 * `tests/js/tests/model/model-polymorphic.test.ts` drives both ends against one
 * executor, so the drift fails a test rather than a user's query.
 *
 * The value is the class's target class IRI (two classes may share a name):
 * {@link classKey} of the class it was read as. The executor also returns
 * `__subjectClasses`, the whole set this one names the head of, by target
 * class IRI. Nothing here reads it — it rides onto the instance with every
 * other JSON key, for a caller that wants to know a choice was made.
 */
const SUBJECT_CLASS_KEY = '__subjectClass';

/**
 * The IRI the executor names `cls` by (`queriedAs`: its target class), or
 * `undefined` for something that is not a declared model class.
 */
function classKey(cls: unknown): string | undefined {
  try {
    const key = (cls as any)?.queriedAs?.();
    return typeof key === 'string' ? key : undefined;
  } catch {
    return undefined;
  }
}

/**
 * The namespace of a target class IRI: everything up to and including its
 * last `#`, `:` or `/`, the split the executor's `class_name_of` makes
 * (`urn:isbn:Book` → `urn:isbn:`, `a://b/c/Thing` → `a://b/c/`). `undefined`
 * when there is no local name after it.
 */
function namespaceOf(targetClass: string | undefined): string | undefined {
  if (!targetClass) return undefined;
  const split = Math.max(targetClass.lastIndexOf('#'), targetClass.lastIndexOf(':'), targetClass.lastIndexOf('/'));
  if (split < 0 || split === targetClass.length - 1) return undefined;
  return targetClass.slice(0, split + 1);
}

/**
 * Turn on `polymorphic` for every relation that declares it, at every depth of
 * an include map.
 *
 * A relation being heterogeneous is a fact about the data, not about one query,
 * so the declaration lives on the model and the call site writes
 * `include: { children: true }`. That has to hold just as much one level down:
 * in `include: { posts: { include: { children: true } } }`, `children` is a
 * relation on `Post`, so the walk carries the class it is reading against down
 * with it. Without that, a nested include of an untyped polymorphic relation
 * reaches the executor with no shape to resolve and fails the query outright.
 *
 * An explicit `polymorphic: false` at any level still wins — `undefined` is the
 * only state the default fills in.
 *
 * The walk stops descending *through* a polymorphic relation, because there is
 * nothing to descend into: its targets are of several classes by definition, so
 * no single class's metadata could say what a deeper include means. The
 * executor applies each concrete class's declared default from its shape
 * (`ad4m://polymorphic`) to those nested includes instead.
 */
function applyPolymorphicIncludeDefaults(includes: IncludeMap, ctor: Function): void {
  const relMeta = getRelationsMetadata(ctor);
  for (const [relName, val] of Object.entries(includes)) {
    // `$`-prefixed keys are projections, not relations.
    if (relName.startsWith('$')) continue;
    const meta = relMeta[relName];
    if (!meta) continue;

    let subQuery = val as any;
    if (meta.polymorphic) {
      if (val === true) {
        subQuery = { polymorphic: true };
        includes[relName] = subQuery;
      } else if (typeof val === 'object' && val !== null && subQuery.polymorphic === undefined) {
        subQuery.polymorphic = true;
      }
      // The classes this call site can build are also the classes it wants its
      // targets read as, so the declaration answers both questions and the call
      // site repeats neither. Sent as part of the query rather than applied to
      // the results: a preference in the request is reproducible by anyone who
      // sends it, where one applied afterwards would make the same data read
      // differently depending on which classes the caller happened to import.
      //
      // It ranks and does not narrow — a target matching none of them still
      // arrives — so declaring what you can construct never costs you a member
      // of a relation that is heterogeneous by definition.
      if (
        typeof subQuery === 'object' &&
        subQuery !== null &&
        subQuery.polymorphic &&
        subQuery.preferClasses === undefined &&
        meta.instantiateAs
      ) {
        const classes = (meta.instantiateAs() ?? [])
          .map(classKey)
          .filter((key): key is string => typeof key === 'string');
        if (classes.length) subQuery.preferClasses = classes;
      }
    }

    if (typeof subQuery !== 'object' || subQuery === null) continue;
    const nested = subQuery.include as IncludeMap | undefined;
    if (!nested || subQuery.polymorphic) continue;

    // The thunk is only evaluated here, at query time, so a self-referential or
    // circularly-imported target that is not yet defined costs the nested
    // default rather than the whole query.
    let TargetClass: any;
    try {
      TargetClass = meta.target?.();
    } catch (e) {
      // A class not yet initialised throws `ReferenceError` from the temporal
      // dead zone, which is the expected cost of an import cycle and not worth
      // saying anything about. Anything else — a thunk closing over nothing,
      // typically — is a broken declaration that would otherwise cost the nested
      // default in silence, so it gets said out loud.
      const circularImport = e instanceof ReferenceError;
      const say = circularImport ? console.debug : console.warn;
      say(`prepareModelQueryParams: target class unavailable for include '${relName}':`, e);
      continue;
    }
    if (TargetClass) applyPolymorphicIncludeDefaults(nested, TargetClass);
  }
}

/**
 * Split an include map into its relations and its `$`-prefixed projections.
 *
 * The two travel in different fields: a relation is hydrated, a projection is
 * computed over one. Both halves are new objects, so the caller's query is left
 * as it was written and can be sent again.
 */
function splitIncludeProjections(include: IncludeMap): {
  includes: IncludeMap;
  projections: Record<string, IncludeProjection>;
} {
  const includes: IncludeMap = {};
  const projections: Record<string, IncludeProjection> = {};
  for (const [key, val] of Object.entries(include)) {
    if (key.startsWith('$')) {
      projections[key] = { ...(val as IncludeProjection) };
    } else {
      includes[key] = val;
    }
  }
  return { includes, projections };
}

/**
 * Tag each projection with its target class IRI, so the executor can resolve
 * the target shape through its in-memory cache when applying a projection's
 * where-clause. `ctor` is the class the projections are read against; without
 * one (the members of a polymorphic relation) nothing is tagged, and the
 * executor resolves each projection against each member's own class.
 */
function tagProjectionTargets(projections: Record<string, IncludeProjection>, ctor: Function | undefined): void {
  if (!ctor) return;
  const allRelMeta = getRelationsMetadata(ctor);
  for (const proj of Object.values(projections)) {
    const relMeta = allRelMeta[proj.from];
    if (!proj.targetClassName && relMeta?.target) {
      try {
        const TargetClass = relMeta.target() as any;
        if (typeof TargetClass?.queriedAs === 'function') {
          proj.targetClassName = TargetClass.queriedAs();
        }
      } catch (e) { console.debug(`prepareModelQueryParams: target class unavailable for projection:`, e); }
    }
  }
}

/**
 * Move the `$`-prefixed keys of every nested include into that sub-query's own
 * `projections`, at every depth.
 *
 * The executor reads a sub-query exactly as it reads a top-level query, so a
 * projection on related records belongs in the sub-query's `projections` field,
 * as it does at the top. Left inside `include` it named a relation that does not
 * exist and was skipped: the query succeeded and the field was simply absent, so
 * `include: { posts: { include: { $likeCount: … } } }` came back without counts
 * and said nothing.
 *
 * Copies every sub-query it changes rather than editing it, so a query object
 * the caller keeps (a subscription re-preparing its query, say) still says what
 * it said.
 */
function liftNestedProjections(includes: IncludeMap, ctor: Function | undefined): IncludeMap {
  const relMeta = ctor ? getRelationsMetadata(ctor) : {};
  const out: IncludeMap = {};
  for (const [relName, val] of Object.entries(includes)) {
    if (typeof val !== 'object' || val === null || relName.startsWith('$')) {
      out[relName] = val;
      continue;
    }
    const subQuery: any = { ...val };
    // The class the nested keys are read against. A polymorphic relation's
    // members are of several classes, so there is none to name: its nested
    // projections go untagged and the executor resolves them per member class.
    let TargetClass: Function | undefined;
    if (!subQuery.polymorphic) {
      try {
        TargetClass = relMeta[relName]?.target?.();
      } catch (e) {
        console.debug(`prepareModelQueryParams: target class unavailable for include '${relName}':`, e);
      }
    }
    if (subQuery.include && typeof subQuery.include === 'object') {
      const { includes: nestedIncludes, projections } = splitIncludeProjections(subQuery.include);
      if (Object.keys(nestedIncludes).length > 0) {
        subQuery.include = liftNestedProjections(nestedIncludes, TargetClass);
      } else {
        delete subQuery.include;
      }
      if (Object.keys(projections).length > 0) {
        tagProjectionTargets(projections, TargetClass);
        subQuery.projections = { ...(subQuery.projections ?? {}), ...projections };
      }
    }
    out[relName] = subQuery;
  }
  return out;
}

/** Snapshot a hydrated instance for dirty tracking, so `save()` on it (a query
 *  row, a union row, a subscription push, or a record an `include` loaded)
 *  takes the update path. Only the included relations (not `$` projections)
 *  are recorded. Rows of a query by class name are base `Ad4mModel`s:
 *  nothing to track. */
function snapshotHydrated(instance: any, include?: IncludeMap): void {
  if (!(instance instanceof Ad4mModel) || instance.constructor === Ad4mModel) return;
  const relations = include && Object.fromEntries(Object.entries(include).filter(([k]) => !k.startsWith('$')));
  const recorded = relations && Object.keys(relations).length > 0 ? relations : undefined;
  (instance as any).takeSnapshot(recorded);
}

function jsonToModelInstance<T extends Ad4mModel>(
  ModelClass: typeof Ad4mModel & (new (...args: any[]) => T),
  perspective: PerspectiveProxy,
  json: any,
  include?: IncludeMap,
  properties?: string[],
): T {
  const instance = new ModelClass(perspective, json.id || json.baseExpression) as any;

  // When a properties projection is active, remove own properties set by the
  // constructor that are not in the projected JSON.  This ensures assertions
  // like `expect(r).to.not.have.own.property("body")` pass.
  if (properties) {
    const jsonKeys = new Set(Object.keys(json));
    for (const key of Object.getOwnPropertyNames(instance)) {
      // Keep internal fields (backing store for getters), id, baseExpression, perspective
      if (key === 'id' || key === 'baseExpression' || key === 'perspective' || key.startsWith('_')) continue;
      if (!jsonKeys.has(key)) {
        delete instance[key];
      }
    }
  }

  for (const [key, value] of Object.entries(json)) {
    if (key === 'baseExpression' || key === 'id') continue;

    // Skip getter-only properties anywhere in the prototype chain
    let isReadonly = false;
    let proto = Object.getPrototypeOf(instance);
    while (proto) {
      const desc = Object.getOwnPropertyDescriptor(proto, key);
      if (desc) { isReadonly = !!(desc.get && !desc.set); break; }
      proto = Object.getPrototypeOf(proto);
    }
    if (isReadonly) continue;

    // Parse ISO timestamps to epoch milliseconds (matches hydrateFromLinks behaviour)
    if ((key === 'createdAt' || key === 'updatedAt') && typeof value === 'string' && value.includes('T')) {
      instance[key] = new Date(value).getTime();
      continue;
    }

    instance[key] = value;
  }


  // Recursively convert included relation values to class instances
  if (include) {
    const relMeta = getRelationsMetadata(ModelClass as any);
    for (const [relName, includeVal] of Object.entries(include)) {
      if (!includeVal) continue;
      const meta = relMeta[relName];
      // A polymorphic relation may legitimately declare no target at all —
      // that is the case it exists for — so it must not be gated on one.
      if (!meta) continue;
      if (!meta.target && !meta.polymorphic) continue;
      const TargetClass = meta.target?.() as any;
      // For a polymorphic relation the executor sends each child's concrete
      // class back on the instance, because it had to know it in order to
      // hydrate against the right shape at all. What it cannot send is the
      // constructor, so `instantiateAs` names the classes this side can build
      // and the target class → class map is derived from them: each class
      // knows its own target class IRI, which the executor names it by (two
      // classes may share a name).
      //
      // Built on first use rather than at decoration time, because the thunk
      // exists to be called late: a class defined further round a circular
      // import graph is not there yet when the decorator runs.
      let byTargetClass: Map<string, any> | undefined;
      // May resolve to nothing: a polymorphic relation is allowed to declare no
      // target at all, and the list may not name the class that arrived — it
      // says what this call site can construct, not what the relation may hold.
      // The item then stays plain JSON — correct data, carrying its target
      // class for a caller that wants to dispatch on it — rather than being
      // forced through a constructor that does not exist.
      const resolveChildClass = (item: any): any => {
        if (!meta.polymorphic) return TargetClass;
        const concrete = item?.[SUBJECT_CLASS_KEY];
        // No concrete class means this was not read polymorphically after all —
        // an explicit `polymorphic: false` at the call site — so the relation's
        // own declared target is the right shape.
        if (typeof concrete !== 'string') return TargetClass;
        if (meta.instantiateAs) {
          if (!byTargetClass) {
            byTargetClass = new Map();
            for (const cls of meta.instantiateAs() ?? []) {
              const key = classKey(cls);
              if (key) byTargetClass.set(key, cls);
            }
          }
          const cls = byTargetClass.get(concrete);
          if (cls) return cls;
        }
        // The declared target is not a fallback, and having named no classes to
        // build does not make it one. Where a relation both declares a target
        // and reads polymorphically, a child of some third class was hydrated
        // against *its own* shape, so constructing the declared class over that
        // JSON produces an instance that answers `instanceof` for a class it is
        // not, carrying fields that class never declared — the exact
        // mislabelling polymorphic reads exist to prevent. It fits only when it
        // is the class the executor named.
        return TargetClass && classKey(TargetClass) === concrete ? TargetClass : undefined;
      };
      const nestedInclude =
        typeof includeVal === 'object' && includeVal !== null
          ? (includeVal as any).include
          : undefined;
      const nestedProperties =
        typeof includeVal === 'object' && includeVal !== null
          ? (includeVal as any).properties
          : undefined;

      const raw = instance[relName];
      if (Array.isArray(raw)) {
        instance[relName] = raw.map((item: any) => {
          const ChildClass = resolveChildClass(item);
          if (ChildClass && typeof item === 'object' && item !== null && item.id) {
            return jsonToModelInstance(ChildClass, perspective, item, nestedInclude, nestedProperties);
          }
          return item;
        });
      } else if (typeof raw === 'object' && raw !== null && raw.id) {
        const ChildClass = resolveChildClass(raw);
        if (ChildClass) {
          instance[relName] = jsonToModelInstance(
            ChildClass, perspective, raw, nestedInclude, nestedProperties,
          );
        }
      }
    }
  }

  // After its relations are built, so the snapshot holds what was loaded.
  snapshotHydrated(instance, include);
  return instance;
}

/**
 * The ops of each open {@link Ad4mModel.transaction}, by the id of the batch it
 * opened, with the undo of each write's provisional snapshot. A model write
 * that names such a batch is queued here and sent with the others as one
 * `perspective.mutate` call, so a later write of the transaction finds an
 * instance an earlier one created. When the call or the callback fails,
 * the undos run, so no instance looks saved that was not.
 */
const transactionOps = new Map<string, { perspective: PerspectiveProxy; ops: MutateOp[]; undo: (() => void)[]; done: (() => void)[] }>();

/**
 * Writes `ops` as one `perspective.mutate` call: into the open batch `batchId`
 * when given (the caller commits), else committed at once. In a transaction's
 * batch they are queued instead; the transaction sends them, and runs `undo`
 * if they are never written.
 */
async function writeOps(perspective: PerspectiveProxy, ops: MutateOp[], batchId?: string, claim?: WriteClaim): Promise<void> {
  const queued = batchId ? transactionOps.get(batchId) : undefined;
  if (queued) {
    if (queued.perspective.uuid !== perspective.uuid) {
      throw new Error(
        `The batch ${batchId} belongs to perspective ${queued.perspective.uuid}, not ${perspective.uuid}`,
      );
    }
    // A copy: the ops hold the caller's JSON values, which it may change before the flush.
    queued.ops.push(...ops.map((op) => JSON.parse(JSON.stringify(op)) as MutateOp));
    if (claim) {
      queued.undo.push(claim.undo);
      queued.done.push(claim.done);
    }
    return;
  }
  await perspective.mutate(ops, batchId ? { batch: batchId } : {});
  claim?.done();
}

/** The fields an update op writes. */
function opFields(op: MutateOp): string[] {
  const { set, add, remove } = op as { set?: object; add?: object; remove?: object };
  return [...new Set([...Object.keys(set ?? {}), ...Object.keys(add ?? {}), ...Object.keys(remove ?? {})])];
}

/** A write's hold on the fields it carries (see `claimFields`): `undo` when it was not written, `done` when it was. */
type WriteClaim = { undo: () => void; done: () => void };

/**
 * One write of one field: what the field held before it (`unsaved`: the instance
 * was not in the store then), and whether the write is in the store.
 */
type FieldClaim = { owner: object; had: boolean; before: unknown; unsaved: boolean; settled: boolean };

/**
 * The writes of each instance field, oldest first, back to the oldest one not
 * settled (see `claimFields`). A value cannot tell an undo what to restore: a
 * later write may store the same value again, or still fail itself.
 */
const fieldClaims = new WeakMap<object, Map<string, FieldClaim[]>>();

/**
 * The instances whose `save()` is `Model.create()`'s, with the create's parent: it creates under
 * that parent (see `persist`) and reads nothing back, whatever a `save(batchId)` override forwards.
 */
const creating = new WeakMap<object, { parent?: ParentRef }>();

/** The fields of `instance` with a write not settled yet: the store may not hold them, so a read does not own them. */
function unsettledFields(instance: object): string[] {
  const claims = fieldClaims.get(instance);
  if (!claims) return [];
  return [...claims].filter(([, held]) => held.some((c) => !c.settled)).map(([field]) => field);
}

/**
 * The `parent` of a create op for `create(…, { parent })`. Resolving it first
 * means a parent that names no single instance or no relation to `childCtor`
 * throws before anything is written. `{ id }` (containment, #1446) has the
 * executor use the parent's own `children` relation, else `ad4m://has_child`;
 * `{ model, id }` names the parent's relation to the child class, written
 * through its adder (with ordering) or, for a `@HasOne`, as the relation's one
 * member; `{ id, predicate }` is one plain link.
 */
function parentRef(scope: Scope, childCtor: Function): ParentRef {
  const parent = requireSingleParent(scope);
  if (isContainedScope(parent)) return { id: parent.id };
  if ('predicate' in parent) return { id: parent.id, predicate: parent.predicate };
  return {
    id: parent.id,
    relation: resolveParentRelation(parent, childCtor),
    model: parent.model.queriedAs(),
  };
}

/**
 * Base class for defining data models in AD4M.
 * 
 * @description
 * Ad4mModel provides the foundation for creating data models that are stored in AD4M perspectives.
 * Each model instance is represented as a subgraph in the perspective, with properties and relations
 * mapped to links in that graph. The class uses Prolog-based queries to efficiently search and filter
 * instances based on their properties and relationships.
 * 
 * Key concepts:
 * - Each model instance has a unique base expression that serves as its identifier
 * - Properties are stored as links with predicates defined by the `through` option
 * - Relations represent one-to-many relationships as sets of links
 * - Queries are translated to Prolog for efficient graph pattern matching
 * - Changes are tracked through the perspective's subscription system
 * 
 * @example
 * ```typescript
 * // Define a recipe model
 * @Model({ name: "Recipe" })
 * class Recipe extends Ad4mModel {
 *   // Plain property — no `resolveLanguage` → deterministic typed literal
 *   // storage (fast POS-index path). This is the perf default.
 *   @Property({ through: "recipe://name" })
 *   name: string = "";
 *
 *   // Property that needs per-value provenance (e.g. a signed message body):
 *   // `resolveLanguage: "literal"` routes values through expression_create on
 *   // the built-in literal language, producing a signed envelope URI
 *   // (author/timestamp/proof) instead of a bare typed literal.
 *   @Property({
 *     through: "recipe://signed_note",
 *     resolveLanguage: "literal"
 *   })
 *   signedNote: string = "";
 *
 *   // Property resolved through a custom language: values are routed through
 *   // expression_create on that language (signed expression URIs).
 *   @Optional({
 *     through: "recipe://photo",
 *     resolveLanguage: "QmFileStorageLanguageAddress..."
 *   })
 *   photo: string = "";
 *
 *   // Optional property with custom initial value
 *   @Optional({
 *     through: "recipe://status",
 *     initial: "recipe://draft"
 *   })
 *   status: string = "";
 * 
 *   // Read-only computed property
 *   @ReadOnly({
 *     through: "recipe://rating",
 *     getter: `
 *       findall(Rating, triple(Base, "recipe://user_rating", Rating), Ratings),
 *       sum_list(Ratings, Sum),
 *       length(Ratings, Count),
 *       Value is Sum / Count
 *     `
 *   })
 *   averageRating: number = 0;
 * 
 *   // Relation of ingredients
   *   @HasMany({ through: "recipe://ingredient" })
   *   ingredients: string[] = [];
 * 
 *   // Relation of comments linked to another model
 *   @HasMany(() => Comment, { through: "recipe://comment" })
 *   comments: Comment[] = [];
 * }
 * 
 * // Create and save a new recipe
 * const recipe = new Recipe(perspective);
 * recipe.name = "Chocolate Cake";
 * recipe.ingredients = ["flour", "sugar", "cocoa"];
 * await recipe.save();
 * 
 * // Query recipes in different ways
 * // Get all recipes
 * const allRecipes = await Recipe.findAll(perspective);
 * 
 * // Find recipes with specific criteria
 * const desserts = await Recipe.findAll(perspective, {
 *   where: { 
 *     status: "recipe://published",
 *     averageRating: { gt: 4 }
 *   },
 *   order: { name: "ASC" },
 *   limit: 10
 * });
 * 
 * // Sort and limit
 * const popularRecipes = await Recipe.findAll(perspective, {
 *   where: { averageRating: { gt: 4.5 } },
 *   order: { averageRating: "DESC" },
 *   limit: 5
 * });
 * 
 * // Follow real-time updates with a live query
 * const live = Recipe.query(perspective, { where: { status: "recipe://cooking" } });
 * live.subscribe(({ data }) => {
 *   if (data) console.log("Currently being cooked:", data.results);
 * });
 * 
 * // Paginate results
 * const { results, totalCount, pageNumber } = await Recipe.paginate(perspective, 10, 1, {
 *   where: { status: "recipe://published" }
 * });
 * ```
 */
export class Ad4mModel {
  private _baseExpression: string;
  private _perspective: PerspectiveProxy;
  private _snapshot: Record<string, any> | null = null;
  author: string;
  createdAt: any;
  updatedAt: any;
  /**
   * Per-link rows for the entries asked for with `Query.links`, keyed as they
   * were asked. Absent when the query did not ask for any.
   */
  declare __links?: LinksMap;

  /**
   * Generates the SHACL shape graph for this model class.
   * Injected at class-definition time by the `@Model` decorator.
   * Returns `{ shape: null, name: '' }` on un-decorated base classes —
   * the decorator's `parentSHACL?.shape?.nodeShapeUri` check handles this.
   */
  static generateSHACL(): { shape: any; name: string } {
    return { shape: null, name: '' };
  }

  /** The `@Model` name of this class: its human-facing name. Two classes
   *  may share one; the executor addresses each by {@link getTargetClass}. */
  static getClassName(): string {
    const className = (this as any).className;
    if (!className) throw new Error(`Class ${this.name} has no class name: declare it with @Model.`);
    return className;
  }

  /** The target class IRI of this class (`sh:targetClass` of its shape),
   *  which the executor addresses it by. */
  static getTargetClass(): string {
    const targetClass = this.generateSHACL().shape?.targetClass;
    if (!targetClass) throw new Error(`Class ${this.name} has no target class: declare it with @Model.`);
    return targetClass;
  }

  /**
   * Extracts metadata from decorators for query building.
   * 
   * @description
   * This method reads the metadata stored by decorators (@Property, @HasMany, etc.)
   * and returns it in a structured format that's easier to work with for query builders
   * and other systems that need to introspect model structure.
   * 
   * The metadata includes:
   * - Class name from @Model
   * - Property metadata (predicates, types, constraints, etc.)
   * - Relation metadata (predicates, filters, etc.)
   * 
   * For models created via `fromJSONSchema()`, this method will derive metadata from
   * the WeakMap registries that were populated during the dynamic class creation.
   * If these structures are empty but a JSON schema was attached to the class,
   * it can fall back to deriving metadata from that schema.
   * 
   * @returns Structured metadata object containing className, properties, and relations
   * @throws Error if the class doesn't have @Model decorator
   * 
   * @example
   * ```typescript
   * @Model({ name: "Recipe" })
   * class Recipe extends Ad4mModel {
   *   @Property({ through: "recipe://name" })
   *   name: string = "";
   *
   *   @HasMany({ through: "recipe://ingredient" })
   *   ingredients: string[] = [];
   * }
   * 
   * const metadata = Recipe.getModelMetadata();
   * console.log(metadata.className); // "Recipe"
   * console.log(metadata.properties.name.predicate); // "recipe://name"
   * console.log(metadata.relations.ingredients.predicate); // "recipe://ingredient"
   * ```
   */
  public static getModelMetadata(): ModelMetadata {
    // Access the prototype with any type to access decorator-added properties
    const prototype = this.prototype as any;
    
    // Validate that the class has @Model decorator
    // The decorator sets prototype.className, so we check for its existence
    if (!prototype.className || prototype.className === 'Ad4mModel') {
      throw new Error("Model class must be decorated with @Model");
    }
    
    // Extract className
    const className = prototype.className;
    
    // Extract properties from WeakMap registry
    const propertiesMetadata: Record<string, PropertyMetadata> = {};
    const prototypeProperties = getPropertiesMetadata(this);
    
    for (const [propertyName, opts] of Object.entries(prototypeProperties)) {
      const options = opts as PropertyOptions & { required?: boolean; flag?: boolean; writable?: boolean };
      propertiesMetadata[propertyName] = {
        name: propertyName,
        predicate: options.through || "",
        required: options.required || false,
        readOnly: !(options.writable ?? false),
        ...(options.initial !== undefined && { initial: options.initial }),
        ...(options.resolveLanguage !== undefined && { resolveLanguage: options.resolveLanguage }),
        ...(options.getter !== undefined && { getter: options.getter }),
        ...(options.local !== undefined && { local: options.local }),
        ...(options.flag !== undefined && { flag: options.flag }),
        ...(options.transform !== undefined && { transform: options.transform })
      };
    }
    
    // Extract relations (relations) from WeakMap registry
    const relationsMetadata: Record<string, RelationMetadata> = {};
    const allRelationsMeta = getRelationsMetadata(this as any);
    const prototypeRelations = allRelationsMeta;
    
    for (const [relationName, opts] of Object.entries(prototypeRelations)) {
      const options = opts as RelationMetadataEntry;
      relationsMetadata[relationName] = {
        name: relationName,
        predicate: options.predicate || "",
        ...(options.local !== undefined && { local: options.local }),
        ...(options.getter !== undefined && { getter: options.getter }),
        ...(options.readOnly && { readOnly: true }),
        direction: (options.kind === 'belongsToMany' || options.kind === 'belongsToOne') ? 'reverse' : 'forward',
        ...(options.kind !== undefined && { kind: options.kind }),
        ...(options.maxCount !== undefined && { maxCount: options.maxCount }),
        ...(options.target !== undefined && { target: options.target }),
        ...(options.filter !== undefined && { filter: options.filter }),
        ...(options.where !== undefined && { where: options.where }),
        ...(options.ordering !== undefined && { ordering: options.ordering }),
      };
    }
    
    // Fallback: If both structures are empty but a JSON schema is attached, derive from it
    // This handles edge cases where fromJSONSchema() was called but metadata wasn't properly populated
    const hasProperties = Object.keys(propertiesMetadata).length > 0;
    const hasRelations = Object.keys(relationsMetadata).length > 0;
    const hasMetadata = hasProperties || hasRelations;
    
    if (!hasMetadata && prototype.__jsonSchema) {
      // Derive metadata from the attached JSON schema
      const schema = prototype.__jsonSchema;
      const options = prototype.__jsonSchemaOptions || {};
      
      if (schema.properties) {
        for (const [propertyName, propertySchema] of Object.entries(schema.properties)) {
          const isArray = isArrayType(propertySchema as JSONSchemaProperty);
          const predicate = determinePredicate(
            schema, 
            propertyName, 
            propertySchema as JSONSchemaProperty, 
            determineNamespace(schema, options),
            options
          );
          
          if (isArray) {
            relationsMetadata[propertyName] = {
              name: propertyName,
              predicate: predicate,
              ...(propertySchema["x-ad4m"]?.local !== undefined && { local: propertySchema["x-ad4m"].local })
            };
          } else {
            const isRequired = schema.required?.includes(propertyName) || false;
            propertiesMetadata[propertyName] = {
              name: propertyName,
              predicate: predicate,
              required: isRequired,
              readOnly: propertySchema["x-ad4m"]?.writable === false,
              ...(propertySchema["x-ad4m"]?.resolveLanguage !== undefined && { resolveLanguage: propertySchema["x-ad4m"].resolveLanguage }),
              ...(propertySchema["x-ad4m"]?.initial && { initial: propertySchema["x-ad4m"].initial }),
              ...(propertySchema["x-ad4m"]?.local !== undefined && { local: propertySchema["x-ad4m"].local })
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

  /**
   * Constructs a new model instance.
   * 
   * @param perspective - The perspective where this model will be stored
   * @param baseExpression - Optional expression URI for this instance.
   *             If omitted, a random `ad4m://obj/<id>` IRI is generated,
   *             independent of any property content — this is what keeps
   *             two instances with identical property values distinct.
   * @param source - Optional source expression this instance is linked to
   *
   * @example
   * ```typescript
   * // Create a new recipe with auto-generated base expression
   * const recipe = new Recipe(perspective);
   *
   * // Create with specific base expression
   * const recipe = new Recipe(perspective, "ad4m://obj/existing-id");
   * ```
   */
  constructor(perspective: PerspectiveProxy, baseExpression?: string) {
    // Use a dedicated `ad4m://obj/<id>` scheme for auto-generated
    // baseExpressions instead of `Literal.from(...).toUrl()`'s
    // `literal:string:<id>`. After typed-literal storage lands, the storage
    // layer translates any `literal:string:X` target into a typed
    // `"X"^^xsd:string` literal — which is correct for property values but
    // wrong for relation targets (which must remain NamedNode IRIs). Using
    // a distinct scheme keeps auto-generated instance IDs unambiguously
    // IRIs, so relation links like `<post> --has_comment--> <comment-id>`
    // round-trip correctly through the SPARQL store.
    this._baseExpression = baseExpression ? baseExpression : `ad4m://obj/${makeRandomId(24)}`;
    this._perspective = perspective;
  }

  /**
   * The unique identifier (expression URI) of this model instance.
   */
  get id(): string {
    return this._baseExpression;
  }

  /**
   * Protected getter for the perspective.
   * Allows subclasses to access the perspective while keeping it private from external code.
   */
  protected get perspective(): PerspectiveProxy {
    return this._perspective;
  }

  /**
   * Get property metadata from decorator.
   * @private
   */
  private getPropertyMetadata(key: string): PropertyMetadataEntry | undefined {
    const ctor = this.constructor;
    const props = getPropertiesMetadata(ctor);
    return props[key];
  }

  /**
   * Get relation options from decorator
   * @private
   */
  private getRelationOptions(key: string): RelationMetadataEntry | undefined {
    const ctor = this.constructor;
    const rels = getRelationsMetadata(ctor);
    return rels[key];
  }

  /**
   * Refuses a write of field `key` when its class declares a call writer
   * for it. A call writer is the mount form of a write: the executor runs it
   * from the shape, and a language owns the record. A `mutate` update would
   * write the field as links and bypass that language. A create hands the
   * field on, because the executor runs the shape's constructor and setters.
   * @private
   */
  private refuseCallWriter(key: string): void {
    if (this.getPropertyMetadata(key)?.setter) {
      throw new Error(
        `Property "${key}" declares a call setter — call writers run only through ` +
        `the executor's shape actions, so Ad4mModel's save path cannot ` +
        `write it. Declare a plain @Property (no setter) for SDK-written properties.`
      );
    }
    const relation = this.getRelationOptions(key);
    if (relation?.adder || relation?.remover) {
      const writers = [relation.adder && "adder", relation.remover && "remover"]
        .filter(Boolean).join("/");
      throw new Error(
        `Relation "${key}" declares a call ${writers} — call writers run only through ` +
        `the executor's shape actions, so Ad4mModel's save path cannot ` +
        `write it. Declare a plain relation (no adder/remover) for SDK-written relations.`
      );
    }
  }
  // ──────────────────────────────────────────────────────────
  //  Snapshot / dirty tracking
  // ──────────────────────────────────────────────────────────

  /**
   * @param includedRelations  Controls which relation fields are recorded
   *   in the snapshot for dirty-tracking:
   *     • `undefined` (default) — snapshot ALL relations (used by `.get()`,
   *       `.create()`, `.save()` etc. where full hydration has occurred).
   *     • `IncludeMap` object (e.g. `{ views: true }`) — only snapshot the
   *       relations named in the map.  Fields not listed are omitted from
   *       the snapshot so that `changedFields()` ignores them.
   *     • `null` / empty object — skip ALL relations (used by bare
   *       subscriptions that don't eagerly load relations).
   */
  /** @internal */
  protected takeSnapshot(includedRelations?: Record<string, any> | null): void {
    const ctor = this.constructor as typeof Ad4mModel;
    const metadata = ctor.getModelMetadata();
    const snap: Record<string, any> = {};

    // Always snapshot properties
    for (const propName of Object.keys(metadata.properties)) {
      const val = (this as any)[propName];
      snap[propName] = normalizeValue(
        Array.isArray(val) ? [...val] : val,
      );
    }

    // Snapshot relations only when appropriate:
    //   undefined → all relations (backward compat)
    //   IncludeMap → only the keys present in the map
    //   null / {} → none
    if (includedRelations === undefined) {
      // Full snapshot — e.g. after getData() / create()
      for (const relName of Object.keys(metadata.relations)) {
        const val = (this as any)[relName];
        snap[relName] = normalizeValue(
          Array.isArray(val) ? [...val] : val,
        );
      }
    } else if (includedRelations && Object.keys(includedRelations).length > 0) {
      // Partial snapshot — only the explicitly included relations
      for (const relName of Object.keys(includedRelations)) {
        if (relName in metadata.relations) {
          const val = (this as any)[relName];
          snap[relName] = normalizeValue(
            Array.isArray(val) ? [...val] : val,
          );
        }
      }
    }
    // else: null or empty object → skip all relations

    this._snapshot = snap;
  }

  /**
   * Returns `true` if any tracked property or relation has changed
   * since the last hydration (or since `takeSnapshot()` was last called).
   *
   * Always returns `true` if no snapshot exists (e.g. a freshly
   * constructed instance that hasn't been fetched yet).
   *
   * @example
   * ```typescript
   * const recipe = await Recipe.create(perspective, { name: "Pasta" });
   * recipe.isDirty();        // false — just hydrated
   * recipe.name = "Risotto";
   * recipe.isDirty();        // true
   * ```
   */
  isDirty(): boolean {
    if (!this._snapshot) return true;
    return this.changedFields().length > 0;
  }

  /**
   * Returns the names of properties/relations that differ from the
   * snapshot taken at the last hydration.
   *
   * Returns **all** field names if no snapshot exists.
   *
   * @example
   * ```typescript
   * recipe.name = "New Name";
   * recipe.changedFields(); // ["name"]
   * ```
   */
  changedFields(): string[] {
    const ctor = this.constructor as typeof Ad4mModel;
    const metadata = ctor.getModelMetadata();

    if (!this._snapshot) {
      return [
        ...Object.keys(metadata.properties),
        ...Object.keys(metadata.relations),
      ];
    }

    const changed: string[] = [];
    const allFields = [
      ...Object.keys(metadata.properties),
      ...Object.keys(metadata.relations),
    ];

    for (const field of allFields) {
      // Skip fields that were not recorded in the snapshot (e.g. relation
      // fields omitted because the originating query had no `include`).
      // Without this guard, a relation populated by hydrateFromLinks with
      // raw string IDs would appear "dirty" against a missing snapshot
      // entry and trigger an unnecessary (and potentially destructive)
      // relation write on save().
      if (!(field in this._snapshot)) continue;

      const current = normalizeValue((this as any)[field]);
      const original = this._snapshot[field];

      if (Array.isArray(current) || Array.isArray(original)) {
        // For an ordered relation the sequence *is* the state, so a reorder is
        // the whole change and must mark the field dirty — sorting first would
        // make `save()` a no-op for the one edit the ordering feature exists to
        // support.
        //
        // Everything else compares order-insensitively, because an unordered
        // relation is a set: the executor returns its members by link timestamp,
        // and a caller who assigned the same members in another order has not
        // changed anything.
        const ordered = !!(metadata.relations as any)[field]?.ordering;
        const a = Array.isArray(current) ? [...current] : [];
        const b = Array.isArray(original) ? [...original] : [];
        if (!ordered) {
          a.sort();
          b.sort();
        }
        if (a.length !== b.length || a.some((v: any, i: number) => !sameValue(v, b[i]))) {
          changed.push(field);
        }
      } else if (!sameValue(current, original)) {
        changed.push(field);
      }
    }
    return changed;
  }

  /** Hydrate this instance from the executor. `null` when no instance has this ID. */
  private async getData(opts?: GetOptions): Promise<this | null> {
    const ctor = this.constructor as typeof Ad4mModel;
    const query: Query = {
      where: { id: this._baseExpression },
      limit: 1,
      deepQuery: true,  // Single-instance get() always evaluates property getters
    };
    if (opts?.properties) query.properties = opts.properties;
    if (opts?.include) query.include = opts.include;

    const { results } = await (ctor as any).executeModelQuery(
      this._perspective, query, null
    );
    if (results.length === 0) return null;

    // A field with a write not settled yet (queued in a transaction, or in flight)
    // is not the store's to give: it keeps its value and its snapshot, which the
    // write's claim owns (see `claimFields`).
    const pending = new Set(unsettledFields(this));
    const held = this._snapshot;
    // Copy all hydrated values from the executor's result onto this instance
    for (const [key, value] of Object.entries(results[0] as any)) {
      if (key.startsWith('_') || pending.has(key)) continue;
      // Check for readonly getters
      let isReadonly = false;
      let proto = Object.getPrototypeOf(this);
      while (proto) {
        const desc = Object.getOwnPropertyDescriptor(proto, key);
        if (desc) { isReadonly = !!(desc.get && !desc.set); break; }
        proto = Object.getPrototypeOf(proto);
      }
      if (isReadonly) continue;
      (this as any)[key] = value;
    }

    this.takeSnapshot(opts?.include);
    for (const field of pending) {
      if (held && field in held && this._snapshot) this._snapshot[field] = held[field];
    }
    return this;
  }

  /**
   * `query` with `includeAll` expanded to an `include` of every forward
   * (has-many / has-one) relation of `classes`; explicit `include` entries win.
   * @internal
   */
  static withIncludeAll(classes: (typeof Ad4mModel)[], query: Query): Query {
    if (!query.includeAll) return query;
    const { includeAll, ...rest } = query;
    if (classes.some((c) => c === Ad4mModel)) {
      // A query by class name runs on the undecorated base class: the client
      // does not know the named model's relations, so it cannot expand them.
      throw new Error("includeAll needs a decorated model class; for a query by class name, list the relations in `include`");
    }
    const expanded: IncludeMap = {};
    for (const c of classes) {
      for (const [name, rel] of Object.entries(c.getModelMetadata().relations)) {
        if (rel.direction !== 'reverse') expanded[name] = true;
      }
    }
    return { ...rest, include: { ...expanded, ...query.include } };
  }

  /**
   * Build the JSON parameters needed for a model query/subscription endpoint.
   * Returns the className and queryJson that the Rust executor expects.
   * Shape resolution happens server-side from the perspective's SHACL triples.
   * @internal
   */
  /**
   * The IRI the executor addresses this class by: its target class, or a
   * built-in abstract class's URI (`ad4m://TextContent`, #1446). The URI is
   * the class's own static only: a subclass is a class of its own.
   * @internal
   */
  static queriedAs(): string {
    const builtinUri = Object.prototype.hasOwnProperty.call(this, 'builtinUri') ? (this as any).builtinUri : undefined;
    return builtinUri || this.getTargetClass();
  }

  static prepareModelQueryParams(
    query: Query = {},
    classNameOverride?: string | null,
  ): { className: string; queryJson: string } {
    // A query by class name runs on the undecorated base class: no metadata,
    // so queriedAs() (which may read it) only runs without an override.
    const className = classNameOverride || this.queriedAs();

    query = Ad4mModel.withIncludeAll([this], query);

    const queryInput: any = {};
    if (query.parent && isContainedScope(query.parent)) {
      // Containment: the executor resolves the predicates from the installed
      // classes, so only the id travels.
      queryInput.parent = { id: query.parent.id };
    } else if (query.parent && isContainmentTraversal(query.parent)) {
      // A traversal over containment: no predicate travels either.
      const t = query.parent;
      const traverse: Record<string, unknown> = { ids: t.ids };
      if (t.transitive !== undefined) traverse.transitive = t.transitive;
      if (t.direction !== undefined) traverse.direction = t.direction;
      if (t.limitPerAnchor !== undefined) traverse.limitPerAnchor = t.limitPerAnchor;
      if (t.levels !== undefined) traverse.levels = t.levels;
      if (t.maxDepth !== undefined) traverse.maxDepth = t.maxDepth;
      queryInput.parent = traverse;
    } else if (query.parent) {
      const parentPredicate = resolveParentPredicate(query.parent, this);
      // A traversal keeps its own shape on the wire: the executor reads `ids`,
      // and flattening it to one `id` here is how a request for a whole level
      // would quietly become a request for one parent's children.
      //
      // Its fields are named rather than spread, because `model` — the class
      // itself — is how the scope may name its predicate, and that is a
      // constructor, not something to put in a query variable. The predicate is
      // resolved above; what travels is the result.
      if (isTraverseScope(query.parent)) {
        const t = query.parent;
        const traverse: Record<string, unknown> = {
          ids: t.ids,
          predicate: parentPredicate,
        };
        if (t.transitive !== undefined) traverse.transitive = t.transitive;
        if (t.direction !== undefined) traverse.direction = t.direction;
        if (t.limitPerAnchor !== undefined) traverse.limitPerAnchor = t.limitPerAnchor;
        if (t.levels !== undefined) traverse.levels = t.levels;
        if (t.maxDepth !== undefined) traverse.maxDepth = t.maxDepth;
        queryInput.parent = traverse;
      } else {
        queryInput.parent = { id: query.parent.id, predicate: parentPredicate };
      }
    }
    if (query.properties) queryInput.properties = query.properties;
    if (query.include) {
      // $-prefixed keys → queryInput.projections; all other keys (boolean |
      // RelationSubQuery) → queryInput.include.
      const { includes: normalIncludes, projections } = splitIncludeProjections(query.include);
      // A relation declared `polymorphic` reads that way by default, so the
      // caller writes `include: { children: true }` and still gets each child
      // hydrated as the class it actually is. Declaring it on the model rather
      // than repeating it at every call site is the point — the relation being
      // heterogeneous is a fact about the data, not about one query — but an
      // explicit `polymorphic: false` at the call site still wins. Applied at
      // every depth, since a nested include is a fact about the data too.
      if (Object.keys(normalIncludes).length > 0) {
        applyPolymorphicIncludeDefaults(normalIncludes, this as any);
        queryInput.include = liftNestedProjections(normalIncludes, this as any);
      }
      if (Object.keys(projections).length > 0) {
        tagProjectionTargets(projections, this as any);
        queryInput.projections = projections;
      }
    }
    if (query.where) queryInput.where = query.where;
    if (query.order) {
      queryInput.order = Object.entries(query.order).map(([k, v]) => [k, v]);
    }
    if (query.offset !== undefined) queryInput.offset = query.offset;
    if (query.limit !== undefined) queryInput.limit = query.limit;
    if (query.after !== undefined) queryInput.after = query.after;
    if (query.count !== undefined) queryInput.count = query.count;
    if (query.links) queryInput.links = query.links;
    queryInput.deepQuery = query.deepQuery ?? true;
    if (query.linkStatus != null) queryInput.linkStatus = query.linkStatus;
    if (query.includeUnverified !== undefined) queryInput.includeUnverified = query.includeUnverified;

    // Conformance getters, where filters, and target shapes for includes
    // are all resolved by the executor from the perspective's SHACL triples
    // — no client-side pre-computation is needed any more.

    return {
      className,
      queryJson: JSON.stringify(queryInput),
    };
  }

  /**
   * Parse raw model query/subscription result JSON into typed model instances.
   * Used by live queries to convert subscription updates without circular imports.
   * @internal
   */
  static parseModelResult<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    raw: any,
    include?: IncludeMap,
    properties?: string[],
  ): T[] {
    const data = typeof raw === 'string' ? JSON.parse(raw) : raw;
    const arr = data.instances || data;
    if (!Array.isArray(arr)) return [];
    return arr.map((json: any) => jsonToModelInstance(this, perspective, json, include, properties));
  }


  // ── One query over several classes (#1238) ────────────────────────────────

  /**
   * Build the wire parameters for one query over several classes.
   *
   * Each class normalises the query as it would for itself — its polymorphic
   * include defaults, its `$` projections, its parent predicate — and the
   * results are merged: includes and projections by key (the first class that
   * declares a key decides its form; a projection two classes tag with
   * different target classes is refused), the parent scope only when every
   * class that can resolve it resolves it to the same predicate.
   * @internal
   */
  static prepareUnionQueryParams(
    classes: (typeof Ad4mModel)[],
    query: Query = {},
  ): { classNames: string[]; queryJson: string } {
    if (!classes.length) throw new Error('A query over several classes needs at least one class');
    const { parent, preferClasses, ...rest } = query;
    const inputs = classes.map((c) => JSON.parse(c.prepareModelQueryParams(rest).queryJson));
    // A projection is tagged with its relation's target class, at every depth,
    // and the executor hydrates a list projection (and resolves its `where`)
    // as that class. Two classes that tag the same key differently cannot share
    // one form of it, so the key is refused rather than read as the first's.
    const tags = new Map<string, string>();
    const collectTags = (node: any, path: string) => {
      if (!node || typeof node !== 'object') return;
      for (const [k, v] of Object.entries(node)) {
        if (k === 'targetClassName' && typeof v === 'string') {
          const seen = tags.get(path);
          if (seen !== undefined && seen !== v) {
            throw new Error(
              `projection '${path}': the classes resolve it to different target classes ` +
              `(${seen}, ${v}); query those classes separately`,
            );
          }
          tags.set(path, v);
        } else {
          collectTags(v, path ? `${path}.${k}` : k);
        }
      }
    };
    for (const input of inputs) {
      collectTags(input.include, 'include');
      collectTags(input.projections, 'projections');
    }
    const merged: any = { ...inputs[0] };
    if (preferClasses) merged.preferClasses = preferClasses;
    for (const key of ['include', 'projections'] as const) {
      const combined = Object.assign({}, ...inputs.map((i) => i[key] ?? {}).reverse());
      if (Object.keys(combined).length) merged[key] = combined;
      else delete merged[key];
    }
    if (parent) {
      // A model-form scope names its predicate through the relation that
      // targets the queried class, so each class may resolve it differently
      // or not at all. One query has one scope.
      const scopes: string[] = [];
      let firstError: unknown;
      for (const c of classes) {
        try {
          scopes.push(JSON.stringify(JSON.parse(c.prepareModelQueryParams({ parent }).queryJson).parent));
        } catch (e) {
          firstError ??= e;
        }
      }
      if (!scopes.length) throw firstError;
      if (new Set(scopes).size > 1) {
        throw new Error(
          'parent(): the classes resolve the parent scope to different predicates; ' +
          'pass the predicate itself (or a `field`) so they share one scope',
        );
      }
      merged.parent = JSON.parse(scopes[0]);
    }
    return {
      classNames: classes.map((c) => c.queriedAs()),
      queryJson: JSON.stringify(merged),
    };
  }

  /**
   * Build each row of a union result as the class the executor hydrated it as
   * (`__subjectClass`, the key it was asked for: {@link queriedAs}). A row of
   * a class not in `classes` stays plain JSON.
   * @internal
   */
  static parseUnionResult(
    classes: (typeof Ad4mModel)[],
    perspective: PerspectiveProxy,
    raw: any,
    include?: IncludeMap,
    properties?: string[],
  ): Ad4mModel[] {
    const data = typeof raw === 'string' ? JSON.parse(raw) : raw;
    const arr = data.instances || data;
    if (!Array.isArray(arr)) return [];
    const byKey = new Map(classes.map((c) => [c.queriedAs(), c]));
    return arr.map((json: any) => {
      const cls = byKey.get(json?.[SUBJECT_CLASS_KEY]);
      return cls ? jsonToModelInstance(cls as any, perspective, json, include, properties) : json;
    });
  }

  /** @internal */
  static async executeUnionQuery(
    perspective: PerspectiveProxy,
    classes: (typeof Ad4mModel)[],
    query: Query = {},
    options?: CallOptions,
  ): Promise<Page<Ad4mModel>> {
    query = Ad4mModel.withIncludeAll(classes, query);
    const { classNames, queryJson } = Ad4mModel.prepareUnionQueryParams(classes, query);
    const result = await perspective.modelQuery(classNames, queryJson, options);
    const results = Ad4mModel.parseUnionResult(classes, perspective, result, query.include, query.properties);
    return { results, totalCount: result.totalCount };
  }

  /**
   * One query over several model classes (#1238).
   *
   * `where`, `order`, `limit` and `offset` apply once, over the union. Each
   * result is an instance of the class the executor hydrated it as: the first
   * `preferClasses` entry it conforms to, otherwise the most specific of
   * `classes` it conforms to. A record that is several of them is returned
   * once; `__subjectClasses` on it names every class it conforms to.
   *
   * - `where` on a property a class does not declare excludes that class's rows.
   * - `order` on such a property sorts those rows last, in both directions,
   *   then by id.
   *
   * @example
   * ```typescript
   * const items = await Ad4mModel.findAllOf(perspective, [Placement, EdgeRoute], {
   *   parent: { id: canvas.id, predicate: "canvas://item" },
   *   order: { zIndex: "ASC" },
   *   limit: 50,
   * });
   * for (const item of items) {
   *   if (item instanceof Placement) { ... }
   * }
   * ```
   */
  static async findAllOf<C extends (typeof Ad4mModel)[]>(
    perspective: PerspectiveProxy,
    classes: [...C],
    query?: Query,
    options?: CallOptions,
  ): Promise<InstanceType<C[number]>[]> {
    const { results } = await Ad4mModel.executeUnionQuery(perspective, classes, query ?? {}, options);
    return results as InstanceType<C[number]>[];
  }

  /**
   * A live page of the query: one {@link LiveQuery} that opens at once, holds
   * the current {@link Page} and moves to the next one whenever the result
   * changes. The snapshot's `initial` is true for the opening page only.
   *
   * `Page.totalCount` is set when the query asks for `count`. A cursor
   * (`after`) is not sent: a live query re-runs one query, it does not walk
   * pages. `options.className` queries the class registered under that name
   * instead of this class (rows of an undecorated `Ad4mModel` hydrate as plain
   * `Ad4mModel`s, and `includeAll` is not available: list the relations in
   * `include`). Call `dispose()` when done.
   *
   * @example
   * ```typescript
   * const live = Recipe.query(perspective, { where: { status: "cooking" }, limit: 10, count: true });
   * const page = await live.first;
   * const stop = live.subscribe((snap) => render(snap.data?.results));
   * // later
   * stop();
   * live.dispose();
   * ```
   */
  static query<T extends Ad4mModel, Q extends TypedQuery<T> = {}>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query?: Q,
    options: LiveOptions & { className?: string } = {},
  ): LiveQuery<Page<T & IncludeExtras<T, IncludeOf<Q>>>> {
    const { after: _after, ...cursorless } = (query ?? {}) as Query;
    const withAll = Ad4mModel.withIncludeAll([this], cursorless);
    const { className, queryJson } = this.prepareModelQueryParams(withAll, options.className);
    return new LiveQuery<Page<T & IncludeExtras<T, IncludeOf<Q>>>>(
      perspective.client, perspective.uuid,
      (signal) => perspective.client.subscribe(perspective.uuid, { className, queryJson }, { signal, timeoutMs: options.timeoutMs }),
      (raw) => ({
        // A copy: LiveQuery keeps the raw result to apply later diffs to.
        results: this.parseModelResult(perspective, structuredClone(raw), withAll.include, withAll.properties) as (T & IncludeExtras<T, IncludeOf<Q>>)[],
        ...(raw?.totalCount !== undefined ? { totalCount: raw.totalCount } : {}),
      }),
    );
  }

  /** {@link query} over several model classes: one live query that re-runs when any of them changes. */
  static queryOf<C extends (typeof Ad4mModel)[]>(
    perspective: PerspectiveProxy,
    classes: [...C],
    query?: Query,
    options: LiveOptions = {},
  ): LiveQuery<Page<InstanceType<C[number]>>> {
    // Copy once, so a caller changing its array later does not change hydration.
    const classList = [...classes];
    const { after: _after, ...cursorless } = query ?? {};
    const withAll = Ad4mModel.withIncludeAll(classList, cursorless);
    const { classNames, queryJson } = Ad4mModel.prepareUnionQueryParams(classList, withAll);
    return new LiveQuery<Page<InstanceType<C[number]>>>(
      perspective.client, perspective.uuid,
      (signal) => perspective.client.subscribe(perspective.uuid, { className: classNames, queryJson }, { signal, timeoutMs: options.timeoutMs }),
      (raw) => ({
        results: Ad4mModel.parseUnionResult(classList, perspective, structuredClone(raw), withAll.include, withAll.properties) as InstanceType<C[number]>[],
        ...(raw?.totalCount !== undefined ? { totalCount: raw.totalCount } : {}),
      }),
    );
  }

  // instancesFromQueryResult — removed (superseded by Rust executeModelQuery pipeline)

  /**
   * Execute a model query via the executor-side endpoint.
   * 
   * This replaces the old SPARQL-build → hydrate → JS-filter → JS-sort → JS-paginate pipeline
   * with a single RPC to the executor that handles everything in Rust.
   * 
   * @internal
   */
  private static async executeModelQuery<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query: Query = {},
    classNameOverride?: string | null,
    options?: CallOptions,
  ): Promise<Page<T>> {
    // Delegate query input building to the shared prepareModelQueryParams
    // helper.  The executor resolves the shape from SHACL server-side.
    query = Ad4mModel.withIncludeAll([this], query);
    const { className, queryJson } = this.prepareModelQueryParams(
      query, classNameOverride,
    );

    const result = await perspective.modelQuery(className, queryJson, options);

    return {
      results: this.parseModelResult(perspective, result, query.include, query.properties),
      totalCount: result.totalCount,
      ...(result.nextCursor !== undefined ? { nextCursor: result.nextCursor } : {}),
    };
  }

  /**
   * Gets all instances of the model in the perspective that match the query params.
   * 
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @param _engine - **Deprecated.** Accepted for backward compatibility but ignored.
   *   All queries now use the Rust SPARQL pipeline.
   * @returns Array of matching models
   *
   * @example
   * ```typescript
   * const allRecipes = await Recipe.findAll(perspective);
   *
   * const recipes = await Recipe.findAll(perspective, {
   *   where: {
   *     name: "Pasta",
   *     rating: { gt: 4 }
   *   },
   *   order: { createdAt: "DESC" },
   *   limit: 10
   * });
   * ```
   */
  static async findAll<T extends Ad4mModel, Q extends TypedQuery<T> = {}>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query?: Q,
    options?: CallOptions,
  ): Promise<(T & IncludeExtras<T, IncludeOf<Q>>)[]> {
    const q = (query ?? {}) as Query;
    if (q.properties && q.properties.length === 0) {
      throw new Error("properties[] must not be empty — omit the field to return all properties, or specify at least one field name");
    }

    const { results } = await this.executeModelQuery(perspective, q, undefined, options);
    return results as (T & IncludeExtras<T, IncludeOf<Q>>)[];
  }

  /**
   * Finds the first instance matching the query, or `null` if none exists.
   *
   * Equivalent to `findAll` with `limit: 1` — only one instance is hydrated.
   *
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @param _engine - **Deprecated.** Accepted for backward compatibility but ignored.
   * @returns The first matching instance, or `null`
   *
   * @example
   * ```typescript
   * const recipe = await Recipe.findOne(perspective, {
   *   where: { name: "Pasta" }
   * });
   * if (recipe) {
   *   console.log(recipe.name);
   * }
   * ```
   */
  static async findOne<T extends Ad4mModel, Q extends TypedQuery<T> = {}>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query?: Q,
    options?: CallOptions,
  ): Promise<(T & IncludeExtras<T, IncludeOf<Q>>) | null> {
    const limitedQuery = { ...((query ?? {}) as Query), limit: 1 } as Q;
    const results = await this.findAll<T, Q>(perspective, limitedQuery, options);
    return results[0] ?? null;
  }

  /**
   * Gets all instances with count of total matches without offset & limit applied.
   * 
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @param _engine - **Deprecated.** Accepted for backward compatibility but ignored.
   * @returns Object containing results array and total count
   *
   * @example
   * ```typescript
   * const { results, totalCount } = await Recipe.findAllAndCount(perspective, {
   *   where: { category: "Dessert" },
   *   limit: 10
   * });
   * console.log(`Showing 10 of ${totalCount} dessert recipes`);
   * ```
   */
  static async findAllAndCount<T extends Ad4mModel, Q extends TypedQuery<T> = {}>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query?: Q,
    options?: CallOptions,
  ): Promise<ResultsWithTotalCount<T & IncludeExtras<T, IncludeOf<Q>>>> {
    const answer = await this.executeModelQuery(perspective, { ...((query ?? {}) as Query), count: true }, undefined, options);
    return { results: answer.results as (T & IncludeExtras<T, IncludeOf<Q>>)[], totalCount: requireTotalCount(answer) };
  }

  /**
   * Helper function for pagination with explicit page size and number.
   * 
   * @param perspective - The perspective to search in
   * @param pageSize - Number of items per page
   * @param pageNumber - Which page to retrieve (1-based)
   * @param query - Optional additional query parameters
   * @returns Paginated results with metadata
   * 
   * @example
   * ```typescript
   * const page = await Recipe.paginate(perspective, 10, 1, {
   *   where: { category: "Main Course" }
   * });
   * console.log(`Page ${page.pageNumber} of recipes, ${page.results.length} items`);
   * ```
   */
  static async paginate<T extends Ad4mModel, Q extends TypedQuery<T> = {}>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    pageSize: number,
    pageNumber: number,
    query?: Q,
    options?: CallOptions,
  ): Promise<PaginationResult<T & IncludeExtras<T, IncludeOf<Q>>>> {
    // Page numbers page by offset; a cursor from the starting query has no meaning here.
    const { after: _after, ...base } = (query ?? {}) as Query;
    const paginationQuery = { ...base, limit: pageSize, offset: pageSize * (pageNumber - 1), count: true };
    const answer = await this.executeModelQuery(perspective, paginationQuery, undefined, options);
    return { results: answer.results as (T & IncludeExtras<T, IncludeOf<Q>>)[], totalCount: requireTotalCount(answer), pageSize, pageNumber };
  }

  /**
   * One page in timestamp order, continued by cursor rather than by offset.
   *
   * Pass no `after` (or `""`) for the first page, then each page's
   * `nextCursor`. A row created while you page cannot shift a later page the
   * way it shifts an `offset` page. The query may order by `timestamp`,
   * `createdAt` or `updatedAt` (either direction) or not at all; the executor
   * refuses any other order, and `offset` is dropped. Use `paginate()` for
   * page numbers or other orders.
   *
   * `nextCursor` is `undefined` on the last page — and `undefined` (or `""`)
   * as `after` asks for the first page, so loop on the cursor:
   * `while (page.nextCursor)`. A `do … while (page.results.length)` loop
   * passes `page?.nextCursor` — `undefined` again on the last page — and
   * re-reads page one forever.
   *
   * @example
   * ```typescript
   * let page = await Message.page(perspective, 50, undefined, { order: { createdAt: "DESC" } });
   * while (page.nextCursor) {
   *   page = await Message.page(perspective, 50, page.nextCursor, { order: { createdAt: "DESC" } });
   * }
   * ```
   */
  static async page<T extends Ad4mModel, Q extends TypedQuery<T> = {}>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    pageSize: number,
    after?: string,
    query?: Q,
    options?: CallOptions,
  ): Promise<CursorPage<T & IncludeExtras<T, IncludeOf<Q>>>> {
    // A cursor page returns no total, so `count` is dropped with `offset`: no COUNT runs for it.
    const { offset: _offset, count: _count, ...rest } = (query ?? {}) as Query;
    const pageQuery: Query = { ...rest, limit: pageSize, after: after ?? '' };
    const { results, nextCursor } = await this.executeModelQuery(perspective, pageQuery, undefined, options);
    return {
      results: results as (T & IncludeExtras<T, IncludeOf<Q>>)[],
      ...(nextCursor !== undefined ? { nextCursor } : {}),
      pageSize,
    };
  }

  /**
   * Gets a count of all matching instances.
   * 
   * @param perspective - The perspective to search in
   * @param query - Optional query parameters to filter results
   * @returns Total count of matching entities
   * 
   * @example
   * ```typescript
   * const totalRecipes = await Recipe.count(perspective);
   * const activeRecipes = await Recipe.count(perspective, {
   *   where: { status: "active" }
   * });
   * ```
   */
  static async count<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    query?: TypedQuery<T>,
    options?: CallOptions,
  ): Promise<number> {
    // A total is one figure over the whole scope; a cursor pages it, so `after` is dropped like `paginate` does.
    const { after: _after, ...countQuery } = (query ?? {}) as Query;
    return requireTotalCount(await this.executeModelQuery(perspective, { ...countQuery, limit: 0, count: true }, undefined, options));
  }

  /** Resolve a relation argument to a plain string ID. Accepts either a raw
   * string ID or an Ad4mModel instance (in which case `.id` is used). */
  private resolveRelationId(value: any): string {
    if (typeof value === 'string') return value;
    if (value && typeof value === 'object' && typeof value.id === 'string') return value.id;
    return String(value);
  }

  private async setRelationValues(key: string, value: any, batchId?: string) {
    await this.writeRelation(key, 'set', value, batchId);
  }

  private async addRelationValue(key: string, value: any, batchId?: string) {
    await this.writeRelation(key, 'add', value, batchId);
  }

  private async removeRelationValue(key: string, value: any, batchId?: string) {
    await this.writeRelation(key, 'remove', value, batchId);
  }

  /** One update op that sets (`set`: an array sets the members), adds or removes the members of relation `key`. */
  private async writeRelation(key: string, kind: 'set' | 'add' | 'remove', value: any, batchId?: string) {
    if (!this.getRelationOptions(key)) {
      console.warn(`Relation "${key}" has no metadata, skipping`);
      return;
    }
    this.refuseCallWriter(key);
    if (value == null) return;
    const ids = (Array.isArray(value) ? value : [value]).map((v) => this.resolveRelationId(v));
    // A single-valued relation (`@HasOne`) has one member: adding one points the relation at it,
    // which is a `set`; the executor refuses `add` on a relation that is not a collection.
    const single = (this.constructor as typeof Ad4mModel).getModelMetadata().relations[key].maxCount === 1;
    const asSet = kind === 'set' || (kind === 'add' && single);
    const members = { [key]: asSet ? this.relationWrite(key, ids) : ids };
    const change = asSet ? { set: members } : { [kind]: members };
    await writeOps(this._perspective, [{ op: 'update', class: this.mutateClass(), id: this._baseExpression, ...change }], batchId);
  }

  /**
   * Saves the model instance to the perspective.
   *
   * Every save is one `perspective.mutate` call.
   *
   * **New instances** (no snapshot yet): one `create` op with every
   * property that has a value and every relation that has members, then
   * (without a `batchId`) a refresh from the perspective. To create
   * without that read, use `Model.create()`.
   *
   * **Existing instances** (snapshot present, i.e. fetched via `get()`
   * or a query): one `update` op with the dirty fields only, then a
   * refresh from the perspective.
   *
   * A property set to `null` or `''` has no links after the write: an
   * existing instance's links are removed, a new instance gets none.
   * `undefined` writes nothing.
   * 
   * @param batchId - Optional batch ID for batch operations. In a batch you
   *   opened with `createBatch()`, a `save()` of a new instance stages it;
   *   a later `save()` of it in the same batch fails `notFound` (the
   *   executor finds an instance an earlier call staged only after the
   *   commit). Inside `Ad4mModel.transaction` the writes are one call, so
   *   they see each other.
   * @throws Will throw if instance creation, linking, or updating fails
   * 
   * @example
   * ```typescript
   * // Create
   * const recipe = new Recipe(perspective);
   * recipe.name = "Spaghetti";
   * await recipe.save();
   * 
   * // Update
   * recipe.rating = 10;
   * await recipe.save();
   * ```
   */
  async save(batchId?: string) {
    await this.persist(batchId);
    // With a caller's batch nothing is committed yet: reading would fetch
    // stale data and overwrite in-memory values. `create()` reads the row itself.
    if (!batchId && !creating.has(this)) await this.getData();
  }

  /**
   * Writes this instance with one `perspective.mutate` op: an update of the
   * changed fields for an instance with a snapshot, a create (with `parent`
   * when given) for one without. Then takes the snapshot, so a further
   * `save()` diffs against what was written and updates instead of creating
   * again.
   */
  private async persist(batchId?: string): Promise<void> {
    const parent = creating.get(this)?.parent;
    const op = this._snapshot ? this.updateOp() : this.createOp(parent);
    // Nothing to write: the snapshot (and an undo that waits on it) stays as it is.
    if (!op) return;
    const before = this._snapshot;
    // The snapshot holds what the op carries: take it before the await, so a field
    // changed while the call is pending stays dirty for the next save.
    this.takeSnapshot();
    const fields = op.op === "create" ? Object.keys(this._snapshot!) : opFields(op);
    const claim = this.claimFields(fields, before);
    try {
      await writeOps(this._perspective, [op], batchId, claim);
    } catch (e) {
      claim.undo();
      throw e;
    }
  }

  /**
   * Records one write of `fields` on top of the writes of them not settled yet.
   * Its `undo` takes the write out: when no later write of a field is held, the
   * snapshot goes back to what the field held before (when that was before a
   * create, the instance is unsaved again once no field is left); otherwise the
   * next write starts from that value instead, so whichever undo comes last restores what the store
   * holds, in any order. Its `done` marks it written; the written writes at the
   * bottom are dropped, as nothing older can hand them a value.
   *
   * Example: the store and the snapshot hold `title` "O". Write A sets "A"
   * (claim: before "O"), then write B sets "B" (claim: before "A"); neither is
   * written yet, so the snapshot shows "B".
   * - Undo B, then A: B is the top claim, so the snapshot goes back to "A"; A is
   *   then the top claim, so it goes back to "O".
   * - Undo A, then B: a later claim (B) is held, so A leaves the snapshot alone
   *   and B's before becomes "O"; B, now the top claim, restores "O".
   * - Undo A, B written: B's before becomes "O", but B's `done` drops it: the
   *   snapshot keeps "B", which the store holds.
   */
  private claimFields(fields: string[], before: Record<string, any> | null): WriteClaim {
    const claims = fieldClaims.get(this) ?? new Map<string, FieldClaim[]>();
    fieldClaims.set(this, claims);
    const owner = {};
    for (const field of fields) {
      const held = claims.get(field) ?? [];
      claims.set(field, held);
      held.push({ owner, had: !!before && field in before, before: before?.[field], unsaved: !before, settled: false });
    }
    const each = (fn: (field: string, held: FieldClaim[], at: number) => void) => {
      for (const field of fields) {
        const held = claims.get(field);
        const at = held ? held.findIndex((c) => c.owner === owner) : -1;
        if (held && at >= 0) fn(field, held, at);
      }
    };
    const drop = (field: string, held: FieldClaim[]) => {
      while (held[0]?.settled) held.shift();
      if (held.length === 0) claims.delete(field);
    };
    return {
      undo: () => {
        const snapshot = this._snapshot;
        let unsaved = false;
        each((field, held, at) => {
          const [claim] = held.splice(at, 1);
          const later = held[at];
          if (later) {
            later.had = claim.had;
            later.before = claim.before;
            later.unsaved = claim.unsaved;
          } else if (snapshot) {
            if (claim.had) snapshot[field] = claim.before;
            else delete snapshot[field];
            unsaved ||= claim.unsaved;
          }
          drop(field, held);
        });
        // The last field of a create taken back: the instance is not in the store.
        if (snapshot && unsaved && Object.keys(snapshot).length === 0) this._snapshot = null;
      },
      done: () =>
        each((field, held, at) => {
          held[at].settled = true;
          drop(field, held);
        }),
    };
  }

  /** The class the executor writes this instance as. */
  private mutateClass(): string {
    return (this.constructor as typeof Ad4mModel).queriedAs();
  }

  /** The metadata of relation `key`, or `null` when the relation is not written: a getter, read-only, or a filtered view of a base relation. */
  private writableRelation(key: string): RelationMetadata | null {
    const metadata = (this.constructor as typeof Ad4mModel).getModelMetadata();
    const relMeta = metadata.relations[key];
    if (relMeta.getter || relMeta.readOnly) return null;
    // A target+filter relation is a filtered *view* of a base relation when
    // another relation claims the same predicate: writing it would collide.
    if (relMeta.target && relMeta.filter !== false) {
      const collides = Object.entries(metadata.relations).some(
        ([otherName, other]) => otherName !== key && (other as RelationMetadata).predicate === relMeta.predicate,
      );
      if (collides) return null;
    }
    return relMeta;
  }

  /**
   * Members `ids` of relation `key` as the executor takes them: one id (or
   * `null` for none) for a single-valued relation, which refuses an array;
   * the array for a collection. More than one member for a single-valued
   * relation stays an array, so the executor refuses it.
   */
  private relationWrite(key: string, ids: string[]): string | string[] | null {
    const single = (this.constructor as typeof Ad4mModel).getModelMetadata().relations[key].maxCount === 1;
    if (!single || ids.length > 1) return ids;
    return ids[0] ?? null;
  }

  /** A relation field's value as the member ids to write: an array, a single member, or a `{ action, value }` envelope. */
  private relationMembers(value: any): string[] {
    const members = value?.action ? value.value : value;
    if (members == null || members === "") return [];
    return (Array.isArray(members) ? members : [members]).map((v) => this.resolveRelationId(v));
  }

  /** The create op of this instance: every property with a value and every relation with members. */
  private createOp(parent?: ParentRef): MutateOp {
    const ctor = this.constructor as typeof Ad4mModel;
    // The executor refuses an abstract class too; refusing here keeps the
    // error ahead of the call.
    if ((ctor as typeof Ad4mModel & { isAbstract?: boolean }).isAbstract) {
      const name = ctor.getClassName();
      throw new Error(
        `'${name}' is an abstract class and cannot be created. Create an instance of ` +
        `a class that has its properties; querying '${name}' reads it.`
      );
    }
    const metadata = ctor.getModelMetadata();
    const values: Record<string, unknown> = {};
    for (const [key, value] of Object.entries(this)) {
      if (value === undefined || value === null || value === "") continue;
      if (metadata.relations[key]) {
        if (!this.writableRelation(key)) continue;
        // A remover has nothing to remove from a new instance.
        if (value?.action === "remover") continue;
        const members = this.relationMembers(value);
        if (members.length > 0) values[key] = this.relationWrite(key, members);
        continue;
      }
      // Only a declared, settable property: not ORM bookkeeping, a read-only
      // property or a flag (the executor writes the flags itself).
      const propMeta = metadata.properties[key];
      if (!propMeta || propMeta.readOnly || propMeta.flag) continue;
      if (Array.isArray(value)) {
        console.warn(`Property "${key}" holds an array and is not a relation, skipping`);
        continue;
      }
      values[key] = value;
    }
    return { op: "create", class: this.mutateClass(), id: this._baseExpression, values, ...(parent && { parent }) };
  }

  /**
   * The update op of the fields changed since the snapshot, or `undefined`
   * when none is. A property set to `null` or `""` is cleared; an array sets
   * a relation's members; a `{ action, value }` envelope adds, removes or sets.
   */
  private updateOp(): MutateOp | undefined {
    const metadata = (this.constructor as typeof Ad4mModel).getModelMetadata();
    const dirty = new Set(this.changedFields());
    const set: Record<string, unknown> = {};
    const add: Record<string, string[]> = {};
    const remove: Record<string, string[]> = {};
    for (const [key, value] of Object.entries(this)) {
      if (value === undefined || !dirty.has(key)) continue;
      this.refuseCallWriter(key);
      if (metadata.relations[key]) {
        if (!this.writableRelation(key)) continue;
        const members = this.relationMembers(value);
        // An envelope that is not an adder or a remover sets, as `set<Rel>` does;
        // an adder of a single-valued relation points it at the member, as
        // `add<Rel>` does (the executor refuses `add` on a relation that is not a collection).
        if (value?.action === "adder" && metadata.relations[key].maxCount === 1) {
          if (members.length > 0) set[key] = this.relationWrite(key, members);
        } else if (value?.action === "adder" || value?.action === "remover") {
          if (members.length > 0) (value.action === "adder" ? add : remove)[key] = members;
        } else if (value?.action || Array.isArray(value) || members.length > 0 || metadata.relations[key].maxCount === 1) {
          set[key] = this.relationWrite(key, members);
        }
        continue;
      }
      const propMeta = metadata.properties[key];
      // Flags are immutable after creation.
      if (!propMeta || propMeta.flag) continue;
      if (Array.isArray(value)) {
        console.warn(`Property "${key}" holds an array and is not a relation, skipping`);
        continue;
      }
      set[key] = value === "" ? null : value;
    }
    const changes = Object.fromEntries(
      Object.entries({ set, add, remove }).filter(([, change]) => Object.keys(change).length > 0),
    );
    if (Object.keys(changes).length === 0) return undefined;
    return { op: "update", class: this.mutateClass(), id: this._baseExpression, ...changes };
  }

  private cleanCopy() {
    const ctor = this.constructor as typeof Ad4mModel;
    const metadata = ctor.getModelMetadata();
    const cleanCopy: Record<string, any> = {};

    // Only include schema-declared fields (properties + relations),
    // preserving internal ORM machinery (_id, _perspective, generated
    // addX/removeX/setX methods, _snapshot, etc.)
    const schemaFields = new Set([
      ...Object.keys(metadata.properties),
      ...Object.keys(metadata.relations),
    ]);

    for (const key of schemaFields) {
      const value = (this as any)[key];
      if (value !== undefined && value !== null && key !== "author" && key !== "timestamp") {
        cleanCopy[key] = value;
      }
    }
    return cleanCopy;
  }

  /**
   * Evaluate SPARQL getters for a batch of instances on demand.
   *
   * Since deepQuery defaults to true, property getters are evaluated
   * automatically during collection queries.  Use this method when
   * deepQuery was explicitly set to false and you want to lazily
   * resolve getter-backed properties for a subset of instances.
   *
   * @param instances - Array of model instances to evaluate getters on
   * @param perspective - The perspective to query against
   * @param propertyNames - Optional list of getter-backed property names to evaluate.
   *                        If omitted, all getters are evaluated.
   *
   * @example
   * ```typescript
   * const messages = await Message.findAll(perspective, { deepQuery: false, limit: 30 });
   * // messages[i].replyingTo is undefined (getter skipped)
   *
   * // Evaluate getters for the visible subset
   * await Message.evaluateGetters(messages.slice(0, 10), perspective, ['replyingTo']);
   * // messages[0..9].replyingTo is now populated
   * ```
   */
  static async evaluateGetters<T extends Ad4mModel>(
    instances: T[],
    perspective: PerspectiveProxy,
    propertyNames?: string[],
  ): Promise<void> {
    if (instances.length === 0) return;
    const metadata = this.getModelMetadata();

    const instanceIds = instances.map(inst => inst.id || (inst as any)._baseExpression);

    // The executor reads getter SPARQL from the registered shape, so no
    // shape JSON is shipped with the call.
    const result = await perspective.evaluateGetters(
      this.queriedAs(),
      instanceIds,
      propertyNames,
    );

    // Determine which property names were evaluated for snapshot sync
    const evaluatedPropertyNames: string[] = [];
    for (const [propName, propMeta] of Object.entries(metadata.properties)) {
      if ((propMeta as any).getter) {
        if (!propertyNames || propertyNames.length === 0 || propertyNames.includes(propName)) {
          evaluatedPropertyNames.push(propName);
        }
      }
    }
    // Also include relation getters
    for (const [relName, relMeta] of Object.entries(metadata.relations)) {
      if ((relMeta as any).getter) {
        if (!propertyNames || propertyNames.length === 0 || propertyNames.includes(relName)) {
          evaluatedPropertyNames.push(relName);
        }
      }
    }

    // Apply results to instances and sync snapshots
    for (const instance of instances) {
      const id = (instance as any).id || (instance as any)._baseExpression;
      const props = result[id];
      if (props) {
        for (const [key, value] of Object.entries(props)) {
          (instance as any)[key] = value;
        }
      }
      // Sync snapshot so isDirty() doesn't flag getter-only changes
      const modelInstance = instance as Ad4mModel;
      if (modelInstance._snapshot) {
        for (const propName of evaluatedPropertyNames) {
          const val = (instance as any)[propName];
          modelInstance._snapshot[propName] = normalizeValue(Array.isArray(val) ? [...val] : val);
        }
      }
    }
  }

  /**
   * Gets the model instance with all properties and relations populated.
   *
   * @param optsOrInclude - Optional hydration options. Accepts two forms:
   *   - `GetOptions` wrapper: `{ include: { comments: true }, properties: ['title'] }`
   *   - `IncludeMap` shorthand: `{ comments: true }` (equivalent to `{ include: { comments: true } }`)
   * @returns This instance, populated, or `null` when no instance has this ID
   * @throws If the read fails
   *
   * @example
   * ```typescript
   * const recipe = new Recipe(perspective, existingId);
   * if (!(await recipe.get())) throw new Error('not found');
   * console.log(recipe.name, recipe.ingredients);
   *
   * // Shorthand — pass IncludeMap directly:
   * await recipe.get({ ingredients: true });
   *
   * // Full options — includes sparse fieldset:
   * await recipe.get({ include: { ingredients: true }, properties: ['name'] });
   * ```
   */
  async get(optsOrInclude?: GetOptions | IncludeMap): Promise<this | null> {
    // Normalise: if the caller passed a plain IncludeMap shorthand (no `include`
    // or `properties` key at the top level) wrap it in GetOptions automatically.
    let opts: GetOptions | undefined;
    if (!optsOrInclude) {
      opts = undefined;
    } else if ('include' in optsOrInclude || 'properties' in optsOrInclude) {
      opts = optsOrInclude as GetOptions;
    } else {
      opts = { include: optsOrInclude as IncludeMap };
    }

    return await this.getData(opts);
  }

  /**
   * Deletes the model instance from the perspective.
   *
   * One `delete` op: the executor runs the class's destructor, else removes
   * the links of the properties and relations the class declares, then
   * removes every incoming link (any link whose target is this instance,
   * e.g. a parent's `@HasMany` link to it), so no reference to the deleted
   * instance is left behind.
   * 
   * @param batchId - Optional batch ID for batch operations
   * @throws Will throw if removing the instance's links or its incoming links fails
   * 
   * @example
   * ```typescript
   * const recipe = await Recipe.findAll(perspective)[0];
   * await recipe.delete();
   * 
   * // Or with batch operations:
   * const batchId = await perspective.createBatch();
   * await recipe.delete(batchId);
   * await perspective.commitBatch(batchId);
   * ```
   */
  async delete(batchId?: string) {
    await writeOps(this._perspective, [{ op: "delete", class: this.mutateClass(), id: this._baseExpression }], batchId);
  }

  // ──────────────────────────────────────────────────────────
  //  Static convenience methods
  // ──────────────────────────────────────────────────────────

  /**
   * Creates and saves a new model instance in one step.
   *
   * @param perspective - The perspective to create the instance in
   * @param data - Property values to assign before saving (`null` or `''`
   *               leaves a property unwritten)
   * @param options - Optional settings:
   *   - `parent` — a `Scope` (model form or raw form) whose `id` will
   *     be used to create an incoming link from the parent to the new instance.
   *   - `batchId` — an existing batch id; when provided the link write and
   *     `save()` are added to the batch instead of committed immediately.
   *   - `returns` — `'id'` (default) returns the instance built from the
   *     values written, with its `id`, and sends no read after the commit:
   *     `author`, `createdAt`, `updatedAt`, getters and relations that were
   *     not written stay unset. `'row'` reads the row back after the commit
   *     (one round trip) and fills them. Not allowed with `batchId`: nothing
   *     is committed yet to read.
   * @returns The created model instance (see `returns`). A later `save()` on
   *   it updates; it does not create again.
   * @throws If a write or the commit fails (the batch it opened is discarded);
   *   with `returns: 'row'`, also if the read fails after the commit (the
   *   record exists then: the message names its id, so do not retry the
   *   create); with `returns: 'row'` and a `batchId`, before anything is written
   *
   * @example
   * ```typescript
   * // Simple create
   * const recipe = await Recipe.create(perspective, {
   *   name: "Spaghetti",
   *   rating: 5,
   * });
   *
   * // Create under a parent (link auto-created)
   * const comment = await Comment.create(perspective, { text: "Great!" }, {
   *   parent: { model: Post, id: postId },
   * });
   *
   * // Create inside a transaction
   * await Ad4mModel.transaction(perspective, async (tx) => {
   *   await Recipe.create(perspective, { name: "Pasta" }, { batchId: tx.batchId });
   * });
   * ```
   */
  static async create<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    data: Record<string, any> = {},
    options?: { parent?: Scope; batchId?: string; returns?: 'id' | 'row' },
  ): Promise<T> {
    if (options?.returns === 'row' && options.batchId) {
      throw new Error(
        "create(): returns: 'row' reads the row after the commit, which a caller's batch (batchId) has not had yet. " +
        "Commit the batch, then read the instance with get()."
      );
    }
    const instance = new this(perspective) as T;
    Object.assign(instance, data);
    // Resolved before any write, so an invalid parent writes nothing.
    const parent = options?.parent ? parentRef(options.parent, this) : undefined;

    // One create op, with the parent: the executor commits the instance and
    // its parent link together, or writes nothing. In a caller's batch, the
    // caller commits.
    // Through save(), the one public write: code that wraps it (a write
    // check) sees every create. The create has no read-back of its own; the
    // row is read below when the caller asks for it.
    // The parent rides on the instance, not through save()'s arguments, so a
    // subclass whose save(batchId) override forwards only the batch id still
    // creates the child under it.
    creating.set(instance, { parent });
    try {
      await instance.save(options?.batchId);
    } finally {
      creating.delete(instance);
    }
    if (options?.batchId) return instance;

    // The instance holds what it wrote and its snapshot is taken (persist()
    // did that), so nothing is read back unless the caller asked for the row.
    if (options?.returns === 'row') {
      const failed = (cause?: unknown) =>
        Object.assign(new Error(`${this.name}.create(): ${instance.id} was created, but reading it back failed`), { cause });
      const row = await instance.getData().catch((e) => { throw failed(e); });
      if (!row) throw failed();
    }
    return instance;
  }

  /**
   * Updates an existing model instance identified by `id`.
   *
   * Fetches the instance, applies the provided changes, calls `save()`,
   * and returns the refreshed instance.
   *
   * @param perspective - The perspective containing the instance
   * @param id - The expression URI of the instance to update
   * @param data - Property values to merge before saving (`null` or `''`
   *               removes the property's links)
   * @returns The updated model instance
   * @throws If no instance has this `id` (nothing is written)
   *
   * @example
   * ```typescript
   * const recipe = await Recipe.update(perspective, recipeId, {
   *   rating: 10,
   * });
   * ```
   */
  static async update<T extends Ad4mModel>(
    this: typeof Ad4mModel & (new (...args: any[]) => T),
    perspective: PerspectiveProxy,
    id: string,
    data: Record<string, any>,
  ): Promise<T> {
    const instance = new this(perspective, id) as T;
    if (!(await instance.get())) {
      throw new Error(`${this.getModelMetadata().className} not found: ${id}`);
    }
    Object.assign(instance, data);
    await instance.save();
    return instance;
  }

  /**
   * Deletes an existing model instance identified by `id`.
   *
   * Also removes every incoming link that points to this instance.
   *
   * @param perspective - The perspective containing the instance
   * @param id - The expression URI of the instance to delete
   * @throws Will throw if removing the instance's links or its incoming links fails
   *
   * @example
   * ```typescript
   * await Recipe.delete(perspective, recipeId);
   * ```
   */
  static async delete(
    this: typeof Ad4mModel & (new (...args: any[]) => Ad4mModel),
    perspective: PerspectiveProxy,
    id: string,
  ): Promise<void> {
    const instance = new this(perspective, id);
    await instance.delete();
  }

  /**
   * Registers this model's SHACL schema on the given perspective.
   *
   * This ensures the perspective knows about the model's shape
   * (properties, relations, constraints) so instances can be
   * created, queried, and validated.
   *
   * @param perspective - The perspective to register the model on
   *
   * @example
   * ```typescript
   * await Recipe.register(perspective);
   * // Now you can create / query Recipe instances on this perspective
   * ```
   */
  static async register(
    this: typeof Ad4mModel,
    perspective: PerspectiveProxy,
  ): Promise<RegisterResult> {
    return Ad4mModel.registerAll(perspective, [this]);
  }

  /**
   * Registers model classes on a perspective in one call. A class whose
   * stored shape already is its shape is `unchanged` and nothing is written,
   * so registering on every start is cheap; built-in classes (`TextContent`)
   * live in the executor and are skipped.
   *
   * @param perspective - The perspective to register the models on
   * @param models - Array of Ad4mModel subclasses to register
   *
   * @example
   * ```typescript
   * await Ad4mModel.registerAll(perspective, [Community, Channel, Message, Task]);
   * ```
   */
  static async registerAll(
    perspective: PerspectiveProxy,
    models: (typeof Ad4mModel)[],
  ): Promise<RegisterResult> {
    const shapes = models
      .filter(model => !Object.prototype.hasOwnProperty.call(model, 'builtinUri'))
      .map(model => {
        const { shape } = model.generateSHACL();
        if (!shape) throw new Error(`Class ${model.name} has no shape: declare it with @Model.`);
        return shape as SHACLShape;
      });
    return perspective.registerShapes(shapes);
  }

  /**
   * Executes a set of model operations inside a single batch (transaction).
   *
   * All `save`, `update`, and `delete` calls made via the provided `batchId`
   * are queued as ops and sent as one `perspective.mutate` call, then
   * committed atomically, when the callback completes. The ops see each
   * other (a `save()` of a new instance, then an update of it). A refused op
   * fails the call, so the validation errors surface when the callback
   * returns, not at the `save()`. If the callback throws, nothing is sent,
   * the batch is discarded, and the error is rethrown: no partial writes.
   * The `batchId` also takes raw link writes (`perspective.add(link, 'shared',
   * tx.batchId)`), which stage at once.
   *
   * @param perspective - The perspective to operate on
   * @param fn - Async callback that receives a `TransactionContext` object.
   *             Pass `tx.batchId` to `save(tx.batchId)`, `update(tx.batchId)`,
   *             `delete(tx.batchId)`, etc.
   * @returns The value returned by `fn`
   *
   * @example
   * ```typescript
   * await Ad4mModel.transaction(perspective, async (tx) => {
   *   const recipe = new Recipe(perspective);
   *   recipe.name = "Spaghetti";
   *   await recipe.save(tx.batchId);
   *
   *   const old = await Recipe.findAll(perspective, { where: { name: "Stale" } });
   *   for (const r of old) await r.delete(tx.batchId);
   * });
   * // All changes committed atomically here
   * ```
   */
  static async transaction<R = void>(
    perspective: PerspectiveProxy,
    fn: (tx: { batchId: string }) => Promise<R>,
  ): Promise<R> {
    const batchId = await perspective.createBatch();
    const queue = { perspective, ops: [] as MutateOp[], undo: [] as (() => void)[], done: [] as (() => void)[] };
    transactionOps.set(batchId, queue);
    try {
      const result = await fn({ batchId });
      // The model writes of the callback go out as one call, so a later op
      // finds an instance an earlier one created, and a refused op fails all.
      transactionOps.delete(batchId);
      if (queue.ops.length > 0) await perspective.mutate(queue.ops, { batch: batchId });
      await perspective.commitBatch(batchId);
      for (const done of queue.done) done();
      return result;
    } catch (e) {
      // Nothing was written: the instances saved in the callback are unsaved again.
      for (const undo of queue.undo.reverse()) undo();
      // A failed discard must not hide the original error.
      await perspective.discardBatch(batchId).catch(() => {});
      throw e;
    } finally {
      transactionOps.delete(batchId);
    }
  }

  /**
   * Creates an Ad4mModel class from a JSON Schema definition.
   * 
   * @description
   * This method dynamically generates an Ad4mModel subclass from a JSON Schema,
   * enabling integration with systems that use JSON Schema for type definitions.
   * 
   * The method follows a cascading approach for determining predicates:
   * 1. Explicit configuration in options parameter (highest precedence)
   * 2. x-ad4m metadata in the JSON Schema
   * 3. Inference from schema title and property names
   * 4. Error if no namespace can be determined
   * 
   * @example
   * ```typescript
   * // With explicit configuration
   * const PersonClass = Ad4mModel.fromJSONSchema(schema, {
   *   name: "Person",
   *   namespace: "person://"
   * });
   * 
   * // With property mapping
   * const ContactClass = Ad4mModel.fromJSONSchema(schema, {
   *   name: "Contact",
   *   namespace: "contact://",
   *   propertyMapping: {
   *     "name": "foaf://name",
   *     "email": "foaf://mbox"
   *   }
   * });
   * 
   * // With x-ad4m metadata in schema
   * const schema = {
   *   "title": "Product",
   *   "x-ad4m": { "namespace": "product://" },
   *   "properties": {
   *     "name": { 
   *       "type": "string",
   *       "x-ad4m": { "through": "product://title" }
   *     }
   *   }
   * };
   * const ProductClass = Ad4mModel.fromJSONSchema(schema, { name: "Product" });
   * ```
   * 
   * @param schema - JSON Schema definition
   * @param options - Configuration options
   * @returns Generated Ad4mModel subclass
   * @throws Error when namespace cannot be inferred
   */
  static fromJSONSchema(
    schema: JSONSchema,
    options: JSONSchemaToModelOptions
  ): typeof Ad4mModel {
    return buildModelFromJSONSchema(this, schema, options);
  }

  /**
   * Creates a fully functional Ad4mModel subclass from a SHACL shape.
   *
   * Unlike `fromJSONSchema()`, no predicate inference is required —
   * `SHACLShape` already contains the exact predicate URI in `path` for
   * every property. The method reads each property's `path`, `maxCount`,
   * `writable`, and `resolveLanguage` directly and writes them to the
   * WeakMap metadata registries.
   *
   * Properties with `hasValue` (flag / type-discrimination markers) are
   * registered as hidden flag entries so that the SPARQL query builder emits
   * the fixed triple `?source <predicate> <value>` needed for type discrimination.
   * Properties without a `name` field are skipped.
   *
   * **Backward-compat:** Old Flux SHACL shapes (created before `sh:hasValue`
   * was persisted on property shapes) carry the flag value only in
   * `shape.constructor_actions` as the `target` of an `addLink` action.
   * This method recovers those values so that old perspectives are queried
   * with the correct type-discriminator triple.
   *
   * @param shape - SHACL node shape (as returned by `PerspectiveProxy.shapes()`)
   * @param name  - Class name to assign (e.g. "Channel")
   * @returns Generated Ad4mModel subclass, ready for querying
   */
  /**
   * Synthesise an `Ad4mModel` subclass from a SHACL shape.
   *
   * @param shape           - The SHACL shape to synthesise from.
   * @param name            - The model name (e.g. "Channel").
   * @param classResolver   - Optional thunk factory.  When provided, any collection
   *   property that carries a `sh:class` URI will have its `target` wired up lazily:
   *   `target: () => classResolver(localName)`.  Because `target` is only called at
   *   query time (inside `enrichShapeForIncludes` / `jsonToModelInstance`), the
   *   resolver just needs to return the correct class by the time a query runs —
   *   making it safe to pass a closure over a registry object that is still being
   *   populated (e.g. the `result` record inside `getModelClasses`).
   */
  static fromSHACL(
    shape: SHACLShape,
    name: string,
    classResolver?: (localName: string) => typeof Ad4mModel | undefined,
  ): typeof Ad4mModel {
    const DynamicModelClass = class extends (this as any) {} as unknown as typeof Ad4mModel;
    (DynamicModelClass as any).className = name;
    (DynamicModelClass.prototype as any).className = name;

    // Build a backward-compat fallback map: predicate → fixed-IRI-value from
    // constructor actions. Old SHACL stores (created before sh:hasValue was
    // persisted on the property shape) don't carry sh:hasValue on properties,
    // but the shape-level constructor_actions always contain an addLink action
    // with the fixed flag value as the target.
    const flagValueFromConstructor = new Map<string, string>();
    for (const action of shape.constructor_actions ?? []) {
      if (
        action.action === 'addLink' &&
        typeof action.predicate === 'string' &&
        typeof action.target === 'string' &&
        action.target !== 'value' &&
        !action.target.startsWith('literal:')
      ) {
        flagValueFromConstructor.set(action.predicate, action.target);
      }
    }

    for (const prop of shape.properties) {
      const resolvedHasValue = prop.hasValue ?? flagValueFromConstructor.get(prop.path);

      if (resolvedHasValue !== undefined) {
        const flagKey = prop.name ?? `_flag_${prop.path.replace(/[^a-zA-Z0-9_]/g, '_')}`;
        setPropertyRegistryEntry(DynamicModelClass, flagKey, {
          through: prop.path,
          required: true,
          initial: resolvedHasValue,
          flag: true,
          writable: false,
          readOnly: true,
        });
        continue;
      }

      // Skip properties without a declared name
      if (!prop.name) continue;

      const isCollection = prop.maxCount === undefined || prop.maxCount > 1;

      if (isCollection) {
        // Derive a lazy `target` thunk when a sh:class URI is present and a
        // classResolver was supplied.  The thunk is evaluated at query time so
        // it is safe even if the target class hasn't been registered yet.
        let targetThunk: (() => typeof Ad4mModel) | undefined;
        if (prop.class && classResolver) {
          const sep = Math.max(prop.class.lastIndexOf('#'), prop.class.lastIndexOf('/'));
          const localName = sep >= 0 ? prop.class.slice(sep + 1) : prop.class;
          targetThunk = () => classResolver(localName) as typeof Ad4mModel;
        }

        setRelationRegistryEntry(DynamicModelClass, prop.name, {
          predicate: prop.path,
          kind: 'hasMany',
          ...(targetThunk !== undefined && { target: targetThunk }),
          ...(prop.local !== undefined && { local: prop.local }),
          ...(prop.getter !== undefined && { getter: prop.getter }),
        });
      } else {
        setPropertyRegistryEntry(DynamicModelClass, prop.name, {
          through: prop.path,
          ...(prop.minCount !== undefined && prop.minCount >= 1 && { required: true }),
          writable: prop.writable ?? true,
          ...(prop.resolveLanguage !== undefined && { resolveLanguage: prop.resolveLanguage }),
          ...(prop.local !== undefined && { local: prop.local }),
        });
      }
    }

    // The class keeps the shape's target class, which the executor
    // addresses it by: its namespace is everything before the local name.
    const namespace = namespaceOf(shape.targetClass);
    const ModelDecorator = Model({ name, ...(namespace && { namespace }) });
    ModelDecorator(DynamicModelClass);

    return DynamicModelClass as typeof Ad4mModel;
  }
}

