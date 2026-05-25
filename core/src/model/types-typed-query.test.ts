/**
 * Type-level assertion tests for the TypedQuery / TypedWhere / TypedIncludeMap
 * surface added in PLAN_6.
 *
 * These tests verify the *compile-time* behaviour of the generic query types.
 * Runtime assertions are minimal — the real checks are the `// @ts-expect-error`
 * markers and the `expectAssignable` / `expectExact` helpers, all of which fail
 * loudly under `tsc --noEmit`.
 */

import { Ad4mModel } from "./Ad4mModel";
import { Model, Property, HasMany, BelongsToOne } from "./decorators";
import type {
  PropertyKeysOf, RelationKeysOf, RelatedModel,
  TypedQuery, TypedWhere, TypedOrder, TypedIncludeMap, TypedIncludeProjection,
  IncludeExtras, Query,
} from "./types";

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

@Model({ name: "Comment" })
class Comment extends Ad4mModel {
  @Property({ through: "comment://text" })
  text: string = "";

  @Property({ through: "comment://likes" })
  likes: number = 0;
}

@Model({ name: "Post" })
class Post extends Ad4mModel {
  @Property({ through: "post://title", required: true })
  title: string = "";

  @Property({ through: "post://body" })
  body: string = "";

  @Property({ through: "post://is_pinned" })
  isPinned: boolean = false;

  @Property({ through: "post://views" })
  views: number = 0;

  @HasMany(() => Comment, { through: "post://comment" })
  comments: Comment[] = [];

  @BelongsToOne({ through: "post://author_of" })
  authorRef: Comment | null = null;
}

// AgentProfile-like fixture: inheritance + optional HasOne relation.
// This mirrors `we/packages/models/src/entities/AgentProfile.ts` which
// previously surfaced a type bug in StrictTypedIncludeMap.
class WeNodeLike extends Ad4mModel {
  @HasMany({ through: "we://signal" })
  signals: string[] = [];
}

class LocationBlockLike extends WeNodeLike {
  @Property({ through: "loc://name" })
  name: string = "";
}

@Model({ name: "AgentProfileLike" })
class AgentProfileLike extends WeNodeLike {
  @Property({ through: "we://handle" })
  handle: string = "";

  @Property({ through: "we://avatar" })
  avatar?: string;

  // Optional HasOne — Ad4mModel | undefined. Was previously not picked up
  // by RelationKeysOf (which only checked `| null`), causing valid
  // `include: { location: true }` calls to be rejected at the type level.
  location?: LocationBlockLike;
}

// ---------------------------------------------------------------------------
// Type-level helpers
// ---------------------------------------------------------------------------

type Equals<X, Y> =
  (<T>() => T extends X ? 1 : 2) extends
  (<T>() => T extends Y ? 1 : 2) ? true : false;

function expectExact<T1, T2>(_: Equals<T1, T2> extends true ? true : never): void {}
function expectAssignable<T>(_: T): void {}

// ---------------------------------------------------------------------------
// PropertyKeysOf / RelationKeysOf
// ---------------------------------------------------------------------------

type PostProps = PropertyKeysOf<Post>;
type PostRels  = RelationKeysOf<Post>;

expectExact<PostProps, "title" | "body" | "isPinned" | "views">(true);
expectExact<PostRels,  "comments" | "authorRef">(true);

expectExact<RelatedModel<Post, "comments">, Comment>(true);
expectExact<RelatedModel<Post, "authorRef">, Comment>(true);

// AgentProfileLike — inheritance + optional HasOne via `?: T`
type AgentRels = RelationKeysOf<AgentProfileLike>;
type AgentProps = PropertyKeysOf<AgentProfileLike>;

// `location?: LocationBlockLike` (i.e. `LocationBlockLike | undefined`) must
// be recognised as a relation. `signals: string[]` inherited from WeNodeLike
// also counts (the `@HasMany` + `string[]` pattern).
expectAssignable<AgentRels>("location");
expectAssignable<AgentRels>("signals");
expectAssignable<AgentProps>("handle");
expectAssignable<AgentProps>("avatar");

expectAssignable<TypedIncludeMap<AgentProfileLike>>({ location: true });
expectAssignable<TypedIncludeMap<AgentProfileLike>>({ signals: true });

// ---------------------------------------------------------------------------
// TypedWhere
// ---------------------------------------------------------------------------

// Valid where shapes
expectAssignable<TypedWhere<Post>>({ title: "hello" });
expectAssignable<TypedWhere<Post>>({ title: { contains: "x" } });
expectAssignable<TypedWhere<Post>>({ views: { gt: 5, lte: 100 } });
expectAssignable<TypedWhere<Post>>({ isPinned: true });
expectAssignable<TypedWhere<Post>>({ id: "literal:foo" });
expectAssignable<TypedWhere<Post>>({ author: "did:key:alice" });

// Misspelled property — must error
// @ts-expect-error
expectAssignable<TypedWhere<Post>>({ titel: "hello" });

// Wrong value type — must error
// @ts-expect-error
expectAssignable<TypedWhere<Post>>({ isPinned: "yes" });

// Numeric op on string field — must error
// @ts-expect-error
expectAssignable<TypedWhere<Post>>({ title: { gt: 5 } });

// String op on numeric field — must error
// @ts-expect-error
expectAssignable<TypedWhere<Post>>({ views: { contains: "x" } });

// ---------------------------------------------------------------------------
// TypedOrder
// ---------------------------------------------------------------------------

expectAssignable<TypedOrder<Post>>({ title: "ASC" });
expectAssignable<TypedOrder<Post>>({ views: "DESC", timestamp: "ASC" });

// @ts-expect-error — unknown field
expectAssignable<TypedOrder<Post>>({ titel: "ASC" });

// @ts-expect-error — invalid direction
expectAssignable<TypedOrder<Post>>({ title: "ASCENDING" });

// ---------------------------------------------------------------------------
// TypedIncludeMap — eager-load + nested
// ---------------------------------------------------------------------------

expectAssignable<TypedIncludeMap<Post>>({ comments: true });
expectAssignable<TypedIncludeMap<Post>>({ authorRef: true });
expectAssignable<TypedIncludeMap<Post>>({
  comments: { where: { text: "x" }, order: { likes: "DESC" }, limit: 5 },
});

// Misspelled relation — must error
// @ts-expect-error
expectAssignable<TypedIncludeMap<Post>>({ commments: true });

// Nested where typo — must error
// @ts-expect-error
expectAssignable<TypedIncludeMap<Post>>({ comments: { where: { txet: "x" } } });

// Nested numeric op on string — must error
// @ts-expect-error
expectAssignable<TypedIncludeMap<Post>>({ comments: { where: { text: { gt: 5 } } } });

// ---------------------------------------------------------------------------
// TypedIncludeProjection ($-prefixed keys)
// ---------------------------------------------------------------------------

// Valid: count true projection
expectAssignable<TypedIncludeMap<Post>>({
  $commentCount: { from: "comments", count: true },
});

// Valid: filtered projection with limit
expectAssignable<TypedIncludeMap<Post>>({
  $firstComment: { from: "comments", where: { text: "hi" }, limit: 1 },
});

// `from` must reference a real relation
{
  const _bad1: TypedIncludeMap<Post> = {
    $bogus: {
      // @ts-expect-error — "notARelation" is not in RelationKeysOf<Post>
      from: "notARelation",
      count: true,
    },
  };
  void _bad1;
}

// Projection's nested where uses the *target's* property keys
expectAssignable<TypedIncludeMap<Post>>({
  $popular: { from: "comments", where: { likes: { gt: 100 } }, limit: 10 },
});

// Wrong nested where key on the target
{
  const _bad2: TypedIncludeMap<Post> = {
    $bad: {
      from: "comments",
      // @ts-expect-error — "titel" is not a property of Comment
      where: { titel: "x" },
    },
  };
  void _bad2;
}

// ---------------------------------------------------------------------------
// IncludeExtras — propagating $-keys into the row type
// ---------------------------------------------------------------------------

type CountProjection = { $commentCount: { from: "comments"; count: true } };
type Limit1Projection = { $firstComment: { from: "comments"; limit: 1 } };
type ListProjection = { $allComments: { from: "comments" } };

expectExact<IncludeExtras<Post, CountProjection>["$commentCount"], number>(true);
expectExact<IncludeExtras<Post, Limit1Projection>["$firstComment"], Comment | null>(true);
expectExact<IncludeExtras<Post, ListProjection>["$allComments"], Comment[]>(true);

// ---------------------------------------------------------------------------
// TypedQuery — full shape
// ---------------------------------------------------------------------------

expectAssignable<TypedQuery<Post>>({
  where: { isPinned: true, views: { gt: 10 } },
  order: { views: "DESC" },
  include: { comments: { limit: 5 }, $commentCount: { from: "comments", count: true } },
  includeAll: false,
  deepQuery: true,
  limit: 20,
  offset: 0,
  count: true,
  properties: ["title", "body"],
});

// @ts-expect-error — properties[] only accepts declared property names
expectAssignable<TypedQuery<Post>>({ properties: ["title", "nonsense"] });

// ---------------------------------------------------------------------------
// fromSHACL escape hatch — TypedQuery<Ad4mModel> collapses to loose Query
// ---------------------------------------------------------------------------

// When T has no declared fields (i.e. Ad4mModel itself), TypedQuery<T> ≡ Query.
expectExact<TypedQuery<Ad4mModel>, Query>(true);
expectAssignable<TypedQuery<Ad4mModel>>({ where: { anyKey: "value" } });

// ---------------------------------------------------------------------------
// End-to-end inference through findAll / findOne — verifies that the literal
// `count: true` propagates through `Q extends TypedQuery<T>` into the return
// type so callers don't need `(row as any).$xxx` casts.
// ---------------------------------------------------------------------------

import type { PerspectiveProxy } from "../perspectives/PerspectiveProxy";
declare const persp: PerspectiveProxy;

// Skip async actually-running them — these calls are unreachable at runtime
// (no perspective). The point is that they compile with correct return types.
function _typeFlowChecks() {
  return async () => {
    // No include — plain Post[]
    const a = await Post.findAll(persp);
    expectAssignable<Post[]>(a);

    // Include eager-loaded relation — still Post[]
    const b = await Post.findAll(persp, { include: { comments: true } });
    expectAssignable<Post[]>(b);

    // Projection: count → number on result rows
    const c = await Post.findAll(persp, {
      include: { $commentCount: { from: "comments", count: true } },
    });
    expectAssignable<number>(c[0].$commentCount);

    // Projection: limit 1 → scalar | null on result rows
    const d = await Post.findAll(persp, {
      include: { $top: { from: "comments", limit: 1 } },
    });
    expectAssignable<Comment | null>(d[0].$top);

    // Projection: no limit → array on result rows
    const e = await Post.findAll(persp, {
      include: { $all: { from: "comments" } },
    });
    expectAssignable<Comment[]>(e[0].$all);

    // findOne — same propagation, nullable wrapper
    const f = await Post.findOne(persp, {
      include: { $commentCount: { from: "comments", count: true } },
    });
    if (f) expectAssignable<number>(f.$commentCount);
  };
}

// ---------------------------------------------------------------------------
// Runtime placeholder — Jest requires at least one test in the file
// ---------------------------------------------------------------------------

describe("typed-query types (compile-time)", () => {
  it("compiles", () => {
    // All assertions above are at type level; this test passes if tsc passed.
    expect(true).toBe(true);
  });
});
