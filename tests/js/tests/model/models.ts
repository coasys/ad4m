/**
 * Shared test model definitions for the Ad4mModel integration tests.
 *
 * These are direct ports of the models in:
 *   we/apps/playgrounds/react/ad4m-model-testing/src/models/
 *
 * Using the same predicates / decorators as the playground keeps the
 * two test environments in sync — a test that passes in the playground
 * should pass here too.
 */

import {
  Ad4mModel,
  BelongsToMany,
  BelongsToOne,
  Flag,
  HasMany,
  HasManyMethods,
  HasOne,
  Model,
  Property,
} from "@coasys/ad4m";

// ── TestComment (declared first to avoid circular-ref issues at class level) ──

@Model({ name: "TestComment" })
export class TestComment extends Ad4mModel {
  @Flag({ through: "test://comment_type", value: "test://comment" })
  type = "test://comment";

  @Property({ through: "test://body", required: true, writable: true })
  body: string = "";

  /** Reverse traversal — find the TestPost that has a test://has_comment link pointing to this */
  @BelongsToOne(() => TestPost, { through: "test://has_comment" })
  post: TestPost | null = null;

  /** Reverse traversal — find the TestPost that has this comment as its pinnedComment */
  @BelongsToOne(() => TestPost, { through: "test://pinned_comment" })
  pinnedBy: TestPost | null = null;
}

// ── TestTag ───────────────────────────────────────────────────────────────────

@Model({ name: "TestTag" })
export class TestTag extends Ad4mModel {
  @Flag({ through: "test://tag_type", value: "test://tag" })
  type = "test://tag";

  @Property({ through: "test://label", required: true, writable: true })
  label: string = "";

  /** Reverse traversal — all TestPosts that have a test://has_tag link pointing to this tag */
  @BelongsToMany(() => TestPost, { through: "test://has_tag" })
  posts: TestPost[] = [];
}

// ── TestPost ──────────────────────────────────────────────────────────────────

@Model({ name: "TestPost" })
export class TestPost extends Ad4mModel {
  @Flag({ through: "test://post_type", value: "test://post" })
  type = "test://post";

  @Property({ through: "test://title", required: true, writable: true })
  title: string = "";

  @Property({ through: "test://body", writable: true })
  body: string = "";

  @Property({ through: "test://view_count", writable: true })
  viewCount: number = 0;

  @HasMany(() => TestTag, { through: "test://has_tag" })
  tags: TestTag[] = [];

  @HasMany(() => TestComment, { through: "test://has_comment" })
  comments: TestComment[] = [];

  @HasOne(() => TestComment, { through: "test://pinned_comment" })
  pinnedComment: TestComment | null = null;
}
export interface TestPost extends HasManyMethods<
  "tags" | "comments" | "pinnedComment"
> {}

// ── TestBaseModel ─────────────────────────────────────────────────────────────

@Model({ name: "TestBaseModel" })
export class TestBaseModel extends Ad4mModel {
  /** No @Flag — all nodes with test://base_content qualify as base instances */
  @Property({ through: "test://base_content", writable: true })
  content: string = "";
}

// ── TestDerivedModel ──────────────────────────────────────────────────────────

@Model({ name: "TestDerivedModel" })
export class TestDerivedModel extends TestBaseModel {
  @Flag({ through: "test://poll_type", value: "test://poll_block" })
  pollType = "test://poll_block";

  @Property({ through: "test://poll_question", required: true, writable: true })
  question: string = "";
}
