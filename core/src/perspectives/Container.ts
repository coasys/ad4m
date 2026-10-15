import { Ad4mModel } from "../model/Ad4mModel";
import { HasMany, Model } from "../model/decorators";

/**
 * The built-in abstract class `Container { children }` (#1446): what holds
 * other records — a channel, a post, a call, a document, a board.
 *
 * A class conforms with a `children` relation of its own (or one aliased to
 * `children`), whatever predicate it holds them through:
 *
 * ```typescript
 * @Model({ name: "CollectionBlock" })
 * class CollectionBlock extends Ad4mModel {
 *   @HasMany({ through: "we://child", polymorphic: true })
 *   children: string[] = [];
 * }
 * ```
 *
 * The executor follows those predicates, beside `ad4m://has_child`, wherever
 * it reads a node's children: the waker's parent walk, `get_children` and
 * `instance_transcript`.
 *
 * Built in: the executor declares it (mirrors
 * `rust-executor/src/perspectives/hardwired_sdna/container.json`) and reads
 * it by its URI, `ad4m://Container`, so a perspective never stores it and an
 * app class named `Container` does not replace it. `register()` on it writes
 * nothing.
 */
@Model({ name: "Container", abstract: true })
export class Container extends Ad4mModel {
  /** The URI the executor reads it by. */
  static readonly builtinUri = "ad4m://Container";

  /** What it holds. */
  @HasMany({ through: "ad4m://has_child" })
  children: string[] = [];
}
