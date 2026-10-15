import { Ad4mModel } from "../model/Ad4mModel";
import { HasMany, Model } from "../model/decorators";

/**
 * The built-in abstract class `Container { children }` (#1446): what holds
 * other records — a channel, a thread, a post, a call, a document, a board.
 *
 * X is contained in P when `P ad4m://has_child X` exists, whatever P is (an
 * instance of any class, or a node no class describes, such as
 * `ad4m://self`), or when P is an instance of a class whose `children`
 * relation (by name, or aliased to `children`) links X:
 *
 * ```typescript
 * @Model({ name: "Block" })
 * class Block extends Ad4mModel {
 *   @HasMany({ through: "we://child", alias: { Container: "children" } })
 *   blocks: string[] = [];
 * }
 *
 * // Messages held by P, through has_child or a Block's we://child:
 * await Message.findAll(perspective, { parent: { id: P } });
 * // Everything that holds X:
 * await Container.findAll(perspective, { parent: { ids: [X], direction: "in" } });
 * ```
 *
 * A row's `children` is everything it holds, merged across its classes. A
 * node no class describes is a row of its own when it holds something by
 * `has_child`. Always scope a `Container` query (`where: { id }`, a
 * `children` filter or `parent`): unscoped, it reads every holder in the
 * perspective.
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

  /** What it holds, whatever their class. */
  @HasMany({ through: "ad4m://has_child" })
  children: string[] = [];
}
