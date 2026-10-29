import { Ad4mModel } from "../model/Ad4mModel";
import { HasMany, Model } from "../model/decorators";

/**
 * The built-in abstract class `Mentioning { mentions }`: a record that
 * mentions agents — a post, a message, a block of a document. `mentions` is
 * a relation to their DIDs.
 *
 * Querying it reads every class that has a relation named `mentions`, or one
 * aliased to `mentions`, whether or not the class has text. WE's blocks name
 * theirs `mentions` (over `we://mention`) and conform as they are, a post's
 * root included:
 *
 * ```typescript
 * @Model({ name: "Note", implements: [Mentioning] })
 * class Note extends Ad4mModel {
 *   @HasMany({ through: "app://cc", alias: { Mentioning: "mentions" } })
 *   cc: string[] = [];
 * }
 *
 * const mentioningMe = await Mentioning.findAll(perspective, { where: { mentions: myDid } });
 * ```
 *
 * The waker's mention subscription (`get_mention_waker_config`) reads it: a
 * record whose `mentions` links the agent's DID, in a link another agent
 * wrote, wakes the agent whatever its text says. Names in text it reads
 * through {@link TextContent}.
 *
 * Built in: the executor declares it (mirrors
 * `rust-executor/src/perspectives/hardwired_sdna/mentioning.json`) and reads
 * it by its URI, `ad4m://Mentioning`, so a perspective never stores it and an
 * app class named `Mentioning` does not replace it. `register()` on it writes
 * nothing.
 */
@Model({ name: "Mentioning", abstract: true })
export class Mentioning extends Ad4mModel {
  /** The URI the executor reads it by. */
  static readonly builtinUri = "ad4m://Mentioning";

  /** The agents (DIDs) the record mentions. */
  @HasMany({ through: "ad4m://mentions" })
  mentions: string[] = [];
}
