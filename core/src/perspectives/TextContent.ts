import { Ad4mModel } from "../model/Ad4mModel";
import { HasMany, Model, Property } from "../model/decorators";

/**
 * The built-in abstract class `TextContent { text, mentions? }` (#1446): text
 * a view or an LLM can read — a message, a note, a block of a document — and
 * the agents it mentions. `text` may contain HTML or Markdown; `mentions` is
 * a relation to agent DIDs.
 *
 * `text` is required (`required: true`, `sh:minCount 1`), which leaves
 * `mentions` optional: an abstract class that marks some names required asks
 * only for those. Querying it reads every class that has `text`, its own or
 * aliased. A class that also has a relation named or aliased `mentions` is
 * read through it; one without reads as mentioning no one (`mentions: []`,
 * and a `where` on `mentions` matches none of its rows).
 *
 * ```typescript
 * @Model({ name: "TextBlock", implements: [TextContent] })
 * class TextBlock extends Ad4mModel {
 *   @Property({ through: "we://text", alias: { TextContent: "text" } })
 *   body: string = "";
 *
 *   @HasMany({ through: "we://mention", alias: { TextContent: "mentions" } })
 *   mentionedAgents: string[] = [];
 * }
 *
 * @Model({ name: "Message", implements: [TextContent] })
 * class Message extends Ad4mModel {
 *   @Property({ through: "flux://body", alias: { TextContent: "text" } })
 *   body: string = "";
 * }
 *
 * const everything = await TextContent.findAll(perspective, { parent: { id: channel } });
 * ```
 *
 * The waker's mention subscription (`get_mention_waker_config`) is one query
 * over it: a record whose `mentions` links the agent's DID wakes the agent
 * whatever its text says, and one whose `text` holds the agent's name or DID
 * as a whole word wakes it too, in a link another agent wrote. A class
 * without a mention relation wakes it through its text.
 *
 * Built in: the executor declares it (mirrors
 * `rust-executor/src/perspectives/hardwired_sdna/text_content.json`) and
 * reads it by its URI, `ad4m://TextContent`, so a perspective never stores it
 * and an app class named `TextContent` does not replace it. `register()` on
 * it writes nothing.
 */
@Model({ name: "TextContent", abstract: true })
export class TextContent extends Ad4mModel {
  /** The URI the executor reads it by. */
  static readonly builtinUri = "ad4m://TextContent";

  /** The text. May contain HTML or Markdown. Required. */
  @Property({ through: "ad4m://text", required: true })
  text: string = "";

  /** The agents (DIDs) the text mentions. Optional: `[]` for a class without it. */
  @HasMany({ through: "ad4m://mentions" })
  mentions: string[] = [];
}
