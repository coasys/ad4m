import { Ad4mModel } from "../model/Ad4mModel";
import { Model, Property } from "../model/decorators";

/**
 * The built-in abstract class `TextContent { text }` (#1446): text a view or
 * an LLM can read — a message, a note, a block of a document. `text` may
 * contain HTML or Markdown.
 *
 * Querying it reads every class that has a `text` property, or a property
 * aliased to `text`:
 *
 * ```typescript
 * @Model({ name: "Message", implements: [TextContent] })
 * class Message extends Ad4mModel {
 *   @Property({ through: "flux://body", alias: { TextContent: "text" } })
 *   body: string = "";
 * }
 *
 * const everything = await TextContent.findAll(perspective, { parent: { id: channel } });
 * ```
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

  /** The text. May contain HTML or Markdown. */
  @Property({ through: "ad4m://text" })
  text: string = "";
}
