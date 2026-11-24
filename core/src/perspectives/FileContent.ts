import { Ad4mModel } from "../model/Ad4mModel";
import { Model, Property } from "../model/decorators";

/**
 * The built-in abstract class `FileContent { name, mimeType, size, url }`: a
 * file, such as an attachment to a message, a post or a note. `url` is the
 * address its bytes are stored under.
 *
 * Querying it reads every class that has all four names, as properties or
 * aliased to them, under whatever predicates the app chose:
 *
 * ```typescript
 * // The files a message holds, whatever the app calls its file class:
 * const files = await FileContent.findAll(perspective, { parent: { id: message } });
 * ```
 *
 * Built in: the executor declares it (mirrors
 * `rust-executor/src/perspectives/hardwired_sdna/file_content.json`) and
 * reads it by its URI, `ad4m://FileContent`, so a perspective never stores it
 * and an app class named `FileContent` does not replace it. `register()` on
 * it writes nothing.
 *
 * The waker lists the files a mentioned record holds through it
 * (`MentionMessage.attachments`).
 */
@Model({ name: "FileContent", abstract: true })
export class FileContent extends Ad4mModel {
  /** The URI the executor reads it by. */
  static readonly builtinUri = "ad4m://FileContent";

  @Property({ through: "ad4m://file_name" })
  name: string = "";

  @Property({ through: "ad4m://file_mime_type" })
  mimeType: string = "";

  /** Bytes. */
  @Property({ through: "ad4m://file_size" })
  size: number = 0;

  /** The address of the stored bytes. */
  @Property({ through: "ad4m://file_url" })
  url: string = "";
}
