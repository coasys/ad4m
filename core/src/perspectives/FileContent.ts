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
 *
 * Its own predicates are W3C terms: `rdfs:label` for the name, and DCAT's
 * `dcat:mediaType`, `dcat:byteSize` and `dcat:downloadURL`.
 */
@Model({ name: "FileContent", namespace: "ad4m://", abstract: true })
export class FileContent extends Ad4mModel {
  /** The URI the executor reads it by. */
  static readonly builtinUri = "ad4m://FileContent";

  @Property({ through: "http://www.w3.org/2000/01/rdf-schema#label" })
  name: string = "";

  @Property({ through: "http://www.w3.org/ns/dcat#mediaType" })
  mimeType: string = "";

  /** Bytes. */
  @Property({ through: "http://www.w3.org/ns/dcat#byteSize" })
  size: number = 0;

  /** The address of the stored bytes. */
  @Property({ through: "http://www.w3.org/ns/dcat#downloadURL" })
  url: string = "";
}
