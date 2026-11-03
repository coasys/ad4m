/**
 * The executor's agent replies, as a model: what `perspective.agentReply` writes into a
 * conversation.
 *
 * A conversation is any node that holds messages (the built-in {@link Container}), and its
 * messages are its children that carry text ({@link TextContent}), whatever class the app writes
 * them in. The executor writes its own turns as `AgentReply` records under the container. A
 * reply is a `TextContent` too (its `text`), so a reader of the conversation sees it as a message;
 * `status` says whether the agent is still working on it.
 *
 * Mirrors `rust-executor/src/perspectives/hardwired_sdna/agent_reply.json`, which the executor
 * registers before it writes the first reply. The executor writes these; an app reads them, and
 * follows a running one with a subscription:
 *
 * ```typescript
 * const { replyId } = await perspective.agentReply(chat, { workIn: perspective.uuid });
 * AgentReplyRecord.query(perspective).where({ id: replyId }).subscribe((replies) => { ... });
 * ```
 *
 * Named `AgentReplyRecord` because `AgentReply` is already the SDK's name for what
 * `perspective.runAgent` returns; the class in the graph is `ad4m://AgentReply`.
 */

import { Ad4mModel } from "../model/Ad4mModel";
import { Model, Property } from "../model/decorators";

/** Where a reply is. Only `running` changes. */
export type AgentReplyStatus = "running" | "done" | "failed" | "cancelled" | "interrupted";

@Model({ name: "AgentReply" })
export class AgentReplyRecord extends Ad4mModel {
  /**
   * `running` from the moment the reply exists; then `done`, `failed` (see {@link error}),
   * `cancelled`, or `interrupted` (the executor restarted while it ran).
   *
   * First-declared, so the shape's namespace comes from its `through` (`ad4m://`) and the target
   * class is `ad4m://AgentReply`.
   */
  @Property({ through: "ad4m://agent_reply/status", required: true })
  status: string = "";

  /** What the agent answered, once it is `done`. */
  @Property({ through: "ad4m://agent_reply/text" })
  text: string = "";

  /** Why it `failed`. */
  @Property({ through: "ad4m://agent_reply/error" })
  error: string = "";
}
