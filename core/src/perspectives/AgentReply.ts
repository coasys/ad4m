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
import { HasMany, Model, Property, ReadOnly } from "../model/decorators";

/** Where a reply is. Only `running` changes. */
export type AgentReplyStatus = "running" | "done" | "failed" | "cancelled" | "interrupted";

/** How a step went. A step left `running` under a reply that is no longer running was cut off. */
export type AgentStepOutcome = "running" | "ok" | "error";

/**
 * One tool call an agent made while writing a reply, written as it made it: the tool, the
 * record it acted on, and how it went. Never the call's arguments or result. Mirrors
 * `hardwired_sdna/agent_step.json`.
 */
@Model({ name: "AgentStep" })
export class AgentStepRecord extends Ad4mModel {
  /** The tool the agent called (`template_read`, `instance_update`, …). First-declared, for the namespace. */
  @Property({ through: "ad4m://agent_step/tool", required: true })
  tool: string = "";

  /** The record it acted on, by id, when the call names one. */
  @Property({ through: "ad4m://agent_step/target" })
  target: string = "";

  /** `running` until the call returns, then `ok` or `error`. */
  @Property({ through: "ad4m://agent_step/outcome" })
  outcome: string = "";

  /** The tool's message when it failed, capped. */
  @Property({ through: "ad4m://agent_step/error" })
  error: string = "";
}

@Model({ name: "AgentReply" })
export class AgentReplyRecord extends Ad4mModel {
  /**
   * The id of the message the reply answers. Written once, when the reply is created, so the
   * reply's `timestamp` (its earliest link) is when it started, not when it ended: a reply keeps
   * its place in the conversation.
   *
   * First-declared, so the shape's namespace comes from its `through` (`ad4m://`) and the target
   * class is `ad4m://AgentReply`.
   */
  @ReadOnly({ through: "ad4m://agent_reply/answers", required: true })
  answers: string = "";

  /**
   * `running` from the moment the reply exists; then `done`, `failed` (see {@link error}),
   * `cancelled`, or `interrupted` (the executor restarted while it ran).
   */
  @Property({ through: "ad4m://agent_reply/status", required: true })
  status: AgentReplyStatus = "running";

  /** What the agent answered, once it is `done`. */
  @Property({ through: "ad4m://agent_reply/text" })
  text: string = "";

  /** Why it `failed`. */
  @Property({ through: "ad4m://agent_reply/error" })
  error: string = "";

  /**
   * Its tool calls, as they happen. Read them with `include: { steps: true }` (oldest first by
   * `createdAt`); a subscription with that include follows new steps and their outcomes.
   *
   * In a shared perspective another member can add a step to a reply or rewrite one's `outcome`.
   * A view that shows steps should keep only those the reply's author wrote.
   */
  @HasMany(() => AgentStepRecord, { through: "ad4m://agent_step" })
  steps: AgentStepRecord[] = [];
}

/**
 * A conversation the executor owns, for a client with no conversation of its own to point
 * `perspective.agentReply` at. Mirrors `hardwired_sdna/agent_session.json`.
 *
 * A session is the `container` its replies are written under: write the person's messages as
 * its children (`ad4m://has_child`), then reply in it without sending the settings again. List a
 * perspective's sessions about a record with `AgentSessionRecord.findAll(p, { where: { about } })`.
 * Create and delete them with `perspective.agentSessionCreate` / `agentSessionDelete`: the
 * executor writes these, an app does not.
 *
 * `workIn`, `writeTools` and `model` are kept on the node that created the session only (local
 * links): `workIn` is a perspective uuid, which means nothing to another node.
 */
@Model({ name: "AgentSession" })
export class AgentSessionRecord extends Ad4mModel {
  /** First-declared, for the namespace (`ad4m://`). */
  @Property({ through: "ad4m://agent_session/title", required: true })
  title: string = "";

  /** The record the session is about, by id. */
  @Property({ through: "ad4m://agent_session/about" })
  about: string = "";

  /** The perspective its replies' agent works in. Local to the node. */
  @Property({ through: "ad4m://agent_session/work_in", local: true })
  workIn: string = "";

  /** The tools its replies' agent may write with, as a JSON array. Local to the node. */
  @Property({ through: "ad4m://agent_session/write_tools", local: true })
  writeTools: string = "";

  /** The model its replies use. Local to the node. */
  @Property({ through: "ad4m://agent_session/model", local: true })
  model: string = "";
}
