/**
 * perspective.agentReply over the wire: a reply is created under the conversation at once, runs
 * on the node, and is followed with an ordinary query subscription on `AgentReplyRecord`.
 *
 * No model is needed. A reply that names a model the node does not have fails, and the failure
 * is a status change a subscription must report: the reply is created `running` in one commit and
 * set to `failed` in another. The lifecycle with a working model (done, cancel, one at a time,
 * the sweep at load) is covered by `rust-executor/src/perspectives/agent_reply_tests.rs`.
 */

import { expect } from "chai";
import {
  Ad4mClient, Ad4mModel, AgentReplyRecord, AgentSessionRecord, Link, LinkQuery, Model, PerspectiveProxy,
  Property,
} from "@coasys/ad4m";
import { getSharedAgent } from "./hooks.js";
import { startAgent, waitUntil } from "../../helpers/index.js";

/** An app's message class: it has `text`, so it is a TextContent. */
@Model({ name: "ReplyTestLine" })
class ReplyTestLine extends Ad4mModel {
  @Property({ through: "replytest://line_text", required: true })
  text: string = "";
}

const CHAT = "replytest://chat/1";
const MISSING_MODEL = "no-such-model-for-agent-reply";

describe("perspective.agentReply", function () {
  this.timeout(120_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const shared = getSharedAgent();
    if (shared) {
      ad4m = shared.client;
    } else {
      const agent = await startAgent("agent-reply");
      ad4m = agent.client;
      stopAgent = agent.stop;
    }
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    const handle = await ad4m.perspective.add("agent-reply-test");
    p = (await ad4m.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await ReplyTestLine.register(p);
  });

  async function say(text: string): Promise<string> {
    const line = new ReplyTestLine(p);
    line.text = text;
    await line.save();
    await p.add(new Link({ source: CHAT, predicate: "ad4m://has_child", target: line.id }));
    return line.id;
  }

  async function replyTo(text: string): Promise<string> {
    await say(text);
    const { replyId } = await p.agentReply(CHAT, { workIn: p.uuid, model: MISSING_MODEL });
    return replyId;
  }

  it("refuses a conversation with nothing to answer, and writes no reply", async () => {
    let refused: unknown;
    try {
      await p.agentReply(CHAT, { workIn: p.uuid, model: MISSING_MODEL });
    } catch (e) {
      refused = e;
    }
    expect(String(refused)).to.contain("nothing to answer");
    expect(await AgentReplyRecord.findAll(p)).to.deep.equal([]);
  });

  it("creates the reply under the conversation, and a subscription sees how it ends", async () => {
    // The first reply registers AgentReply in the perspective.
    const first = await replyTo("hello");
    await waitUntil(
      async () => (await AgentReplyRecord.findAll(p)).some((r) => r.id === first && r.status === "failed"),
      30_000,
      "the first reply fails",
    );

    const seen: AgentReplyRecord[][] = [];
    const builder = AgentReplyRecord.query(p);
    await builder.subscribe((replies) => seen.push(replies));

    const second = await replyTo("and again");
    await waitUntil(
      () => seen.some((batch) => batch.some((r) => r.id === second && r.status === "failed")),
      30_000,
      "the subscription reports the second reply as failed",
    );
    builder.dispose();

    const [reply] = (await AgentReplyRecord.findAll(p, { include: { steps: true } })).filter(
      (r) => r.id === second,
    );
    expect(reply.error, "the reply says why it failed").to.not.equal("");
    expect(reply.steps, "no tool was called, so no steps").to.deep.equal([]);

    // The reply is a message of the conversation, a child of the container.
    const children = await p.get(new LinkQuery({ source: CHAT, predicate: "ad4m://has_child" }));
    expect(children.map((l) => l.data.target)).to.include(second);
  });

  it("replies in a session with its settings, lists it by what it is about, and deletes it", async () => {
    const about = "replytest://template/1";
    const { sessionId } = await p.agentSessionCreate({
      title: "Chat 1",
      about,
      workIn: p.uuid,
      model: MISSING_MODEL,
    });
    const listed = await AgentSessionRecord.findAll(p, { where: { about } });
    expect(listed.map((s) => [s.id, s.title])).to.deep.equal([[sessionId, "Chat 1"]]);

    const line = new ReplyTestLine(p);
    line.text = "hello";
    await line.save();
    await p.add(new Link({ source: sessionId, predicate: "ad4m://has_child", target: line.id }));
    // No settings sent: the session's are used.
    const { replyId } = await p.agentReply(sessionId);
    await waitUntil(
      async () => (await AgentReplyRecord.findAll(p)).some((r) => r.id === replyId && r.status === "failed"),
      30_000,
      "the session's reply fails on the session's model",
    );
    const [reply] = (await AgentReplyRecord.findAll(p)).filter((r) => r.id === replyId);
    expect(reply.error, "it ran with the session's model, not the node's default").to.not.contain(
      "no model was named",
    );

    expect(await p.agentSessionDelete(sessionId)).to.equal(true);
    expect(await AgentSessionRecord.findAll(p, { where: { about } })).to.deep.equal([]);
    expect((await AgentReplyRecord.findAll(p)).map((r) => r.id)).to.not.include(replyId);
    expect(await p.agentSessionDelete(sessionId), "already gone").to.equal(false);
  });

  it("answers false to a cancel with nothing running", async () => {
    expect(await p.agentReplyCancel(CHAT)).to.equal(false);
  });
});
