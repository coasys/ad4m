/**
 * The SDK's AgentReplyRecord mirrors the class the executor registers and writes
 * (`hardwired_sdna/agent_reply.json`).
 */
import * as fs from "fs";
import * as path from "path";
import { AgentReplyRecord } from "./AgentReply";

const SDNA = JSON.parse(
  fs.readFileSync(
    path.resolve(__dirname, "../../../rust-executor/src/perspectives/hardwired_sdna/agent_reply.json"),
    "utf-8",
  ),
);

describe("AgentReplyRecord", () => {
  it("matches the executor's class", () => {
    const { shape } = (AgentReplyRecord as any).generateSHACL();
    expect(shape.targetClass).toBe(SDNA.target_class);
    expect(shape.properties.map((p: any) => [p.path, p.name])).toEqual(
      SDNA.properties.map((p: any) => [p.path, p.name]),
    );
  });

  it("is a TextContent: it has a property named text", () => {
    const { shape } = (AgentReplyRecord as any).generateSHACL();
    expect(shape.properties.map((p: any) => p.name)).toContain("text");
  });
});
