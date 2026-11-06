/**
 * The SDK's AgentReplyRecord mirrors the class the executor registers and writes
 * (`hardwired_sdna/agent_reply.json`).
 */
import * as fs from "fs";
import * as path from "path";
import { AgentReplyRecord, AgentSessionRecord, AgentStepRecord } from "./AgentReply";

const sdna = (file: string) =>
  JSON.parse(
    fs.readFileSync(path.resolve(__dirname, "../../../rust-executor/src/perspectives/hardwired_sdna", file), "utf-8"),
  );
const SDNA = sdna("agent_reply.json");
const STEP_SDNA = sdna("agent_step.json");
const SESSION_SDNA = sdna("agent_session.json");

describe("AgentReplyRecord", () => {
  it("matches the executor's class", () => {
    const { shape } = (AgentReplyRecord as any).generateSHACL();
    expect(shape.targetClass).toBe(SDNA.target_class);
    expect(shape.properties.map((p: any) => [p.path, p.name])).toEqual(
      SDNA.properties.map((p: any) => [p.path, p.name]),
    );
  });

  it("AgentStepRecord matches the executor's step class", () => {
    const { shape } = (AgentStepRecord as any).generateSHACL();
    expect(shape.targetClass).toBe(STEP_SDNA.target_class);
    expect(shape.properties.map((p: any) => [p.path, p.name])).toEqual(
      STEP_SDNA.properties.map((p: any) => [p.path, p.name]),
    );
  });

  it("AgentSessionRecord matches the executor's session class, settings local", () => {
    const { shape } = (AgentSessionRecord as any).generateSHACL();
    expect(shape.targetClass).toBe(SESSION_SDNA.target_class);
    expect(shape.properties.map((p: any) => [p.path, p.name, !!p.local])).toEqual(
      SESSION_SDNA.properties.map((p: any) => [p.path, p.name, !!p.local]),
    );
  });

  it("is a TextContent: it has a property named text", () => {
    const { shape } = (AgentReplyRecord as any).generateSHACL();
    expect(shape.properties.map((p: any) => p.name)).toContain("text");
  });
});
