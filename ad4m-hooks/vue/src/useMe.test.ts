import { describe, it, expect, beforeEach } from "vitest";
import { effectScope } from "vue";
import { Ad4mClient } from "@coasys/ad4m";
import { emit, installFakeExecutor, profileOf, rpcLog, until } from "../../test-support/fakeExecutor";
import { useMe } from "./useMe";
import { useAgent } from "./useAgent";

const nameOf = (links: any[]) => links[0].data.target as string;
const wire = (did: string, name: string) => profileOf(did, name);

beforeEach(installFakeExecutor);

describe("useMe", () => {
  it("shows the agent and re-renders when agent-updated replaces it", async () => {
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const scope = effectScope();
    const { me, profile, status } = scope.run(() => useMe(client, nameOf))!;

    await until(() => profile.value === "none");
    expect(status.value?.did).toBe("did:test:alice");

    emit("alice", { type: "agent-updated", agent: wire("did:test:alice", "Alice") });
    expect(me.value?.did).toBe("did:test:alice");
    expect(profile.value).toBe("Alice");
    expect(rpcLog.filter((c) => c.type === "agent.get")).toHaveLength(1);
    scope.stop();
    client.close();
  });

  it("two clients with two tokens share nothing", async () => {
    const alice = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const bob = new Ad4mClient("http://127.0.0.1:12000", "bob");
    const scope = effectScope();
    const [a, b] = scope.run(() => [useMe(alice, nameOf), useMe(bob, nameOf)])!;
    await until(() => a.me.value !== undefined && b.me.value !== undefined);
    expect(a.me.value?.did).toBe("did:test:alice");
    expect(b.me.value?.did).toBe("did:test:bob");

    emit("alice", { type: "agent-updated", agent: wire("did:test:alice", "Alice") });
    expect(a.profile.value).toBe("Alice");
    expect(b.profile.value).toBe("none");
    scope.stop();
    alice.close();
    bob.close();
  });
});

describe("useAgent", () => {
  it("shows another agent and re-renders when agent-updated refreshes it", async () => {
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const scope = effectScope();
    const { agent, profile } = scope.run(() => useAgent(client.agent, "did:test:friend", (links) => ({ name: nameOf(links) })))!;

    await until(() => agent.value !== null);
    expect(profile.value).toEqual({ name: "none", did: "did:test:friend" });

    emit("alice", { type: "agent-updated", agent: wire("did:test:friend", "Friend") });
    expect(profile.value).toEqual({ name: "Friend", did: "did:test:friend" });
    expect(rpcLog.filter((c) => c.type === "agent.byDIDs")).toHaveLength(1);
    scope.stop();
    client.close();
  });
});
