import { describe, it, expect, beforeEach } from "vitest";
import { effectScope } from "vue";
import { Ad4mClient } from "@coasys/ad4m";
import { emit, failing, holding, installFakeExecutor, profileOf, release, rpcLog, until } from "../../test-support/fakeExecutor";
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
  it("keeps the DID in the profile when the caller passes a getter", async () => {
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const scope = effectScope();
    const { profile } = scope.run(() => useAgent(client.agent, () => "did:test:friend", (links) => ({ name: nameOf(links) })))!;
    await until(() => profile.value !== null);
    expect(profile.value).toMatchObject({ did: "did:test:friend" });
    scope.stop();
    client.close();
  });

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

  it("returns the error only while the state is error, not during a retry", async () => {
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    failing.add("agent.byDIDs");
    const use = () => useAgent(client.agent, "did:test:friend", (links) => ({ name: nameOf(links) }));
    const firstScope = effectScope();
    const first = firstScope.run(use)!;
    await until(() => first.error.value !== undefined);
    expect(first.error.value).toContain("agent.byDIDs failed");

    // A second view subscribes to the failed key: it reads again, and the read waits.
    failing.clear();
    holding.add("agent.byDIDs");
    const secondScope = effectScope();
    const second = secondScope.run(use)!;
    await until(() => rpcLog.filter((c) => c.type === "agent.byDIDs").length === 2);
    expect(second.error.value).toBeUndefined();
    expect(first.error.value).toBeUndefined();

    release();
    await until(() => second.agent.value !== null);
    expect(second.error.value).toBeUndefined();
    firstScope.stop();
    secondScope.stop();
    client.close();
  });
});
