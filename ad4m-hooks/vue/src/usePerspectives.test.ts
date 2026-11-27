import { describe, it, expect, beforeEach } from "vitest";
import { effectScope } from "vue";
import { Ad4mClient } from "@coasys/ad4m";
import { emit, handleOf, installFakeExecutor, perspectives, rpcLog, until } from "../../test-support/fakeExecutor";
import { usePerspectives } from "./usePerspectives";

beforeEach(installFakeExecutor);

const wireLink = (source: string) => ({
  author: "did:test:alice",
  timestamp: "2024-01-01T00:00:00.000Z",
  data: { source, predicate: "test://has", target: "test://value" },
  proof: { key: "key", signature: "sig", valid: true, invalid: false },
  status: "SHARED",
});

const names = (view: { perspectives: { value: Record<string, { name: string }> } }) =>
  Object.values(view.perspectives.value).map((p) => p.name).sort();

describe("usePerspectives", () => {
  it("lists the perspectives and follows the executor's events with one request", async () => {
    perspectives.set("alice", [handleOf("a"), handleOf("b", "b", "neighbourhood://b")]);
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const scope = effectScope();
    const view = scope.run(() => usePerspectives(client))!;
    await until(() => names(view).length === 2);
    expect(Object.keys(view.neighbourhoods.value)).toEqual(["b"]);

    emit("alice", { type: "perspective-added", perspectiveUuid: "c", perspective: handleOf("c") });
    emit("alice", { type: "perspective-updated", perspectiveUuid: "a", perspective: handleOf("a", "renamed") });
    emit("alice", { type: "perspective-removed", perspectiveUuid: "b", uuid: "b" });
    await until(() => names(view).join() === "c,renamed");
    expect(Object.keys(view.neighbourhoods.value)).toEqual([]);
    expect(rpcLog.filter((c) => c.type === "perspective.all")).toHaveLength(1);
    scope.stop();
    client.close();
  });

  it("two clients share nothing: no state is kept at module level", async () => {
    perspectives.set("alice", [handleOf("a")]);
    perspectives.set("bob", [handleOf("x"), handleOf("y")]);
    const alice = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const bob = new Ad4mClient("http://127.0.0.1:12000", "bob");
    const scope = effectScope();
    const [a, b] = scope.run(() => [usePerspectives(alice), usePerspectives(bob)])!;
    await until(() => names(a).length === 1 && names(b).length === 2);
    expect(names(a)).toEqual(["a"]);
    expect(names(b)).toEqual(["x", "y"]);
    scope.stop();
    alice.close();
    bob.close();
  });

  it("calls onLinkRemoved for a removed link, and an updated link is the old one removed, then the new one added", async () => {
    perspectives.set("alice", [handleOf("a")]);
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const scope = effectScope();
    const view = scope.run(() => usePerspectives(client))!;
    await until(() => names(view).length === 1);
    const calls: string[] = [];
    view.onLinkAdded((p: { uuid: string }, link: any) => calls.push(`added ${p.uuid} ${link.data.source}`));
    view.onLinkRemoved((p: { uuid: string }, link: any) => calls.push(`removed ${p.uuid} ${link.data.source}`));

    emit("alice", { type: "link-removed", perspectiveUuid: "a", link: wireLink("s1") });
    emit("alice", { type: "link-updated", perspectiveUuid: "a", oldLink: wireLink("s2"), newLink: wireLink("s3") });
    expect(calls).toEqual(["removed a s1", "removed a s2", "added a s3"]);
    scope.stop();
    client.close();
  });
});
