import { describe, it, expect, beforeEach } from "vitest";
import { createElement, type ReactElement } from "react";
import { Window } from "happy-dom";
import { Ad4mClient } from "@coasys/ad4m";
import { emit, handleOf, installFakeExecutor, perspectives, rpcLog } from "../../test-support/fakeExecutor";
import { usePerspectives } from "./usePerspectives";

// The DOM is set up before react-dom loads: it reads `window` when it is first imported.
const dom = new Window();
Object.assign(globalThis, { window: dom, document: dom.document, HTMLElement: dom.HTMLElement });
const { createRoot } = await import("react-dom/client");
const { act } = await import("react-dom/test-utils");

(globalThis as any).IS_REACT_ACT_ENVIRONMENT = true;

beforeEach(installFakeExecutor);

/** Renders `Component` and returns what it rendered last, one entry per render. */
async function render<T>(use: () => T) {
  const renders: T[] = [];
  const Component = (): ReactElement | null => {
    renders.push(use());
    return null;
  };
  const root = createRoot(document.createElement("div"));
  await act(async () => root.render(createElement(Component)));
  return { renders, last: () => renders[renders.length - 1], unmount: () => act(async () => root.unmount()) };
}

const settle = (fn: () => void) => act(async () => fn());

// Updates render only after an act scope ends, so wait outside one, flushing in between.
async function rendered(condition: () => boolean) {
  while (!condition()) await act(async () => { await new Promise((r) => setTimeout(r, 1)); });
}

const wireLink = (source: string) => ({
  author: "did:test:alice",
  timestamp: "2024-01-01T00:00:00.000Z",
  data: { source, predicate: "test://has", target: "test://value" },
  proof: { key: "key", signature: "sig", valid: true, invalid: false },
  status: "SHARED",
});

const names = (view: { last(): { perspectives: Record<string, { name: string }> } }) =>
  Object.values(view.last().perspectives).map((p) => `${p.name}`).sort();

describe("usePerspectives", () => {
  it("lists the perspectives and follows the executor's events with one request", async () => {
    perspectives.set("alice", [handleOf("a"), handleOf("b", "b", "neighbourhood://b")]);
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const view = await render(() => usePerspectives(client));
    await rendered(() => names(view).length === 2);
    expect(Object.keys(view.last().neighbourhoods)).toEqual(["b"]);

    await settle(() => emit("alice", { type: "perspective-added", perspectiveUuid: "c", perspective: handleOf("c") }));
    await settle(() => emit("alice", { type: "perspective-updated", perspectiveUuid: "a", perspective: handleOf("a", "renamed") }));
    await settle(() => emit("alice", { type: "perspective-removed", perspectiveUuid: "b", uuid: "b" }));
    expect(names(view)).toEqual(["c", "renamed"]);
    expect(Object.keys(view.last().neighbourhoods)).toEqual([]);
    expect(rpcLog.filter((c) => c.type === "perspective.all")).toHaveLength(1);
    await view.unmount();
    client.close();
  });

  it("two components with one client share one request; two clients share nothing", async () => {
    perspectives.set("alice", [handleOf("a")]);
    perspectives.set("bob", [handleOf("x"), handleOf("y")]);
    const alice = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const bob = new Ad4mClient("http://127.0.0.1:12000", "bob");
    const one = await render(() => usePerspectives(alice));
    const two = await render(() => usePerspectives(alice));
    const other = await render(() => usePerspectives(bob));
    await rendered(() => names(one).length === 1 && names(two).length === 1 && names(other).length === 2);
    expect(rpcLog.filter((c) => c.type === "perspective.all" && c.token === "alice")).toHaveLength(1);
    expect(names(other)).toEqual(["x", "y"]);
    await one.unmount();
    await two.unmount();
    await other.unmount();
    alice.close();
    bob.close();
  });

  it("calls onLinkRemoved for a removed link, and an updated link is the old one removed, then the new one added", async () => {
    perspectives.set("alice", [handleOf("a")]);
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const calls: string[] = [];
    const view = await render(() => {
      const hook = usePerspectives(client);
      return hook;
    });
    await rendered(() => names(view).length === 1);
    view.last().onLinkAdded((p: { uuid: string }, link: any) => calls.push(`added ${p.uuid} ${link.data.source}`));
    view.last().onLinkRemoved((p: { uuid: string }, link: any) => calls.push(`removed ${p.uuid} ${link.data.source}`));

    await settle(() => emit("alice", { type: "link-removed", perspectiveUuid: "a", link: wireLink("s1") }));
    await settle(() => emit("alice", { type: "link-updated", perspectiveUuid: "a", oldLink: wireLink("s2"), newLink: wireLink("s3") }));
    expect(calls).toEqual(["removed a s1", "removed a s2", "added a s3"]);
    await view.unmount();
    client.close();
  });
});
