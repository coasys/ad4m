import { describe, it, expect, beforeEach } from "vitest";
import { createElement, type ReactElement } from "react";
import { Window } from "happy-dom";
import { Ad4mClient } from "@coasys/ad4m";
import { emit, failing, holding, installFakeExecutor, profileOf, release, rpcLog } from "../../test-support/fakeExecutor";
import { useMe } from "./useMe";
import { useAgent } from "./useAgent";

// The DOM is set up before react-dom loads: it reads `window` when it is first imported.
const dom = new Window();
Object.assign(globalThis, { window: dom, document: dom.document, HTMLElement: dom.HTMLElement });
const { createRoot } = await import("react-dom/client");
const { act } = await import("react-dom/test-utils");

(globalThis as any).IS_REACT_ACT_ENVIRONMENT = true;

const nameOf = (links: any[]) => links[0].data.target as string;

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
  return {
    renders,
    last: () => renders[renders.length - 1],
    rerender: () => act(async () => root.render(createElement(Component))),
    unmount: () => act(async () => root.unmount()),
  };
}

const settle = (fn: () => void) => act(async () => fn());

// Updates render only after an act scope ends, so wait outside one, flushing in between.
async function rendered(condition: () => boolean) {
  while (!condition()) await act(async () => { await new Promise((r) => setTimeout(r, 1)); });
}

describe("useMe", () => {
  it("shows the agent and re-renders when agent-updated replaces it", async () => {
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const view = await render(() => useMe(client, nameOf));
    await rendered(() => view.last().profile === "none");
    expect(view.last().status?.did).toBe("did:test:alice");

    await settle(() => emit("alice", { type: "agent-updated", agent: profileOf("did:test:alice", "Alice") }));
    expect(view.last().profile).toBe("Alice");
    expect(rpcLog.filter((c) => c.type === "agent.get")).toHaveLength(1);
    await view.unmount();
    client.close();
  });

  it("computes the profile again when only the formatter changes", async () => {
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    let format = nameOf;
    const view = await render(() => useMe(client, format));
    await rendered(() => view.last().profile === "none");
    await settle(() => emit("alice", { type: "agent-updated", agent: profileOf("did:test:alice", "Alice") }));
    expect(view.last().profile).toBe("Alice");
    format = (links) => nameOf(links).toUpperCase();
    await view.rerender();
    expect(view.last().profile).toBe("ALICE");
    await view.unmount();
    client.close();
  });

  it("two clients with two tokens share nothing", async () => {
    const alice = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const bob = new Ad4mClient("http://127.0.0.1:12000", "bob");
    const a = await render(() => useMe(alice, nameOf));
    const b = await render(() => useMe(bob, nameOf));
    await rendered(() => a.last().me !== undefined && b.last().me !== undefined);
    expect(a.last().me?.did).toBe("did:test:alice");
    expect(b.last().me?.did).toBe("did:test:bob");

    await settle(() => emit("alice", { type: "agent-updated", agent: profileOf("did:test:alice", "Alice") }));
    expect(a.last().profile).toBe("Alice");
    expect(b.last().profile).toBe("none");
    await a.unmount();
    await b.unmount();
    alice.close();
    bob.close();
  });
});

describe("useAgent", () => {
  it("shows another agent and re-renders when agent-updated refreshes it", async () => {
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    const view = await render(() =>
      useAgent({ client: client.agent, did: "did:test:friend", formatter: (links) => nameOf(links) }));
    await rendered(() => view.last().profile === "none");

    await settle(() => emit("alice", { type: "agent-updated", agent: profileOf("did:test:friend", "Friend") }));
    expect(view.last().profile).toBe("Friend");
    expect(rpcLog.filter((c) => c.type === "agent.byDIDs")).toHaveLength(1);
    await view.unmount();
    client.close();
  });

  it("returns the error only while the state is error, not during a retry", async () => {
    const client = new Ad4mClient("http://127.0.0.1:12000", "alice");
    failing.add("agent.byDIDs");
    const use = () => useAgent({ client: client.agent, did: "did:test:friend", formatter: (links) => nameOf(links) });
    const first = await render(use);
    await rendered(() => first.last().error !== undefined);
    expect(first.last().error).toContain("agent.byDIDs failed");

    // A second view subscribes to the failed key: it reads again, and the read waits.
    failing.clear();
    holding.add("agent.byDIDs");
    const second = await render(use);
    await rendered(() => rpcLog.filter((c) => c.type === "agent.byDIDs").length === 2);
    expect(second.last().error).toBeUndefined();
    expect(first.last().error).toBeUndefined();

    await act(async () => release());
    await rendered(() => second.last().profile === "none");
    expect(second.last().error).toBeUndefined();
    await first.unmount();
    await second.unmount();
    client.close();
  });
});
