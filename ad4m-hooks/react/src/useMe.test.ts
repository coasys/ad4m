import { describe, it, expect, beforeEach } from "vitest";
import { createElement, type ReactElement } from "react";
import { Window } from "happy-dom";
import { Ad4mClient } from "@coasys/ad4m";
import { emit, installFakeExecutor, profileOf, rpcLog } from "../../test-support/fakeExecutor";
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
  return { renders, last: () => renders[renders.length - 1], unmount: () => act(async () => root.unmount()) };
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
});
