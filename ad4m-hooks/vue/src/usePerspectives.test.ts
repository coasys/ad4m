import { describe, it, expect, vi } from "vitest";

// vue is a peer dependency and not installed here; these stand-ins cover what the hook uses.
vi.mock("vue", () => ({
  ref: (value: unknown) => ({ value }),
  shallowRef: (value: unknown) => ({ value }),
  watch: () => {},
  effect: (fn: () => unknown) => fn(),
}));

import { usePerspectives } from "./usePerspectives";

describe("usePerspectives", () => {
  it("calls onLinkRemoved callbacks (and only those) for a removed link", async () => {
    const listeners: Record<string, ((event: { link: unknown }) => void)[]> = {};
    const perspective = {
      uuid: "uuid-1",
      on: (type: string, cb: (event: { link: unknown }) => void) => {
        (listeners[type] ??= []).push(cb);
        return () => {};
      },
    };
    const client = {
      on: () => () => {},
      perspective: { all: async () => [perspective] },
    };

    const { onLinkAdded, onLinkRemoved } = usePerspectives(client as any);
    await vi.waitFor(() => expect(listeners["link-removed"]).toHaveLength(1));
    const added = vi.fn();
    const removed = vi.fn();
    onLinkAdded(added);
    onLinkRemoved(removed);

    listeners["link-removed"][0]({ link: { data: { source: "s" } } });

    expect(removed).toHaveBeenCalledWith(perspective, { data: { source: "s" } });
    expect(added).not.toHaveBeenCalled();
  });

  it("reports an updated link as the old one removed and the new one added", async () => {
    const listeners: Record<string, ((event: any) => void)[]> = {};
    const perspective = {
      uuid: "uuid-1",
      on: (type: string, cb: (event: any) => void) => {
        (listeners[type] ??= []).push(cb);
        return () => {};
      },
    };
    const client = {
      on: () => () => {},
      perspective: { all: async () => [perspective] },
    };

    // The hook keeps its state at module level and fetches once: load it afresh.
    vi.resetModules();
    const fresh = await import("./usePerspectives");
    const { onLinkAdded, onLinkRemoved } = fresh.usePerspectives(client as any);
    await vi.waitFor(() => expect(listeners["link-updated"]).toHaveLength(1));
    const order: string[] = [];
    const added = vi.fn(() => order.push("added"));
    const removed = vi.fn(() => order.push("removed"));
    onLinkAdded(added);
    onLinkRemoved(removed);

    const oldLink = { data: { target: "old" } };
    const newLink = { data: { target: "new" } };
    listeners["link-updated"][0]({ oldLink, newLink });

    expect(removed).toHaveBeenCalledTimes(1);
    expect(removed).toHaveBeenCalledWith(perspective, oldLink);
    expect(added).toHaveBeenCalledTimes(1);
    expect(added).toHaveBeenCalledWith(perspective, newLink);
    expect(order).toEqual(["removed", "added"]);
  });
});
