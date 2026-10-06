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
    const listeners: Record<string, ((link: unknown) => void)[]> = {};
    const perspective = {
      uuid: "uuid-1",
      addListener: (type: string, cb: (link: unknown) => void) => { (listeners[type] ??= []).push(cb); },
    };
    const client = {
      perspective: {
        all: async () => [perspective],
        addPerspectiveUpdatedListener: () => () => {},
        addPerspectiveAddedListener: () => () => {},
        addPerspectiveRemovedListener: () => () => {},
      },
    };

    const { onLinkAdded, onLinkRemoved } = usePerspectives(client as any);
    await vi.waitFor(() => expect(listeners["link-removed"]).toHaveLength(1));
    const added = vi.fn();
    const removed = vi.fn();
    onLinkAdded(added);
    onLinkRemoved(removed);

    listeners["link-removed"][0]({ data: { source: "s" } });

    expect(removed).toHaveBeenCalledWith(perspective, { data: { source: "s" } });
    expect(added).not.toHaveBeenCalled();
  });
});
