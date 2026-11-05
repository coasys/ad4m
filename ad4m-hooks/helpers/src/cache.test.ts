import { describe, it, expect, vi } from "vitest";
import { subscribeToPerspective, unsubscribeFromPerspective } from "./cache";

function fakePerspective(uuid: string) {
  const listeners: Record<string, ((event: any) => void)[]> = {};
  const perspective = {
    uuid,
    on: (type: string, cb: (event: any) => void) => {
      (listeners[type] ??= []).push(cb);
      return () => {};
    },
  };
  const emit = (type: string, event: any) => (listeners[type] ?? []).forEach((cb) => cb(event));
  return { perspective, listeners, emit };
}

describe("subscribeToPerspective", () => {
  it("reports an updated link to subscribers as the old one removed and the new one added", () => {
    const { perspective, emit } = fakePerspective("cache-update");
    const order: string[] = [];
    const added = vi.fn(() => order.push("added"));
    const removed = vi.fn(() => order.push("removed"));
    subscribeToPerspective(perspective as any, added, removed);

    const oldLink = { data: { target: "old" } };
    const newLink = { data: { target: "new" } };
    emit("link-updated", { oldLink, newLink });

    expect(removed).toHaveBeenCalledTimes(1);
    expect(removed).toHaveBeenCalledWith(oldLink);
    expect(added).toHaveBeenCalledTimes(1);
    expect(added).toHaveBeenCalledWith(newLink);
    expect(order).toEqual(["removed", "added"]);
  });

  it("listens to a perspective once however many subscribe, and stops delivering after unsubscribe", () => {
    const { perspective, listeners, emit } = fakePerspective("cache-shared");
    const first = { added: vi.fn(), removed: vi.fn() };
    const second = { added: vi.fn(), removed: vi.fn() };
    subscribeToPerspective(perspective as any, first.added, first.removed);
    subscribeToPerspective(perspective as any, second.added, second.removed);
    expect(listeners["link-updated"]).toHaveLength(1);

    unsubscribeFromPerspective(perspective as any, first.added, first.removed);
    emit("link-updated", { oldLink: "old", newLink: "new" });

    expect(first.removed).not.toHaveBeenCalled();
    expect(first.added).not.toHaveBeenCalled();
    expect(second.removed).toHaveBeenCalledWith("old");
    expect(second.added).toHaveBeenCalledWith("new");
  });
});
