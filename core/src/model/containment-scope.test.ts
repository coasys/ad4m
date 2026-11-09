/**
 * The containment scope `{ id }` (#1446): sent to the executor as the id
 * alone, which resolves the predicates through the built-in `Container`.
 */
import { Ad4mModel } from "./Ad4mModel";
import { Model, Property } from "./decorators";
import { ModelQueryBuilder } from "./ModelQueryBuilder";
import { resolveParentPredicate } from "./query-common";
import { isContainedScope } from "./types";

@Model({ name: "ScopedNote" })
class ScopedNote extends Ad4mModel {
  @Property({ through: "test://note_body" })
  body: string = "";
}

@Model({ name: "ScopedTask" })
class ScopedTask extends Ad4mModel {
  @Property({ through: "test://task_title" })
  title: string = "";
}

describe("containment scope { id }", () => {
  it("travels as the id alone", () => {
    const { queryJson } = (ScopedNote as any).prepareModelQueryParams({ parent: { id: "ad4m://p" } });
    expect(JSON.parse(queryJson).parent).toEqual({ id: "ad4m://p" });
  });

  it("is what .parent(id) with no second argument asks for", () => {
    const builder: any = new ModelQueryBuilder({} as any, ScopedNote).parent("ad4m://p");
    expect(builder.queryParams.parent).toEqual({ id: "ad4m://p" });
    expect(isContainedScope(builder.queryParams.parent)).toBe(true);
  });

  it("is one scope over a union: every class sends the same", () => {
    const { queryJson } = Ad4mModel.prepareUnionQueryParams([ScopedNote, ScopedTask] as any, {
      parent: { id: "ad4m://p" },
    });
    expect(JSON.parse(queryJson).parent).toEqual({ id: "ad4m://p" });
  });

  it("names no single predicate, so the client-side resolver refuses it", () => {
    expect(() => resolveParentPredicate({ id: "ad4m://p" }, ScopedNote)).toThrow(
      "containment scope",
    );
    expect(isContainedScope({ id: "x", predicate: "p" })).toBe(false);
    expect(isContainedScope({ ids: ["x"] } as any)).toBe(false);
  });
});

describe("create({ parent: { id } })", () => {
  it("puts the record under the parent through the built-in Container", async () => {
    const calls: any[][] = [];
    const client: any = new Proxy({}, {
      get: (_t, key) => async (...args: any[]) => {
        calls.push([key, ...args]);
        return true;
      },
    });
    const { PerspectiveProxy } = await import("../perspectives/PerspectiveProxy");
    const proxy = new PerspectiveProxy({ uuid: "p" } as any, client);
    const note = await ScopedNote.create(proxy as any, { body: "hi" }, {
      parent: { id: "ad4m://p" },
      batchId: "b",
    });
    const writes = calls.filter(([key]) => key === "updateSubject" || key === "addLink");
    expect(writes).toEqual([
      [
        "updateSubject",
        "p",
        "ad4m://Container",
        "ad4m://p",
        JSON.stringify({ children: { action: "adder", value: [note.id] } }),
        "b",
      ],
    ]);
  });
});

describe("containment traversal { ids } with no predicate", () => {
  it("travels without a predicate, with maxDepth", () => {
    const { queryJson } = (ScopedNote as any).prepareModelQueryParams({
      parent: { ids: ["ad4m://m"], direction: "in", transitive: true, maxDepth: 3 },
    });
    expect(JSON.parse(queryJson).parent).toEqual({
      ids: ["ad4m://m"],
      direction: "in",
      transitive: true,
      maxDepth: 3,
    });
  });

  it("a traversal over a predicate still sends it", () => {
    const { queryJson } = (ScopedNote as any).prepareModelQueryParams({
      parent: { ids: ["ad4m://m"], predicate: "test://p", transitive: true, maxDepth: 2 },
    });
    expect(JSON.parse(queryJson).parent).toEqual({
      ids: ["ad4m://m"],
      predicate: "test://p",
      transitive: true,
      maxDepth: 2,
    });
  });
});
