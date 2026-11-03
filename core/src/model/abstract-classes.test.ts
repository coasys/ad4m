/**
 * Abstract classes and aliases (#1435): what `@Model({ abstract })`,
 * `@Property({ alias })` and `@Model({ implements })` put into the SHACL, the
 * `implements` check, and where writes through an abstract class go.
 */
import { Ad4mModel } from "./Ad4mModel";
import { Model, Property, HasMany } from "./decorators";
import { SHACLShape } from "../shacl/SHACLShape";

@Model({ name: "Readable", abstract: true })
class Readable extends Ad4mModel {
  @Property({ through: "test://text" })
  text: string = "";
}

@Model({ name: "Folderish", abstract: true })
class Folderish extends Ad4mModel {
  @HasMany({ through: "test://child" })
  children: string[] = [];
}

describe("abstract classes", () => {
  it("writes one marker on the shape node, in JSON and in links", () => {
    const { shape } = (Readable as any).generateSHACL();
    expect((shape.toJSON() as any).abstract).toBe(true);
    const markers = shape.toLinks().filter((l: any) => l.predicate === "ad4m://abstract");
    expect(markers).toEqual([
      { source: shape.nodeShapeUri, predicate: "ad4m://abstract", target: "literal:true" },
    ]);
    expect(SHACLShape.fromLinks(shape.toLinks(), shape.nodeShapeUri).isAbstract).toBe(true);
    expect(SHACLShape.fromJSON(shape.toJSON()).isAbstract).toBe(true);
  });

  it("writes no marker on a concrete class", () => {
    @Model({ name: "PlainNote" })
    class PlainNote extends Ad4mModel {
      @Property({ through: "test://text" })
      text: string = "";
    }
    const { shape } = (PlainNote as any).generateSHACL();
    expect((shape.toJSON() as any).abstract).toBeUndefined();
    expect(shape.toLinks().some((l: any) => l.predicate === "ad4m://abstract")).toBe(false);
  });

  it("carries a property's and a relation's alias through JSON and links", () => {
    @Model({ name: "AliasedMessage", implements: [Readable, Folderish] })
    class AliasedMessage extends Ad4mModel {
      @Property({ through: "test://body", alias: { Readable: "text" } })
      body: string = "";

      @HasMany({ through: "test://reply", alias: { Folderish: "children" } })
      replies: string[] = [];
    }
    const { shape } = (AliasedMessage as any).generateSHACL();
    const json: any = shape.toJSON();
    expect(json.properties.find((p: any) => p.name === "body").alias).toEqual({ Readable: "text" });
    expect(json.properties.find((p: any) => p.name === "replies").alias).toEqual({ Folderish: "children" });

    const alias = shape.toLinks().find(
      (l: any) => l.predicate === "ad4m://alias" && l.source.endsWith("AliasedMessage.body"),
    );
    expect(alias.target).toBe(`literal:string:${encodeURIComponent('{"Readable":"text"}')}`);

    const back = SHACLShape.fromLinks(shape.toLinks(), shape.nodeShapeUri);
    expect(back.properties.find((p) => p.name === "body")!.alias).toEqual({ Readable: "text" });
    expect(SHACLShape.fromJSON(json).properties.find((p) => p.name === "replies")!.alias)
      .toEqual({ Folderish: "children" });
  });

  it("implements: a property or relation name or alias must cover every name", () => {
    expect(() => {
      @Model({ name: "Mute", implements: [Readable] })
      class Mute extends Ad4mModel {
        @Property({ through: "test://body" })
        body: string = "";
      }
    }).toThrow("Mute implements Readable but has no property or alias named `text`");

    // An alias for another abstract class does not count.
    expect(() => {
      @Model({ name: "Elsewhere", implements: [Readable] })
      class Elsewhere extends Ad4mModel {
        @Property({ through: "test://body", alias: { Other: "text" } })
        body: string = "";
      }
    }).toThrow("Elsewhere implements Readable but has no property or alias named `text`");

    // A relation does not stand in for a property, nor a property for a relation.
    expect(() => {
      @Model({ name: "Crossed", implements: [Folderish] })
      class Crossed extends Ad4mModel {
        @Property({ through: "test://children" })
        children: string = "";
      }
    }).toThrow("Crossed implements Folderish but has no relation or alias named `children`");

    // Its own name is enough.
    expect(() => {
      @Model({ name: "Plain", implements: [Readable] })
      class Plain extends Ad4mModel {
        @Property({ through: "test://other_text" })
        text: string = "";
      }
    }).not.toThrow();
  });

  it("implements: only abstract classes", () => {
    @Model({ name: "Concrete" })
    class Concrete extends Ad4mModel {
      @Property({ through: "test://text" })
      text: string = "";
    }
    expect(() => {
      @Model({ name: "Wrong", implements: [Concrete] })
      class Wrong extends Ad4mModel {
        @Property({ through: "test://text" })
        text: string = "";
      }
    }).toThrow("Wrong implements Concrete, which is not an abstract class");
  });

  it("save() refuses an abstract class before touching the perspective", async () => {
    const perspective: any = new Proxy({}, {
      get: () => () => { throw new Error("the perspective was touched"); },
    });
    const instance = new Readable(perspective);
    await expect(instance.save()).rejects.toThrow("'Readable' is an abstract class and cannot be created");
  });

  it("save() and delete() through an abstract class go to the executor's dispatch", async () => {
    const calls: any[][] = [];
    const perspective: any = new Proxy({}, {
      get: (_t, key) => async (...args: any[]) => {
        calls.push([key, ...args]);
        if (key === "modelQuery") {
          return { instances: [{ id: "ad4m://m/1", __subjectClass: "Message", text: "hi" }], totalCount: 1 };
        }
        return [];
      },
    });
    const [row] = await Readable.findAll(perspective);
    row.text = "edited";
    calls.length = 0;
    await row.save("batch");
    expect(calls).toEqual([["updateSubject", "Readable", "ad4m://m/1", { text: "edited" }, "batch"]]);

    calls.length = 0;
    await row.delete("batch");
    expect(calls[0][0]).toBe("removeSubject");
    expect(calls[0][2]).toBe("ad4m://m/1");
  });

  it("add/remove/set of a relation through an abstract class go to the executor's dispatch", async () => {
    const calls: any[][] = [];
    const perspective: any = new Proxy({}, {
      get: (_t, key) => async (...args: any[]) => {
        calls.push([key, ...args]);
        if (key === "modelQuery") {
          return { instances: [{ id: "ad4m://folder/1", __subjectClass: "Folder", children: [] }], totalCount: 1 };
        }
        return [];
      },
    });
    const [row]: any[] = await Folderish.findAll(perspective);
    calls.length = 0;
    await row.addChildren("ad4m://child/1", "batch");
    await row.removeChildren(["ad4m://child/1"], "batch");
    await row.setChildren([{ id: "ad4m://child/2" }, "ad4m://child/3"], "batch");
    // No link at the abstract predicate (test://child): only the dispatch.
    expect(calls).toEqual([
      ["updateSubject", "Folderish", "ad4m://folder/1", { children: { action: "adder", value: ["ad4m://child/1"] } }, "batch"],
      ["updateSubject", "Folderish", "ad4m://folder/1", { children: { action: "remover", value: ["ad4m://child/1"] } }, "batch"],
      ["updateSubject", "Folderish", "ad4m://folder/1", { children: { action: "setter", value: ["ad4m://child/2", "ad4m://child/3"] } }, "batch"],
    ]);
  });

  it("save() through an abstract class sends a changed relation's members as IDs", async () => {
    const calls: any[][] = [];
    const perspective: any = new Proxy({}, {
      get: (_t, key) => async (...args: any[]) => {
        calls.push([key, ...args]);
        if (key === "modelQuery") {
          return { instances: [{ id: "ad4m://folder/1", __subjectClass: "Folder", children: [] }], totalCount: 1 };
        }
        return [];
      },
    });
    const [row]: any[] = await Folderish.findAll(perspective);
    row.children = [{ id: "ad4m://child/2" }, "ad4m://child/3"];
    calls.length = 0;
    await row.save("batch");
    expect(calls).toEqual([
      ["updateSubject", "Folderish", "ad4m://folder/1", { children: ["ad4m://child/2", "ad4m://child/3"] }, "batch"],
    ]);
  });

  it("save() through an abstract class keeps a relation's action envelope", async () => {
    const calls: any[][] = [];
    const perspective: any = new Proxy({}, {
      get: (_t, key) => async (...args: any[]) => {
        calls.push([key, ...args]);
        if (key === "modelQuery") {
          return { instances: [{ id: "ad4m://folder/1", __subjectClass: "Folder", children: ["ad4m://child/1"] }], totalCount: 1 };
        }
        return [];
      },
    });
    const [row]: any[] = await Folderish.findAll(perspective);
    const sent = async (children: any) => {
      row.children = children;
      calls.length = 0;
      await row.save("batch");
      return calls.find((c) => c[0] === "updateSubject")?.[3];
    };
    expect(await sent({ action: "adder", value: [{ id: "ad4m://child/2" }] })).toEqual({
      children: { action: "adder", value: ["ad4m://child/2"] },
    });
    expect(await sent({ action: "remover", value: "ad4m://child/1" })).toEqual({
      children: { action: "remover", value: "ad4m://child/1" },
    });
    expect(await sent({ action: "other", value: ["ad4m://child/3"] })).toEqual({
      children: { action: "setter", value: ["ad4m://child/3"] },
    });
  });
});
