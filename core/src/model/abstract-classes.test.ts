/**
 * Abstract classes and aliases (#1435): what `@Model({ abstract })`,
 * `@Property({ alias })` and `@Model({ implements })` put into the SHACL the
 * executor stores, and the `implements` check at declaration time.
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

  it("save() and delete() refuse a record read through an abstract class", async () => {
    const touched: string[] = [];
    const perspective: any = new Proxy({}, {
      get: (_t, key) => {
        if (key === "modelQuery") {
          return async () => ({
            instances: [{ id: "ad4m://m/1", __subjectClass: "Message", text: "hi" }],
            totalCount: 1,
          });
        }
        return () => { touched.push(String(key)); throw new Error(`the perspective was touched: ${String(key)}`); };
      },
    });
    const [row] = await Readable.findAll(perspective);
    expect(row.id).toBe("ad4m://m/1");
    row.text = "edited";
    await expect(row.save()).rejects.toThrow(
      "'Readable' is an abstract class: a record cannot be updated through it. Write it through its own class, 'Message'.",
    );
    await expect(row.delete()).rejects.toThrow(
      "'Readable' is an abstract class: a record cannot be deleted through it.",
    );
    expect(touched).toEqual([]);
  });
});
