/**
 * The SDK's Mentioning mirrors the executor's built-in
 * (`hardwired_sdna/mentioning.json`), is read by its URI, and is never
 * registered in a perspective.
 */
import * as fs from "fs";
import * as path from "path";
import { Mentioning } from "./Mentioning";
import { Ad4mModel } from "../model/Ad4mModel";
import { HasMany, Model, Property } from "../model/decorators";

const SDNA = JSON.parse(
  fs.readFileSync(
    path.resolve(__dirname, "../../../rust-executor/src/perspectives/hardwired_sdna/mentioning.json"),
    "utf-8",
  ),
);

describe("Mentioning", () => {
  it("matches the executor's built-in shape", () => {
    const { shape } = (Mentioning as any).generateSHACL();
    expect(shape.targetClass).toBe(SDNA.target_class);
    expect(shape.isAbstract).toBe(SDNA.abstract);
    expect(shape.properties.map((p: any) => [p.path, p.name])).toEqual(
      SDNA.properties.map((p: any) => [p.path, p.name]),
    );
  });

  it("is queried by its URI", () => {
    expect((Mentioning as any).prepareModelQueryParams({}).className).toBe("ad4m://Mentioning");
  });

  it("is implemented by a relation named mentions, with text or without", () => {
    expect(() => {
      @Model({ name: "MentioningRoot", implements: [Mentioning] })
      class MentioningRoot extends Ad4mModel {
        @Property({ through: "we://title" })
        title: string = "";

        @HasMany({ through: "we://mention" })
        mentions: string[] = [];
      }
    }).not.toThrow();
  });

  it("is implemented through an alias", () => {
    @Model({ name: "CcNote", implements: [Mentioning] })
    class CcNote extends Ad4mModel {
      @HasMany({ through: "app://cc", alias: { Mentioning: "mentions" } })
      cc: string[] = [];
    }
    // The executor finds the relation by the alias in the shape, not by `implements`.
    const { shape } = (CcNote as any).generateSHACL();
    const cc = shape.properties.find((p: any) => p.name === "cc");
    expect([cc.path, cc.alias]).toEqual(["app://cc", { Mentioning: "mentions" }]);
  });

  it("is not implemented by a relation that answers to another name", () => {
    expect(() => {
      @Model({ name: "UnaliasedNote", implements: [Mentioning] })
      class UnaliasedNote extends Ad4mModel {
        @HasMany({ through: "app://cc" })
        cc: string[] = [];
      }
    }).toThrow("UnaliasedNote implements Mentioning but has no relation or alias named `mentions`");
  });

  it("register() writes nothing", async () => {
    const { PerspectiveProxy } = await import("./PerspectiveProxy");
    const calls: string[] = [];
    const proxy = Object.create(PerspectiveProxy.prototype);
    Object.defineProperty(proxy, "addSdna", { value: () => { calls.push("addSdna"); } });
    Object.defineProperty(proxy, "addSdnaAll", { value: () => { calls.push("addSdnaAll"); } });
    await expect(proxy.ensureSubjectClasses([Mentioning])).resolves.toBeUndefined();
    await expect(proxy.ensureSubjectClass(Mentioning)).resolves.toBeUndefined();
    expect(calls).toEqual([]);
  });
});
