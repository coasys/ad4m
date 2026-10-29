/**
 * The SDK's Container mirrors the executor's built-in
 * (`hardwired_sdna/container.json`), is read by its URI, and is never
 * registered in a perspective (#1446).
 */
import * as fs from "fs";
import * as path from "path";
import { Container } from "./Container";
import { Ad4mModel } from "../model/Ad4mModel";
import { HasMany, Model } from "../model/decorators";

const SDNA = JSON.parse(
  fs.readFileSync(
    path.resolve(__dirname, "../../../rust-executor/src/perspectives/hardwired_sdna/container.json"),
    "utf-8",
  ),
);

describe("Container", () => {
  it("matches the executor's built-in shape", () => {
    const { shape } = (Container as any).generateSHACL();
    expect(shape.targetClass).toBe(SDNA.target_class);
    expect(shape.isAbstract).toBe(SDNA.abstract);
    expect(shape.properties.map((p: any) => [p.path, p.name])).toEqual(
      SDNA.properties.map((p: any) => [p.path, p.name]),
    );
  });

  it("is queried by its URI", () => {
    expect((Container as any).prepareModelQueryParams({}).className).toBe("ad4m://Container");
  });

  it("is implemented through a class's own relation, aliased to children", () => {
    @Model({ name: "ImplBlock", implements: [Container] })
    class ImplBlock extends Ad4mModel {
      @HasMany({ through: "we://child", alias: { Container: "children" } })
      blocks: string[] = [];
    }
    // The executor finds the relation by the alias in the shape, not by `implements`.
    const { shape } = (ImplBlock as any).generateSHACL();
    const blocks = shape.properties.find((p: any) => p.name === "blocks");
    expect([blocks.path, blocks.alias]).toEqual(["we://child", { Container: "children" }]);
  });

  it("is not implemented by a relation that answers to another name", () => {
    expect(() => {
      @Model({ name: "UnaliasedBlock", implements: [Container] })
      class UnaliasedBlock extends Ad4mModel {
        @HasMany({ through: "we://child" })
        blocks: string[] = [];
      }
    }).toThrow("UnaliasedBlock implements Container but has no relation or alias named `children`");
  });

  it("register() writes nothing", async () => {
    const { PerspectiveProxy } = await import("./PerspectiveProxy");
    const calls: string[] = [];
    const proxy = Object.create(PerspectiveProxy.prototype);
    Object.defineProperty(proxy, "addSdna", { value: () => { calls.push("addSdna"); } });
    Object.defineProperty(proxy, "addSdnaAll", { value: () => { calls.push("addSdnaAll"); } });
    await expect(proxy.ensureSubjectClasses([Container])).resolves.toBeUndefined();
    await expect(proxy.ensureSubjectClass(Container)).resolves.toBeUndefined();
    expect(calls).toEqual([]);
  });
});
