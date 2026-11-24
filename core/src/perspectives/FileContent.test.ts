/**
 * The SDK's FileContent mirrors the executor's built-in
 * (`hardwired_sdna/file_content.json`), is read by its URI, and is never
 * registered in a perspective.
 */
import * as fs from "fs";
import * as path from "path";
import { FileContent } from "./FileContent";
import { Ad4mModel } from "../model/Ad4mModel";
import { Model, Property } from "../model/decorators";

const SDNA = JSON.parse(
  fs.readFileSync(
    path.resolve(__dirname, "../../../rust-executor/src/perspectives/hardwired_sdna/file_content.json"),
    "utf-8",
  ),
);

describe("FileContent", () => {
  it("matches the executor's built-in shape", () => {
    const { shape } = (FileContent as any).generateSHACL();
    expect(shape.targetClass).toBe(SDNA.target_class);
    expect(shape.isAbstract).toBe(SDNA.abstract);
    expect(shape.properties.map((p: any) => [p.path, p.name])).toEqual(
      SDNA.properties.map((p: any) => [p.path, p.name]),
    );
  });

  it("is queried by its URI", () => {
    expect((FileContent as any).prepareModelQueryParams({}).className).toBe("ad4m://FileContent");
  });

  it("is implemented through aliases on an app's own predicates", () => {
    expect(() => {
      @Model({ name: "AppFile", implements: [FileContent] })
      class AppFile extends Ad4mModel {
        @Property({ through: "we://name", alias: { FileContent: "name" } })
        title: string = "";
        @Property({ through: "we://mime_type", alias: { FileContent: "mimeType" } })
        type: string = "";
        @Property({ through: "we://size", alias: { FileContent: "size" } })
        bytes: number = 0;
        @Property({ through: "we://url", alias: { FileContent: "url" } })
        address: string = "";
      }
    }).not.toThrow();
  });

  it("register() sends no shape: the executor holds it", async () => {
    const { PerspectiveProxy } = await import("./PerspectiveProxy");
    const sent: unknown[][] = [];
    const proxy = Object.create(PerspectiveProxy.prototype);
    Object.defineProperty(proxy, "registerShapes", {
      value: async (shapes: unknown[]) => { sent.push(shapes); return { unchanged: [], written: [], generation: 0 }; },
    });
    await FileContent.register(proxy);
    expect(sent).toEqual([[]]);
  });
});
