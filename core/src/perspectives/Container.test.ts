/**
 * The SDK's Container mirrors the executor's built-in
 * (`hardwired_sdna/container.json`), is read by its URI, and is never
 * registered in a perspective (#1446).
 */
import * as fs from "fs";
import * as path from "path";
import { Container } from "./Container";

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
