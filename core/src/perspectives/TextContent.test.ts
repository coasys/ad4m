/**
 * The SDK's TextContent mirrors the executor's built-in
 * (`hardwired_sdna/text_content.json`), is read by its URI, and is never
 * registered in a perspective (#1446).
 */
import * as fs from "fs";
import * as path from "path";
import { TextContent } from "./TextContent";
import { Ad4mModel } from "../model/Ad4mModel";
import { HasMany, Model, Property } from "../model/decorators";

const SDNA = JSON.parse(
  fs.readFileSync(
    path.resolve(__dirname, "../../../rust-executor/src/perspectives/hardwired_sdna/text_content.json"),
    "utf-8",
  ),
);

describe("TextContent", () => {
  it("matches the executor's built-in shape", () => {
    const { shape } = (TextContent as any).generateSHACL();
    expect(shape.targetClass).toBe(SDNA.target_class);
    expect(shape.isAbstract).toBe(SDNA.abstract);
    expect(shape.properties.map((p: any) => [p.path, p.name])).toEqual(
      SDNA.properties.map((p: any) => [p.path, p.name]),
    );
    // `text` required, `mentions` not: the executor leaves `mentions` optional.
    expect(shape.properties.map((p: any) => [p.name, p.minCount])).toEqual(
      SDNA.properties.map((p: any) => [p.name, p.min_count]),
    );
    expect(SDNA.properties.find((p: any) => p.name === "text").min_count).toBe(1);
  });

  it("is queried by its URI; a class extending it is not", () => {
    expect((TextContent as any).prepareModelQueryParams({}).className).toBe("ad4m://TextContent");

    @Model({ name: "OwnText" })
    class OwnText extends TextContent {
      @Property({ through: "app://extra" })
      extra: string = "";
    }
    expect((OwnText as any).prepareModelQueryParams({}).className).toBe("OwnText");
  });

  it("is sent by its URI in a union too, so the executor refuses it as abstract", async () => {
    @Model({ name: "UnionPeer" })
    class UnionPeer extends Ad4mModel {
      @Property({ through: "app://peer" })
      peer: string = "";
    }
    expect(Ad4mModel.prepareUnionQueryParams([TextContent, UnionPeer] as any, {}).classNames).toEqual([
      "ad4m://TextContent",
      "UnionPeer",
    ]);
    const sent: any[] = [];
    const perspective: any = {
      modelQuery: async (classNames: any) => {
        sent.push(classNames);
        throw new Error("classNames: 'ad4m://TextContent' is an abstract class.");
      },
    };
    await expect(Ad4mModel.findAllOf(perspective, [TextContent, UnionPeer] as any)).rejects.toThrow("abstract class");
    expect(sent).toEqual([["ad4m://TextContent", "UnionPeer"]]);
  });

  it("is implemented through aliases, for the text and the mention relation", () => {
    @Model({ name: "WeishTextBlock", implements: [TextContent] })
    class WeishTextBlock extends Ad4mModel {
      @Property({ through: "we://text", alias: { TextContent: "text" } })
      body: string = "";

      @HasMany({ through: "we://mention", alias: { TextContent: "mentions" } })
      mentionedAgents: string[] = [];
    }
    // The executor finds both by the aliases in the shape, not by `implements`.
    const { shape } = (WeishTextBlock as any).generateSHACL();
    const byName = (name: string) => shape.properties.find((p: any) => p.name === name);
    expect([byName("body").path, byName("body").alias]).toEqual(["we://text", { TextContent: "text" }]);
    expect([byName("mentionedAgents").path, byName("mentionedAgents").alias]).toEqual([
      "we://mention",
      { TextContent: "mentions" },
    ]);
  });

  it("is implemented through an alias for the text alone: `mentions` is optional", () => {
    expect(() => {
      @Model({ name: "FluxishMessage", implements: [TextContent] })
      class FluxishMessage extends Ad4mModel {
        @Property({ through: "flux://body", alias: { TextContent: "text" } })
        body: string = "";
      }
    }).not.toThrow();
  });

  it("is not implemented without the text, whatever else the class has", () => {
    expect(() => {
      @Model({ name: "MentionsOnly", implements: [TextContent] })
      class MentionsOnly extends Ad4mModel {
        @HasMany({ through: "we://mention", alias: { TextContent: "mentions" } })
        mentionedAgents: string[] = [];
      }
    }).toThrow("MentionsOnly implements TextContent but has no property or alias named `text`");
  });

  it("register() writes nothing", async () => {
    const { PerspectiveProxy } = await import("./PerspectiveProxy");
    const calls: string[] = [];
    const proxy = Object.create(PerspectiveProxy.prototype);
    // The skip comes before anything else: a write would be recorded, and
    // reaching the private registration set throws on this bare object.
    Object.defineProperty(proxy, "addSdna", { value: () => { calls.push("addSdna"); } });
    Object.defineProperty(proxy, "addSdnaAll", { value: () => { calls.push("addSdnaAll"); } });
    await expect(proxy.ensureSubjectClasses([TextContent])).resolves.toBeUndefined();
    await expect(proxy.ensureSubjectClass(TextContent)).resolves.toBeUndefined();
    expect(calls).toEqual([]);
  });

  it("save() and delete() of a row read through it go to the executor by its URI", async () => {
    const calls: any[][] = [];
    const client: any = new Proxy({}, {
      get: (_t, key) => async (...args: any[]) => {
        calls.push([key, ...args]);
        if (key === "modelQuery") {
          return { instances: [{ id: "ad4m://m/1", __subjectClass: "Message", text: "hi" }], totalCount: 1 };
        }
        return true;
      },
    });
    const { PerspectiveProxy } = await import("./PerspectiveProxy");
    const proxy = new PerspectiveProxy({ uuid: "p" } as any, client);
    const [row] = await TextContent.findAll(proxy);
    row.text = "edited";
    calls.length = 0;
    await row.save("batch");
    expect(calls).toEqual([["updateSubject", "p", "ad4m://TextContent", "ad4m://m/1", JSON.stringify({ text: "edited" }), "batch"]]);

    calls.length = 0;
    await row.delete("batch");
    expect(calls[0]).toEqual(["removeSubject", "p", "ad4m://TextContent", "ad4m://m/1", "batch"]);
  });

  it("delete() through it drops the perspective's cached query results, like an update", async () => {
    const { setCachedResult, getCachedResult } = await import("../model/query-cache");
    const client: any = new Proxy({}, { get: () => async () => true });
    const { PerspectiveProxy } = await import("./PerspectiveProxy");
    const proxy = new PerspectiveProxy({ uuid: "p-cache" } as any, client);
    const row = new TextContent(proxy, "ad4m://m/1");

    setCachedResult("p-cache", "SELECT ?v", [{ v: "old" }]);
    await proxy.updateSubject("ad4m://TextContent", "ad4m://m/1", { text: "new" });
    expect(getCachedResult("p-cache", "SELECT ?v")).toBeUndefined();

    setCachedResult("p-cache", "SELECT ?v", [{ v: "old" }]);
    await row.delete();
    expect(getCachedResult("p-cache", "SELECT ?v")).toBeUndefined();
  });
});
