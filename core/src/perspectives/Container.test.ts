/**
 * The SDK's Container mirrors the executor's built-in
 * (`hardwired_sdna/container.json`), is read by its URI, and is never
 * registered in a perspective (#1446).
 */
import * as fs from "fs";
import * as path from "path";
import { Container } from "./Container";
import { Ad4mModel } from "../model/Ad4mModel";
import { BelongsToOne, Flag, HasMany, HasOne, Model, Property } from "../model/decorators";

const SDNA = JSON.parse(
  fs.readFileSync(
    path.resolve(__dirname, "../../../rust-executor/src/perspectives/hardwired_sdna/container.json"),
    "utf-8",
  ),
);

// We's entities as of We origin/dev 4e48f3b6
// (packages/backend-system/ad4m/src/entities/{WeNode,CollectionBlock,TextBlock}.ts), decorators
// copied verbatim; only the typed targets (Signal, ExtractionPass, ExtractionAmendment) are stubs
// and FILE_STORAGE_LANGUAGE is a placeholder address.
const FILE_STORAGE_LANGUAGE = "QmzSYwdFileStorageLanguagePlaceholder";
@Model({ name: "Signal" })
class Signal extends Ad4mModel {}
@Model({ name: "ExtractionPass" })
class ExtractionPass extends Ad4mModel {}
@Model({ name: "ExtractionAmendment" })
class ExtractionAmendment extends Ad4mModel {}

@Model({ name: 'WeNode' })
class WeNode extends Ad4mModel {
  @HasMany({ through: 'we://response', polymorphic: true })
  responses: string[] = [];
  @BelongsToOne({ through: 'we://response', polymorphic: true })
  respondsTo?: string;
  @HasMany({ through: 'we://thread', polymorphic: true })
  thread: string[] = [];
  @BelongsToOne({ through: 'we://thread', polymorphic: true })
  inThread?: string;
  @BelongsToOne({ through: 'we://child', polymorphic: true })
  partOf?: string;
  @BelongsToOne({ through: 'we://extracted', polymorphic: true })
  extractedFrom?: string;
  @HasMany(() => Signal, { through: 'we://signal' })
  signals: string[] = [];
  @HasMany({ through: 'we://participant' })
  participants: string[] = [];
  @HasMany({ through: 'we://mention' })
  mentions: string[] = [];
}

@Model({ name: 'CollectionBlock' })
class CollectionBlock extends WeNode {
  @Flag({ through: 'we://flag', value: 'we://collection_block' })
  flag: string = '';
  @Property({ through: 'we://kind' })
  kind: string = '';
  @Property({ through: 'we://mode' })
  mode: string = '';
  @Property({ through: 'we://title' })
  title: string = '';
  @Property({ through: 'we://description' })
  description: string = '';
  @Property({ through: 'we://slug' })
  slug: string = '';
  @Property({ through: 'we://editor_state', resolveLanguage: FILE_STORAGE_LANGUAGE })
  editorState: string | null = null;
  @Property({ through: 'we://text_content' })
  textContent: string = '';
  @Property({ through: 'we://version' })
  version: number = 0;
  @Property({ through: 'we://source_ref' })
  sourceRef: string = '';
  @Property({ through: 'we://source_name' })
  sourceName: string = '';
  @Property({ through: 'we://auto_extract' })
  autoExtract?: boolean;
  @Property({ through: 'we://extraction_targets' })
  extractionTargets: string = '';
  @HasMany({ through: 'we://child', ordering: { strategy: 'linkedList' }, polymorphic: true })
  children: string[] = [];
  @HasMany({ through: 'we://arranges', ordering: { strategy: 'linkedList' }, polymorphic: true })
  arranges: string[] = [];
  @HasOne({ through: 'we://gathers', polymorphic: true })
  gathers?: string;
  @HasOne(() => CollectionBlock, { through: 'we://board' })
  board?: CollectionBlock;
  @HasMany(() => ExtractionPass, { through: 'we://extraction_pass_record' })
  extractionPasses: string[] = [];
  @HasMany({ through: 'we://extracted', polymorphic: true })
  extracted: string[] = [];
  @HasMany({ through: 'we://recognised', polymorphic: true })
  recognised: string[] = [];
  @HasMany(() => ExtractionAmendment, { through: 'we://extraction_amendment' })
  amendments: string[] = [];
}

@Model({ name: 'TextBlock' })
class TextBlock extends WeNode {
  @Flag({ through: 'we://flag', value: 'we://text_block' })
  flag: string = '';
  @Property({ through: 'we://style' })
  style: string = 'normal';
  @Property({ through: 'we://list_item' })
  listItem: string = '';
  @Property({ through: 'we://level' })
  level: number = 0;
  @Property({ through: 'we://checked' })
  checked: boolean = false;
  @Property({ through: 'we://align' })
  align: string = '';
  @Property({ through: 'we://direction' })
  direction: string = '';
  @Property({ through: 'we://text' })
  text: string = '';
  @Property({ through: 'we://marks' })
  marks: string = '';
  @Property({ through: 'we://text_source' })
  source: string = '';
  @Property({ through: 'we://version' })
  version: number = 0;
}

/** The executor tests (`mcp/tools/instances/tests.rs`) load We's shapes from these files. */
const WE_SHAPES_DIR = path.resolve(__dirname, "../../../rust-executor/src/mcp/tools/instances/we_shapes");

describe("Container", () => {
  it.each([["collection_block.json", CollectionBlock], ["text_block.json", TextBlock]])(
    "We's %s, as the SDK writes it, is the executor test's fixture",
    (file, model) => {
      const generated = JSON.stringify((model as any).generateSHACL().shape.toJSON(), null, 2) + "\n";
      const fixture = path.join(WE_SHAPES_DIR, file as string);
      if (process.env.UPDATE_WE_SHAPES) fs.writeFileSync(fixture, generated);
      expect(fs.readFileSync(fixture, "utf-8")).toBe(generated);
    },
  );

  it("matches the executor's built-in shape", () => {
    const { shape } = (Container as any).generateSHACL();
    expect(shape.targetClass).toBe(SDNA.target_class);
    expect(shape.isAbstract).toBe(SDNA.abstract);
    const relation = (p: any) => [p.path, p.name, p.relationKind ?? p.relation_kind, p.nodeKind ?? p.node_kind];
    expect(shape.properties.map(relation)).toEqual(SDNA.properties.map(relation));
  });

  it("is queried by its URI", () => {
    expect((Container as any).prepareModelQueryParams({}).className).toBe("ad4m://Container");
  });

  it("is implemented by a children relation through any predicate", () => {
    expect(() => {
      @Model({ name: "WeishCollection", implements: [Container] })
      class WeishCollection extends Ad4mModel {
        @HasMany({ through: "we://child", polymorphic: true })
        children: string[] = [];
      }
    }).not.toThrow();
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
