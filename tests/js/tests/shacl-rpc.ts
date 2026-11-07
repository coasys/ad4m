import { expect } from "chai";
import {
    SHACLShape,
    SHACLFlow,
    LinkQuery,
    Link,
    Literal,
    Ad4mModel,
    Flag,
    HasMany,
    Optional,
    Model,
    Property,
    PerspectiveProxy,
} from "@coasys/ad4m";
import { TestContext } from './integration.test'

// Class registration over the executor: perspective.registerShapes and
// perspective.shapes. Runs inside the shared "Integration tests" executor via
// integration.test.ts, same pattern as perspective.ts / ai.ts.

@Model({ name: "Message" })
class ShaclMessage extends Ad4mModel {
    @Flag({ through: "ad4m://type", value: "ad4m://message" })
    type: string = "";

    @Optional({ through: "todo://state" })
    body?: string;
}

@Model({ name: "Todo" })
class ShaclTodo extends Ad4mModel {
    @Property({ through: "todo://state", initial: "todo://ready" })
    state!: string;

    @Optional({ through: "todo://has_title", resolveLanguage: "literal" })
    title?: string;

    @HasMany({ through: "todo://comment" })
    comments: string[] = [];
}

/** Every link of the perspective with its author and timestamp: a link
 *  removed and added back again shows as changed. */
async function snapshot(perspective: PerspectiveProxy): Promise<string[]> {
    return (await perspective.get(new LinkQuery({})))
        .map((l) => JSON.stringify([l.data.source, l.data.predicate ?? "", l.data.target, l.author, l.timestamp]))
        .sort();
}

export default function shaclRpcTests(testContext: TestContext) {
    return () => {
        describe("Class registration (perspective.registerShapes / shapes)", () => {
            // Own perspective (not the one used by other suites in this run)
            // so the "before any class is registered" assertions aren't
            // polluted by classes other test suites may have registered.
            let perspectiveUuid: string;
            const perspective = async () => (await testContext.ad4mClient.perspective.byUUID(perspectiveUuid))!;

            before(async () => {
                perspectiveUuid = (await testContext.ad4mClient.perspective.add("shacl-rpc-endpoints")).uuid;
            });

            after(async () => {
                if (perspectiveUuid) {
                    await testContext.ad4mClient.perspective.remove(perspectiveUuid);
                }
            });

            async function withPerspective(name: string, body: (p: PerspectiveProxy) => Promise<void>) {
                const p = await testContext.ad4mClient.perspective.add(name);
                try {
                    await body(p);
                } finally {
                    await testContext.ad4mClient.perspective.remove(p.uuid);
                }
            }

            describe("before any class is registered", () => {
                it("shapes() returns []", async () => {
                    expect(await (await perspective()).shapes()).to.deep.equal([]);
                });

                it("shapes(['Todo']) returns []", async () => {
                    expect(await (await perspective()).shapes(["Todo"])).to.deep.equal([]);
                });
            });

            describe("after registering Message + Todo", () => {
                before(async () => {
                    const result = await Ad4mModel.registerAll(await perspective(), [ShaclMessage, ShaclTodo]);
                    expect(result.written).to.deep.equal(["Message", "Todo"]);
                    expect(result.unchanged).to.deep.equal([]);
                });

                it("shapes() returns both classes, sorted by name, each with a hash", async () => {
                    const all = await (await perspective()).shapes();
                    expect(all.map((s) => s.name)).to.deep.equal(["Message", "Todo"]);
                    // The namespace of the first property names the class.
                    expect(all.map((s) => s.shape.targetClass)).to.deep.equal(["ad4m://Message", "todo://Todo"]);
                    for (const { hash } of all) expect(hash).to.match(/^[0-9a-f]{64}$/);
                });

                it("shapes(names) returns only the named classes, equal to the unfiltered read's", async () => {
                    const p = await perspective();
                    const all = await p.shapes();
                    const some = await p.shapes(["Todo", "DoesNotExist"]);
                    expect(some.map((s) => s.name)).to.deep.equal(["Todo"]);
                    expect(some[0].hash).to.equal(all.find((s) => s.name === "Todo")!.hash);
                    expect(await p.shapes([])).to.deep.equal([]);
                });

                it("the stored Todo is the shape the model generates", async () => {
                    const [todo] = await (await perspective()).shapes(["Todo"]);
                    expect(todo.shape.nodeShapeUri).to.equal("todo://TodoShape");
                    const byName = Object.fromEntries(todo.shape.properties.map((p) => [p.name, p]));
                    expect(Object.keys(byName).sort()).to.deep.equal(["comments", "state", "title"]);
                    expect(byName.state.path).to.equal("todo://state");
                    expect(byName.title.path).to.equal("todo://has_title");
                    expect(byName.comments.path).to.equal("todo://comment");
                    expect(byName.comments.collection).to.equal(true);
                    expect(byName.state.collection).to.not.equal(true);
                });

                it("the stored Message keeps the @Flag's fixed value", async () => {
                    const [message] = await (await perspective()).shapes(["Message"]);
                    const byName = Object.fromEntries(message.shape.properties.map((p) => [p.name, p]));
                    expect(Object.keys(byName).sort()).to.deep.equal(["body", "type"]);
                    expect(byName.type.path).to.equal("ad4m://type");
                    expect(byName.type.hasValue).to.equal("ad4m://message");
                    expect(byName.body.path).to.equal("todo://state");
                });

                it("registering the same classes again writes no link", async () => {
                    const p = await perspective();
                    const before = await snapshot(p);
                    const result = await Ad4mModel.registerAll(p, [ShaclMessage, ShaclTodo]);
                    expect(result.unchanged).to.deep.equal(["Message", "Todo"]);
                    expect(result.written).to.deep.equal([]);
                    expect(await snapshot(p)).to.deep.equal(before);
                });
            });

            it("a changed shape is rewritten, and links other apps put on it stay", async () => {
                await withPerspective("shacl-rpc-refresh", async (p) => {
                    const note = (datatype: string) => {
                        const shape = new SHACLShape("note://Note");
                        shape.addProperty({ name: "text", path: "note://text", datatype, maxCount: 1 });
                        return shape;
                    };
                    const first = await p.registerShapes([note("xsd://string")]);
                    expect(first.written).to.deep.equal(["Note"]);
                    // An annotation another app keeps on the property node.
                    await p.add(new Link({ source: "note://Note.text", predicate: "other://label", target: "literal://string:Text" }));

                    const second = await p.registerShapes([note("xsd://integer")]);
                    expect(second.written).to.deep.equal(["Note"]);
                    expect(second.generation).to.be.greaterThan(first.generation);

                    const [stored] = await p.shapes(["Note"]);
                    expect(stored.shape.properties.map((prop) => prop.datatype)).to.deep.equal(["xsd://integer"]);
                    const datatypes = await p.get(new LinkQuery({ source: "note://Note.text", predicate: "sh://datatype" }));
                    expect(datatypes.map((l) => l.data.target)).to.deep.equal(["xsd://integer"]);
                    expect(await p.get(new LinkQuery({ source: "note://Note.text", predicate: "other://label" }))).to.have.length(1);
                });
            });

            it("a shape given a new URI moves: the class points to the new one only", async () => {
                await withPerspective("shacl-rpc-move", async (p) => {
                    const memo = (shapeUri: string) => {
                        const shape = new SHACLShape(shapeUri, "memo://Memo");
                        shape.addProperty({
                            name: "body", path: "memo://body", maxCount: 1,
                            setter: [{ action: "setSingleTarget", source: "this", predicate: "memo://body", target: "value" }],
                        });
                        return shape;
                    };
                    await p.registerShapes([memo("old://MemoShape")]);
                    await p.registerShapes([memo("new://MemoShape")]);

                    const [stored] = await p.shapes(["Memo"]);
                    expect(stored.shape.nodeShapeUri).to.equal("new://MemoShape");
                    const pointers = await p.get(new LinkQuery({ source: "memo://Memo", predicate: "ad4m://shape" }));
                    expect(pointers.map((l) => l.data.target)).to.deep.equal(["new://MemoShape"]);
                    expect(await p.get(new LinkQuery({ source: "old://MemoShape" }))).to.deep.equal([]);

                    // An all-optional class can be instantiated.
                    await p.createSubject("Memo", "memo://1", { body: "hi" });
                    expect(await p.get(new LinkQuery({ source: "memo://1", predicate: "memo://body" }))).to.have.length(1);
                });
            });

            it("refuses a second class under a name another class holds, and writes nothing", async () => {
                await withPerspective("shacl-rpc-iri", async (p) => {
                    const a = new SHACLShape("a://Note");
                    a.addProperty({ name: "text", path: "a://text" });
                    const b = new SHACLShape("b://Note");
                    b.addProperty({ name: "body", path: "b://body" });
                    await p.registerShapes([a]);
                    const before = await snapshot(p);

                    let error: Error | undefined;
                    try {
                        await p.registerShapes([b]);
                    } catch (e) {
                        error = e as Error;
                    }
                    expect(error?.message).to.contain("Class 'a://Note' is already registered under the name 'Note'");
                    expect(await snapshot(p)).to.deep.equal(before);
                    const notes = await p.shapes(["Note"]);
                    expect(notes.map((s) => s.shape.targetClass)).to.deep.equal(["a://Note"]);
                });
            });

            it("refuses a shape URI another class holds, and writes nothing", async () => {
                await withPerspective("shacl-rpc-shared-uri", async (p) => {
                    const a = new SHACLShape("shared://NoteShape", "a://Note");
                    a.addProperty({ name: "text", path: "a://text" });
                    await p.registerShapes([a]);
                    const before = await snapshot(p);

                    const b = new SHACLShape("shared://NoteShape", "b://Note");
                    b.addProperty({ name: "text", path: "b://text" });
                    let error: Error | undefined;
                    try {
                        await p.registerShapes([b]);
                    } catch (e) {
                        error = e as Error;
                    }
                    expect(error?.message).to.contain("already belongs to class 'a://Note'");
                    expect(await snapshot(p)).to.deep.equal(before);
                });
            });

            it("refuses a shape URI that does not name its class", async () => {
                await withPerspective("shacl-rpc-shape-uri", async (p) => {
                    let error: Error | undefined;
                    try {
                        await p.registerShapes([new SHACLShape("shapes://MemoShape", "note://Note")]);
                    } catch (e) {
                        error = e as Error;
                    }
                    expect(error?.message).to.contain("NoteShape");
                });
            });

            it("addFlow() replaces the stored transitions when a flow is re-added", async () => {
                await withPerspective("shacl-rpc-flow-readd", async (p) => {
                    const todoFlow = () => {
                        const flow = new SHACLFlow("Todo", "todo://");
                        flow.addState({ name: "ready", value: 0 });
                        flow.addState({ name: "done", value: 1 });
                        flow.addTransition({ actionName: "Complete", fromState: "ready", toState: "done", actions: [] });
                        return flow;
                    };
                    const transitions = async () =>
                        (await p.getFlow("Todo"))!.transitions.map((t) => `${t.fromState}->${t.toState}:${t.actionName}`);

                    // A flow stored with the old `{from}To{to}` transition URIs.
                    const legacy = todoFlow().toLinks().map((l) => ({
                        ...l,
                        source: l.source.replace(/\.transition\/.*$/, ".readyTodone"),
                        target: l.target.replace(/\.transition\/.*$/, ".readyTodone"),
                    }));
                    await p.addLinks(legacy);

                    await p.addFlow("Todo", todoFlow());
                    expect(await transitions()).to.deep.equal(["ready->done:Complete"]);

                    await p.addFlow("Todo", todoFlow());
                    expect(await transitions()).to.deep.equal(["ready->done:Complete"]);
                });
            });

            it("addFlow() replaces a changed rule and leaves links it does not own", async () => {
                await withPerspective("shacl-rpc-flow-rule", async (p) => {
                    const todoFlow = (n: number) => {
                        const flow = new SHACLFlow("Todo", "todo://");
                        flow.addState({ name: "ready", value: 0 });
                        flow.addState({ name: "done", value: 1, consensusRule: { n } });
                        flow.addTransition({ actionName: "Complete", fromState: "ready", toState: "done", actions: [] });
                        return flow;
                    };
                    const stateUri = todoFlow(1).stateUri("done");
                    const rules = async () =>
                        (await p.get(new LinkQuery({ source: stateUri, predicate: "ad4m://consensusRule" })))
                            .map((l) => JSON.parse(Literal.fromUrl(l.data.target).get()).n);

                    await p.addFlow("Todo", todoFlow(2));
                    // A link on the flow's URI that is not part of its definition.
                    const flowUri = todoFlow(1).flowUri;
                    await p.add(new Link({ source: flowUri, predicate: "todo://receipt", target: "todo://r1" }));

                    await p.addFlow("Todo", todoFlow(3));
                    expect(await rules()).to.deep.equal([3]);
                    expect(await p.get(new LinkQuery({ source: flowUri, predicate: "todo://receipt" })))
                        .to.have.length(1);

                    // An unchanged definition writes nothing.
                    const before = await snapshot(p);
                    await p.addFlow("Todo", todoFlow(3));
                    expect(await snapshot(p)).to.deep.equal(before);
                    expect(await rules()).to.deep.equal([3]);
                });
            });

            it("shapes() keeps the parentShapes of a registered class", async () => {
                await withPerspective("shacl-rpc-parents", async (p) => {
                    const shape = new SHACLShape("zoo://Dog");
                    shape.addParentShape("zoo://AnimalShape");
                    shape.addProperty({ name: "name", path: "zoo://name", datatype: "xsd://string", maxCount: 1 });
                    await p.registerShapes([shape]);

                    const [dog] = await p.shapes(["Dog"]);
                    expect(dog.shape.parentShapes).to.deep.equal(["zoo://AnimalShape"]);
                });
            });
        });
    };
}
