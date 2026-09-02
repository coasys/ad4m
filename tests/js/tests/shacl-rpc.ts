import { expect } from "chai";
import {
    Ad4mModel,
    Flag,
    HasMany,
    Optional,
    Model,
    Property,
} from "@coasys/ad4m";
import { TestContext } from './integration.test'

// Exercises the four server-side SHACL resolution endpoints added in PR #935
// (perspective.getShaclNames / getShaclTargetClass / getShacl / getAllShacl).
// Runs inside the shared "Integration tests" executor via integration.test.ts
// — no separate executor spin-up, same pattern as perspective.ts / ai.ts.

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

export default function shaclRpcTests(testContext: TestContext) {
    return () => {
        describe("SHACL RPC endpoints (perspective.getShacl*)", () => {
            // Own perspective (not the one used by other suites in this run)
            // so the "before any class is registered" assertions aren't
            // polluted by SDNA classes other test suites may have registered.
            let perspectiveUuid: string;

            before(async () => {
                const perspective = await testContext.ad4mClient.perspective.add("shacl-rpc-endpoints");
                perspectiveUuid = perspective.uuid;
            });

            after(async () => {
                if (perspectiveUuid) {
                    await testContext.ad4mClient.perspective.remove(perspectiveUuid);
                }
            });

            describe("before any SHACL class is registered", () => {
                it("getShaclNames() returns []", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    expect(await perspective!.getShaclNames()).to.deep.equal([]);
                });

                it("getAllShacl() returns []", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    expect(await perspective!.getAllShacl()).to.deep.equal([]);
                });

                it("getShaclTargetClass('Todo') returns undefined", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    expect(await perspective!.getShaclTargetClass("Todo")).to.be.undefined;
                });

                it("getShacl('Todo') returns null", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    expect(await perspective!.getShacl("Todo")).to.be.null;
                });
            });

            describe("after registering Message + Todo", () => {
                before(async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    await perspective!.ensureSDNASubjectClass(ShaclMessage);
                    await perspective!.ensureSDNASubjectClass(ShaclTodo);
                });

                it("getShaclNames() returns the two registered names", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    const names = await perspective!.getShaclNames();
                    // Order not guaranteed — the executor returns names in the
                    // order they appear in the perspective's has_shacl link scan.
                    expect(names.slice().sort()).to.deep.equal(["Message", "Todo"]);
                });

                it("getShaclTargetClass() resolves each registered name to the class URI", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    // Namespace inference walks the first @Property/@Flag predicate:
                    //   Message: `ad4m://type`  → ns `ad4m://` → target `ad4m://Message`
                    //   Todo:    `todo://state` → ns `todo://` → target `todo://Todo`
                    expect(await perspective!.getShaclTargetClass("Message")).to.equal("ad4m://Message");
                    expect(await perspective!.getShaclTargetClass("Todo")).to.equal("todo://Todo");
                });

                it("getShaclTargetClass() returns undefined for an unknown name", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    expect(await perspective!.getShaclTargetClass("DoesNotExist")).to.be.undefined;
                });

                it("getShacl('Todo') returns a SHACLShape with the expected shape URI + target class", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    const shape = await perspective!.getShacl("Todo");
                    expect(shape).to.not.be.null;
                    expect(shape!.nodeShapeUri).to.equal("todo://TodoShape");
                    expect(shape!.targetClass).to.equal("todo://Todo");

                    const propByName: Record<string, any> = {};
                    for (const p of shape!.properties) {
                        if (p.name) propByName[p.name] = p;
                    }
                    // Todo declares three properties across three decorator kinds.
                    expect(Object.keys(propByName).sort()).to.deep.equal(["comments", "state", "title"]);

                    // @Property({through: "todo://state"}) — scalar, path matches.
                    expect(propByName.state.path).to.equal("todo://state");
                    // @Optional({through: "todo://has_title", resolveLanguage:"literal"})
                    expect(propByName.title.path).to.equal("todo://has_title");
                    // @HasMany({through: "todo://comment"}) — collection, path only.
                    expect(propByName.comments.path).to.equal("todo://comment");
                });

                it("getShacl('Message') reconstructs a shape with the Flag + Optional properties", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    const shape = await perspective!.getShacl("Message");
                    expect(shape).to.not.be.null;
                    expect(shape!.nodeShapeUri).to.equal("ad4m://MessageShape");
                    expect(shape!.targetClass).to.equal("ad4m://Message");

                    const names = shape!.properties
                        .map((p) => p.name)
                        .filter((n): n is string => Boolean(n))
                        .sort();
                    expect(names).to.deep.equal(["body", "type"]);

                    const byName = Object.fromEntries(
                        shape!.properties.filter((p) => p.name).map((p) => [p.name as string, p]),
                    );
                    // @Flag pins a fixed target via sh:hasValue; verify the path AND
                    // that the fixed value round-trips (the SHACL fingerprint that
                    // a @Flag survived the RPC boundary intact).
                    expect(byName.type.path).to.equal("ad4m://type");
                    expect(byName.type.hasValue).to.equal("ad4m://message");
                    expect(byName.body.path).to.equal("todo://state");
                });

                it("getShacl() returns null for an unknown name", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    expect(await perspective!.getShacl("DoesNotExist")).to.be.null;
                });

                it("getAllShacl() returns both shapes with matching name+targetClass+property counts", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    const all = await perspective!.getAllShacl();
                    expect(all.length).to.equal(2);

                    const byName = Object.fromEntries(all.map((e) => [e.name, e.shape]));
                    expect(Object.keys(byName).sort()).to.deep.equal(["Message", "Todo"]);
                    expect(byName.Message.targetClass).to.equal("ad4m://Message");
                    expect(byName.Todo.targetClass).to.equal("todo://Todo");

                    // Property counts match what the decorator model declared —
                    // proves the bulk-fetch endpoint reconstructs the same shape
                    // that the single-fetch endpoint returns.
                    expect(byName.Message.properties.length).to.equal(2);
                    expect(byName.Todo.properties.length).to.equal(3);
                });

                it("getShacl(name).properties matches the single-shape RPC result inside getAllShacl()", async () => {
                    const perspective = await testContext.ad4mClient.perspective.byUUID(perspectiveUuid);
                    // Consistency check: bulk and single fetch must agree on
                    // properties. Compare by (name, path) pairs — order-insensitive.
                    const single = await perspective!.getShacl("Todo");
                    const all = await perspective!.getAllShacl();
                    const fromAll = all.find((e) => e.name === "Todo")!.shape;

                    const key = (s: any) => s.properties.map((p: any) => `${p.name ?? ""}=${p.path}`).sort();
                    expect(key(single)).to.deep.equal(key(fromAll));
                });
            });
        });
    };
}
