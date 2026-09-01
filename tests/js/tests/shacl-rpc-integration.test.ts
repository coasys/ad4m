import { expect } from "chai";
import { ChildProcess } from "node:child_process";
import {
    Ad4mClient,
    Ad4mModel,
    Flag,
    HasMany,
    Model,
    Optional,
    PerspectiveProxy,
    Property,
} from "@coasys/ad4m";
import path from "path";
import { fileURLToPath } from "url";
import { startExecutor, baseUrl, quitExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Two lightweight SHACL-decorated models exercised end-to-end through the
// four new perspective.getShacl* RPC endpoints. Both patterns (Flag+Optional
// and Property+Optional+HasMany) are lifted from prolog-and-literals.test.ts
// so the shapes we assert against mirror what real Flux-style apps register.
@Model({ name: "Message" })
class Message extends Ad4mModel {
    @Flag({
        through: "ad4m://type",
        value: "ad4m://message",
    })
    type: string = "";

    @Optional({ through: "todo://state" })
    body?: string;
}

@Model({ name: "Todo" })
class Todo extends Ad4mModel {
    @Property({
        through: "todo://state",
        initial: "todo://ready",
    })
    state!: string;

    @Optional({
        through: "todo://has_title",
        resolveLanguage: "literal",
    })
    title?: string;

    @HasMany({ through: "todo://comment" })
    comments: string[] = [];
}

describe("SHACL RPC endpoints (integration)", () => {
    let ad4m: Ad4mClient | null = null;
    let executorProcess: ChildProcess | null = null;
    let perspective: PerspectiveProxy | null = null;

    const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
    const appDataPath = path.join(TEST_DIR, "agents", "shacl-rpc-agent");
    const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
    let apiPort: number;
    let hcAdminPort: number;
    let hcAppPort: number;

    before(async () => {
        [apiPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
        registerPorts([apiPort, hcAdminPort, hcAppPort]);
        executorProcess = await startExecutor(
            appDataPath,
            bootstrapSeedPath,
            apiPort,
            hcAdminPort,
            hcAppPort,
        );

        // @ts-ignore - Apollo Client version mismatch between dependencies
        ad4m = new Ad4mClient(baseUrl(apiPort));
        await ad4m.agent.generate("secret");

        perspective = await ad4m.perspective.add("shacl-rpc-integration");
    });

    after(async () => {
        if (executorProcess) {
            await quitExecutor(executorProcess, apiPort);
        }
        deregisterPorts([apiPort, hcAdminPort, hcAppPort]);
    });

    describe("before any SHACL class is registered", () => {
        it("getShaclNames() returns []", async () => {
            const names = await perspective!.getShaclNames();
            expect(names).to.deep.equal([]);
        });

        it("getAllShacl() returns []", async () => {
            const shapes = await perspective!.getAllShacl();
            expect(shapes).to.deep.equal([]);
        });

        it("getShaclTargetClass('Todo') returns undefined", async () => {
            const tc = await perspective!.getShaclTargetClass("Todo");
            expect(tc).to.be.undefined;
        });

        it("getShacl('Todo') returns null", async () => {
            const shape = await perspective!.getShacl("Todo");
            expect(shape).to.be.null;
        });
    });

    describe("after registering Message + Todo", () => {
        before(async () => {
            await perspective!.ensureSDNASubjectClass(Message);
            await perspective!.ensureSDNASubjectClass(Todo);
        });

        it("getShaclNames() returns the two registered names", async () => {
            const names = await perspective!.getShaclNames();
            // Order not guaranteed — the executor returns names in the order
            // they appear in the perspective's has_shacl link scan.
            expect(names.slice().sort()).to.deep.equal(["Message", "Todo"]);
        });

        it("getShaclTargetClass() resolves each registered name to the class URI", async () => {
            // Namespace inference walks the first @Property/@Flag predicate:
            //   Message: `ad4m://type`  → ns `ad4m://` → target `ad4m://Message`
            //   Todo:    `todo://state` → ns `todo://` → target `todo://Todo`
            // (see core/src/model/shacl-gen.ts around L60–L90)
            const messageTC = await perspective!.getShaclTargetClass("Message");
            expect(messageTC).to.equal("ad4m://Message");

            const todoTC = await perspective!.getShaclTargetClass("Todo");
            expect(todoTC).to.equal("todo://Todo");
        });

        it("getShaclTargetClass() returns undefined for an unknown name", async () => {
            const tc = await perspective!.getShaclTargetClass("DoesNotExist");
            expect(tc).to.be.undefined;
        });

        it("getShacl('Todo') returns a SHACLShape with the expected shape URI + target class", async () => {
            const shape = await perspective!.getShacl("Todo");
            expect(shape).to.not.be.null;
            expect(shape!.nodeShapeUri).to.equal("todo://TodoShape");
            expect(shape!.targetClass).to.equal("todo://Todo");

            const propByName: Record<string, any> = {};
            for (const p of shape!.properties) {
                if (p.name) propByName[p.name] = p;
            }
            // Todo declares three properties across three decorator kinds.
            expect(Object.keys(propByName).sort()).to.deep.equal([
                "comments",
                "state",
                "title",
            ]);

            // @Property({through: "todo://state"}) — scalar, so path matches and
            // the SDK infers xsd:string from the initial URI value.
            expect(propByName.state.path).to.equal("todo://state");

            // @Optional({through: "todo://has_title", resolveLanguage:"literal"})
            expect(propByName.title.path).to.equal("todo://has_title");

            // @HasMany({through: "todo://comment"}) — collection, path only.
            expect(propByName.comments.path).to.equal("todo://comment");
        });

        it("getShacl('Message') reconstructs a shape with the Flag + Optional properties", async () => {
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
                shape!.properties
                    .filter((p) => p.name)
                    .map((p) => [p.name as string, p]),
            );
            // @Flag pins a fixed target via sh:hasValue; verify the path AND
            // that the fixed value round-trips (this is the SHACL fingerprint
            // that a @Flag survived the RPC boundary intact).
            expect(byName.type.path).to.equal("ad4m://type");
            expect(byName.type.hasValue).to.equal("ad4m://message");

            expect(byName.body.path).to.equal("todo://state");
        });

        it("getShacl() returns null for an unknown name", async () => {
            const shape = await perspective!.getShacl("DoesNotExist");
            expect(shape).to.be.null;
        });

        it("getAllShacl() returns both shapes with matching name+targetClass+property counts", async () => {
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
            // Consistency check: bulk and single fetch must agree on
            // properties. Compare by (name, path) pairs — order-insensitive.
            const single = await perspective!.getShacl("Todo");
            const all = await perspective!.getAllShacl();
            const fromAll = all.find((e) => e.name === "Todo")!.shape;

            const key = (s: any) =>
                s.properties
                    .map((p: any) => `${p.name ?? ""}=${p.path}`)
                    .sort();

            expect(key(single)).to.deep.equal(key(fromAll));
        });
    });
});
