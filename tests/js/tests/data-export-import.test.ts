/**
 * A person's own data moves between executors (AR-06 4.10): `data.exportPrepare` and
 * `data.exportChunk` on one executor build and read the gzip file, `data.importBegin`,
 * `data.importChunk` and `data.importCommit` on another write it into new private
 * perspectives, with the links' signatures kept and checked. Only the SDK's `data` client is
 * used. No Holochain. Needs `pnpm run prepare-test-local` first.
 */
import { expect } from "chai";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { ChildProcess } from "child_process";
import { Ad4mClient, LinkQuery } from "@coasys/ad4m";
import { baseUrl, quitExecutor, startExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const TEST_DIR = path.join(__dirname, "..", "tst-tmp");
const SEED_PATH = path.join(__dirname, "..", "bootstrapSeed.json");

interface Node {
    ports: number[];
    process: ChildProcess | null;
    client: Ad4mClient;
}

describe("Data export on one executor, import on another", function () {
    const nodes: Node[] = [];

    async function startNode(name: string): Promise<Node> {
        const ports = await getFreePorts(3);
        registerPorts(ports);
        const dataPath = path.join(TEST_DIR, "agents", `data-export-${name}`);
        fs.mkdirSync(path.dirname(dataPath), { recursive: true });
        const process = await startExecutor(
            dataPath, SEED_PATH, ports[0], ports[1], ports[2],
            false, undefined, undefined, undefined, undefined, false, undefined, undefined,
            false, // runHolochain
            true,  // sharedStores
        );
        const client = new Ad4mClient(baseUrl(ports[0]));
        await client.agent.generate("passphrase");
        const node = { ports, process, client };
        nodes.push(node);
        return node;
    }

    after(async () => {
        for (const node of nodes) {
            if (node.process) await quitExecutor(node.process, node.ports[0]);
            deregisterPorts(node.ports);
        }
    });

    it("moves a perspective and its signed links, and refuses a damaged file", async () => {
        const source = await startNode("source");
        const target = await startNode("target");

        const perspective = await source.client.perspective.add("moved");
        for (let i = 0; i < 5; i++) {
            await perspective.add({ source: "ad4m://self", predicate: "test://has", target: `test://item-${i}` });
        }

        const file = await source.client.data.exportFile([perspective.uuid]);
        expect(file.length).to.be.greaterThan(0);
        // A gzip stream.
        expect(file[0]).to.equal(0x1f);
        expect(file[1]).to.equal(0x8b);

        const imported = await target.client.data.importFile(file);
        expect(imported.linksWritten).to.equal(5);
        expect(imported.linksInvalid).to.equal(0);
        expect(imported.perspectives).to.have.length(1);
        const [fileUuid, newUuid] = imported.perspectives[0];
        expect(fileUuid).to.equal(perspective.uuid);

        const copy = await target.client.perspective.byUUID(newUuid);
        expect(copy).to.not.be.null;
        const links = await copy!.get(new LinkQuery({ source: "ad4m://self", predicate: "test://has" }));
        expect(links.map((l) => l.data.target).sort()).to.deep.equal(
            [0, 1, 2, 3, 4].map((i) => `test://item-${i}`),
        );
        // The signatures arrived as the source's agent made them.
        const original = await perspective.get(new LinkQuery({ source: "ad4m://self" }));
        expect(links.map((l) => l.proof.signature).sort()).to.deep.equal(
            original.map((l) => l.proof.signature).sort(),
        );

        // A file whose bytes do not match its sha256 is refused at the commit.
        const damaged = Uint8Array.from(file);
        damaged[damaged.length - 1] ^= 0xff;
        let refused = "";
        try {
            const begun = await target.client.data.importBegin(file.length, Array(64).fill("0").join(""));
            await target.client.data.importChunk(begun.importId, 0, Buffer.from(damaged).toString("base64"));
            await target.client.data.importCommit(begun.importId);
        } catch (e) {
            refused = String((e as Error).message ?? e);
        }
        expect(refused).to.not.equal("");
    });
});
