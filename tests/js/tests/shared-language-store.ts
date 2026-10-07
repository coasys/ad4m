/**
 * Shared mode of the local language-language, neighbourhood store and
 * agent-language.
 *
 * Invariant: with a `storagePath` setting (written by startExecutor, see
 * utils/sharedStores.ts), a language, neighbourhood or agent profile that
 * one executor publishes can be fetched by another executor that points at
 * the same directory, with no Holochain. Without the setting, the stores
 * keep using the per-executor ad4m:host KV, and nothing reaches the
 * directory.
 *
 * Needs testContext.alice and testContext.bob: two executors started with
 * the default (shared) stores.
 */
import { Ad4mClient, LanguageMetaInput, Perspective } from "@coasys/ad4m";
import { expect } from "chai";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { v4 as uuidv4 } from "uuid";
import { TestContext } from "./test-context";
import { baseUrl, quitExecutor, startExecutor } from "../utils/utils";
import { SHARED_LANGUAGES_DIR, SHARED_NEIGHBOURHOODS_DIR, sharedLanguageExists } from "../utils/sharedStores";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const TEST_DIR = path.join(__dirname, "..", "tst-tmp");
const NOTE_STORE_BUNDLE = path.join(__dirname, "..", "languages", "note-store", "build", "bundle.js");
const DIFF_SYNC_HASH = fs.readFileSync(path.join(__dirname, "..", "scripts", "perspective-diff-sync-hash")).toString();

/** A copy of the note-store bundle with a unique address, so every publish
 *  here is new to the store. */
function uniqueBundle(tag: string): string {
    const file = path.join(TEST_DIR, `shared-store-${tag}-${uuidv4()}.js`);
    fs.writeFileSync(file, fs.readFileSync(NOTE_STORE_BUNDLE).toString() + `\n// ${path.basename(file)}\n`);
    return file;
}

export default function sharedLanguageStoreTests(testContext: TestContext) {
    return () => {
        it("Bob fetches a language that Alice published", async () => {
            const alice = testContext.alice;
            const bob = testContext.bob;

            const published = await alice.languages.publish(uniqueBundle("lang"),
                new LanguageMetaInput("shared-store language", "published by Alice"));

            const expression = await bob.expression.get(`lang://${published.address}`);
            expect(expression, "Bob's language-language returned nothing").to.not.be.null;
            expect(expression.proof.valid).to.be.true;
            expect(expression.author).to.equal((await alice.agent.me()).did);
            expect(JSON.parse(expression.data).name).to.equal("shared-store language");

            const meta = await bob.languages.meta(published.address);
            expect(meta.address).to.equal(published.address);
            expect(meta.author).to.equal((await alice.agent.me()).did);

            expect(sharedLanguageExists(published.address), `meta-${published.address}.json in ${SHARED_LANGUAGES_DIR}`).to.be.true;
        });

        it("Bob fetches and joins a neighbourhood that Alice published", async () => {
            const alice = testContext.alice;
            const bob = testContext.bob;

            // The seed's link language itself, not a template of it: this test
            // is about the neighbourhood store, so it avoids publishing another
            // language through the language-language.
            const perspective = await alice.perspective.add("shared-store neighbourhood");
            const url = await alice.neighbourhood.publishFromPerspective(perspective.uuid, DIFF_SYNC_HASH, new Perspective());

            const expression = await bob.expression.get(url);
            expect(expression, "Bob's neighbourhood store returned nothing").to.not.be.null;
            expect(expression.author).to.equal((await alice.agent.me()).did);
            expect(JSON.parse(expression.data).linkLanguage).to.equal(DIFF_SYNC_HASH);

            const joined = await bob.neighbourhood.joinFromUrl(url);
            expect(joined.sharedUrl).to.equal(url);
            expect(joined.neighbourhood!.data.linkLanguage).to.equal(DIFF_SYNC_HASH);

            const address = url.split("://")[1];
            expect(fs.existsSync(path.join(SHARED_NEIGHBOURHOODS_DIR, `neighbourhood-${address}.json`))).to.be.true;
        });

        // Agent profiles: tests/agent-language.ts "works across remote agents"
        // (run un-skipped in the local suite) covers Bob reading Alice's
        // profile by DID through the shared agent-language.

        describe("without a storagePath setting (KV mode)", () => {
            let client: Ad4mClient;
            let executor: any;
            let ports: number[];

            before(async () => {
                ports = await getFreePorts(3);
                registerPorts(ports);
                const [apiPort, hcAdminPort, hcAppPort] = ports;
                executor = await startExecutor(
                    path.join(TEST_DIR, "agents", "kv-store"),
                    path.join(__dirname, "..", "bootstrapSeed.json"),
                    apiPort, hcAdminPort, hcAppPort,
                    true,        // languageLanguageOnly: no other system language is published in its KV
                    undefined, undefined, undefined, undefined,
                    false, undefined, false,
                    false,       // runHolochain
                    false,       // sharedStores
                );
                client = new Ad4mClient(baseUrl(apiPort));
                await client.agent.generate("passphrase");
            });

            after(async () => {
                client?.close();
                if (executor) await quitExecutor(executor, ports[0]);
                deregisterPorts(ports);
            });

            it("keeps published languages in the executor's own store", async () => {
                const published = await client.languages.publish(uniqueBundle("kv"),
                    new LanguageMetaInput("kv-store language", ""));

                // languages.meta, not expression.get("lang://..."): the `lang`
                // alias is only registered when all system languages load.
                const own = await client.languages.meta(published.address);
                expect(own.name).to.equal("kv-store language");

                expect(sharedLanguageExists(published.address)).to.be.false;
                let aliceError: any = null;
                try {
                    await testContext.alice.languages.meta(published.address);
                } catch (e) {
                    aliceError = e;
                }
                expect(aliceError, "Alice found a language published in another executor's KV").to.not.be.null;
            });
        });
    };
}
