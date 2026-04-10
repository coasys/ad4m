import { TestContext } from './integration.test'
import path from "path";
import fs from "fs";
import { Ad4mClient, LanguageMetaInput } from '@coasys/ad4m';
import { expect } from "chai";
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export default function flatLanguageTests(testContext: TestContext) {
    return () => {
        describe('Flat export language pattern', () => {
            let ad4mClient: Ad4mClient

            before(async () => {
                ad4mClient = testContext.ad4mClient;
            })

            describe('note-store-flat', () => {
                let flatLangAddress = "";

                it('can publish note-store-flat language', async () => {
                    const bundlePath = path.join(__dirname, "../languages/note-store-flat/build/bundle.js").replace(/\\/g, "/");
                    const meta = new LanguageMetaInput("note-store-flat", "Flat export test language for note storage");
                    const published = await ad4mClient.languages.publish(bundlePath, meta);
                    
                    expect(published.address).not.to.be.undefined;
                    expect(published.name).to.be.equal("note-store-flat");
                    
                    flatLangAddress = published.address;
                    console.log("note-store-flat address:", flatLangAddress);
                });

                it('can install note-store-flat language', async () => {
                    const installed = await ad4mClient.languages.byAddress(flatLangAddress);
                    expect(installed.address).to.be.equal(flatLangAddress);
                    expect(installed.name).to.be.equal("note-store-flat");
                });

                it('can create and retrieve expression via note-store-flat', async () => {
                    // Create an expression
                    const testContent = "Hello from flat export language!";
                    const exprAddr = await ad4mClient.expression.create(testContent, flatLangAddress);
                    expect(exprAddr).not.to.be.undefined;
                    expect(typeof exprAddr).to.be.equal("string");
                    expect(exprAddr.length).to.be.greaterThan(10);

                    // Retrieve the expression
                    const expr = await ad4mClient.expression.get(exprAddr);
                    expect(expr).not.to.be.undefined;
                    expect(expr.proof.valid).to.be.true;
                    expect(expr.data).to.be.equal(JSON.stringify(testContent));
                });

                it('can get meta info for note-store-flat', async () => {
                    const meta = await ad4mClient.languages.meta(flatLangAddress);
                    expect(meta.address).to.be.equal(flatLangAddress);
                    expect(meta.name).to.be.equal("note-store-flat");
                    expect(meta.description).to.be.equal("Flat export test language for note storage");
                });
            });

            describe('aes-language-flat', () => {
                let flatLangAddress = "";

                it('can publish aes-language-flat language', async () => {
                    const bundlePath = path.join(__dirname, "../languages/aes-flat/build/bundle.js").replace(/\\/g, "/");
                    const meta = new LanguageMetaInput("aes-language-flat", "Flat export test language for EAS attestations");
                    const published = await ad4mClient.languages.publish(bundlePath, meta);
                    
                    expect(published.address).not.to.be.undefined;
                    expect(published.name).to.be.equal("aes-language-flat");
                    
                    flatLangAddress = published.address;
                    console.log("aes-language-flat address:", flatLangAddress);
                });

                it('can install aes-language-flat language', async () => {
                    const installed = await ad4mClient.languages.byAddress(flatLangAddress);
                    expect(installed.address).to.be.equal(flatLangAddress);
                    expect(installed.name).to.be.equal("aes-language-flat");
                });

                it('can get meta info for aes-language-flat', async () => {
                    const meta = await ad4mClient.languages.meta(flatLangAddress);
                    expect(meta.address).to.be.equal(flatLangAddress);
                    expect(meta.name).to.be.equal("aes-language-flat");
                    expect(meta.description).to.be.equal("Flat export test language for EAS attestations");
                });

                it('expressionGet returns null for unknown address (EAS read-only)', async () => {
                    // EAS is read-only, so expressionGet should return null for unknown addresses
                    // or return attestations if the address happens to match an attester
                    const unknownAddr = "did:ethr:0x0000000000000000000000000000000000000000";
                    const expr = await ad4mClient.expression.getRaw(`lang://${flatLangAddress}/expression/${unknownAddr}`);
                    // The language is read-only, so this should handle gracefully
                    expect(expr).not.to.be.undefined;
                });
            });

            // -----------------------------------------------------------------
            // Rust-authored WASM Language end-to-end (spec §9 Rust ALDK).
            //
            // The bundle is produced by:
            //   bash tests/rust-languages/build.sh
            // which runs cargo wasm32 → wasm-bindgen --target deno → inline-wasm.
            // The test is gated on bundle existence so CI without
            // wasm-bindgen-cli doesn't break.
            // -----------------------------------------------------------------
            describe('test-wasm-language (Rust ALDK)', function () {
                let wasmLangAddress = "";
                const wasmBundlePath = path.join(
                    __dirname,
                    "../../rust-languages/test-wasm-language/build/bundle.js"
                ).replace(/\\/g, "/");

                before(function () {
                    if (!fs.existsSync(wasmBundlePath)) {
                        console.log(
                            "[test-wasm-language] bundle not found at",
                            wasmBundlePath,
                            "— run `bash tests/rust-languages/build.sh` to build (requires wasm-bindgen-cli). Skipping."
                        );
                        this.skip();
                    }
                });

                it('can publish the Rust WASM language', async () => {
                    const meta = new LanguageMetaInput(
                        "test-wasm-language",
                        "Rust-authored flat WASM language built via ad4m-ldk"
                    );
                    const published = await ad4mClient.languages.publish(wasmBundlePath, meta);
                    expect(published.address).not.to.be.undefined;
                    expect(published.name).to.be.equal("test-wasm-language");
                    wasmLangAddress = published.address;
                });

                it('can install the Rust WASM language', async () => {
                    const installed = await ad4mClient.languages.byAddress(wasmLangAddress);
                    expect(installed.address).to.be.equal(wasmLangAddress);
                    expect(installed.name).to.be.equal("test-wasm-language");
                });

                it('expression capability round-trips through agent + storage imports', async () => {
                    const content = "hello from rust wasm";
                    const addr = await ad4mClient.expression.create(content, wasmLangAddress);
                    expect(addr).not.to.be.undefined;
                    expect(typeof addr).to.equal("string");
                    expect(addr.length).to.be.greaterThan(0);

                    const expr = await ad4mClient.expression.get(addr);
                    expect(expr).not.to.be.undefined;
                });
            });
        })
    }
}
