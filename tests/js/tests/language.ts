import { TestContext } from './full-integration.test'
import path from "path";
import fs from "fs";
import { sleep } from '../utils/utils';
import { Ad4mClient, LanguageMetaInput, LanguageRef } from '@perspect3vism/ad4m';
import { expect } from "chai";
import { fileURLToPath } from 'url';
import stringify from 'json-stable-stringify'

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export default function languageTests(testContext: TestContext) {
    return () => {
        describe('with a perspective-diff-sync templated by Alice', () => {
            let ad4mClient: Ad4mClient
            let bobAd4mClient: Ad4mClient
            let sourceLanguage: LanguageRef = new LanguageRef()
            let nonHCSourceLanguage: LanguageRef = new LanguageRef()
            let sourceLanguageMeta: LanguageMetaInput = new LanguageMetaInput("Newly published perspective-diff-sync", "..here for you template");
            sourceLanguageMeta.possibleTemplateParams = ["id", "description", "name"];

            before(async () => {
                ad4mClient = testContext.ad4mClient;
                bobAd4mClient = testContext.bob;

                //First edit bundle for perspective-diff-sync so we get a unique hash which does not clash with existing loaded perspective-diff-sync object in LanguageController
                let socialContextData = fs.readFileSync("./tst-tmp/languages/perspective-diff-sync/build/bundle.js").toString();
                socialContextData = socialContextData + "\n//Test";
                fs.writeFileSync("./tst-tmp/languages/perspective-diff-sync/build/bundle.js", socialContextData);

                //Publish a source language to start working from
                sourceLanguage = await ad4mClient.languages.publish(
                    path.join(__dirname, "../tst-tmp/languages/perspective-diff-sync/build/bundle.js"),
                    sourceLanguageMeta
                )
                expect(sourceLanguage.name).to.be.equal(sourceLanguageMeta.name);
                // @ts-ignore
                sourceLanguageMeta.address = sourceLanguage.address;
            })

            it('Alice can get the source of her own templated language', async () => {
                const sourceFromAd4m = await ad4mClient.languages.source(sourceLanguage.address)
                const sourceFromFile = fs.readFileSync(path.join(__dirname, "../tst-tmp/languages/perspective-diff-sync/build/bundle.js")).toString()
                expect(sourceFromAd4m).to.be.equal(sourceFromFile)
            })

            it('Alice can install her own published language', async () => {
                const install = await ad4mClient.languages.byAddress(sourceLanguage.address);
                expect(install.address).not.to.be.undefined;
                expect(install.constructorIcon).to.be.null;
                expect(install.settingsIcon).not.to.be.undefined;
            })

            it('Alice can install her own non HC published language', async () => {
                let sourceLanguageMeta: LanguageMetaInput = new LanguageMetaInput("Newly published perspective-language", "..here for you template");
                let socialContextData = fs.readFileSync("./tst-tmp/languages/perspective-language/build/bundle.js").toString();
                socialContextData = socialContextData + "\n//Test";
                fs.writeFileSync("./tst-tmp/languages/perspective-language/build/bundle.js", socialContextData);

                //Publish a source language to start working from
                nonHCSourceLanguage = await ad4mClient.languages.publish(
                    path.join(__dirname, "../tst-tmp/languages/perspective-language/build/bundle.js"),
                    sourceLanguageMeta
                )
                expect(nonHCSourceLanguage.name).to.be.equal(nonHCSourceLanguage.name);

                const install = await ad4mClient.languages.byAddress(nonHCSourceLanguage.address);
                expect(install.address).not.to.be.undefined;
                expect(install.constructorIcon).not.to.be.null;
                expect(install.icon).not.to.be.null;
                expect(install.settingsIcon).to.be.null;
            })

            it('Alice can use language.meta() to get meta info of her Language', async() => {
                const meta = await ad4mClient.languages.meta(sourceLanguage.address)
                expect(meta.address).to.be.equal(sourceLanguage.address)
                expect(meta.name).to.be.equal(sourceLanguageMeta.name)
                expect(meta.description).to.be.equal(sourceLanguageMeta.description)
                expect(meta.author).to.be.equal((await ad4mClient.agent.status()).did)
                expect(meta.templated).to.be.false;
            })

            it('Alice can get her own templated perspective-diff-sync and it provides correct meta data', async () => {
                //Get the meta of the source language and make sure it is correct
                const foundSourceLanguageMeta = await ad4mClient.expression.get(`lang://${sourceLanguage.address}`);
                expect(foundSourceLanguageMeta.proof.valid).to.be.true;
                const sourceLanguageMetaData = JSON.parse(foundSourceLanguageMeta.data);
                expect(sourceLanguageMetaData.name).to.be.equal(sourceLanguageMeta.name)
                expect(sourceLanguageMetaData.description).to.be.equal(sourceLanguageMeta.description)
            })

            it('can publish and template a non-Holochain language and provide correct meta data', async() => {
                const noteMetaInfo = new LanguageMetaInput("Newly published note language", "Just to test non-HC language work as well");
                //Publish a source language without a holochain DNA
                const canPublishNonHolochainLang = await ad4mClient.languages.publish(
                    path.join(__dirname, "../languages/note-store/build/bundle.js"), 
                    noteMetaInfo
                );
                expect(canPublishNonHolochainLang.name).to.be.equal(noteMetaInfo.name);
                //TODO/NOTE: this will break if the note language version is changed
                expect(canPublishNonHolochainLang.address).to.be.equal("QmzSYwdmAKKivFm8sp7zLDPnT2kwyk2to8Ey1hqEb25DCPaJoRA");
            
                //Get meta for source language above and make sure it is correct
                const sourceLanguageMetaNonHC = await ad4mClient.expression.get(`lang://${canPublishNonHolochainLang.address}`);
                expect(sourceLanguageMetaNonHC.proof.valid).to.be.true;
                const sourceLanguageMetaNonHCData = JSON.parse(sourceLanguageMetaNonHC.data);
                expect(sourceLanguageMetaNonHCData.name).to.be.equal(noteMetaInfo.name)
                expect(sourceLanguageMetaNonHCData.description).to.be.equal(noteMetaInfo.description)
                expect(sourceLanguageMetaNonHCData.address).to.be.equal("QmzSYwdmAKKivFm8sp7zLDPnT2kwyk2to8Ey1hqEb25DCPaJoRA")
            })


            it("Bob can not get/install Alice's language since he doesn't trust her yet", async () => {
                // Make sure all new Holochain cells get connected..
                await testContext.makeAllNodesKnown()
                // .. and have time to gossip inside the Language Language, 
                // so Bob sees the languages created above by Alice
                await sleep(1000);
                

                //Test that bob cannot install source language which alice created since she is not in his trusted agents
                let error
                try {
                    await bobAd4mClient.languages.byAddress(sourceLanguage.address);
                } catch(e) {
                    error = e
                }

                console.log("Response for non trust got error", error);

                //@ts-ignore
                sourceLanguageMeta.sourceCodeLink = null;

                //@ts-ignore
                expect(error.toString()).to.contain(`ApolloError: Language not created by trusted agent: ${(await ad4mClient.agent.me()).did} and is not templated... aborting language install. Language metadata: ${stringify(sourceLanguageMeta)}`)
            })

            describe('with Bob having added Alice to list of trusted agents', () => {
                let applyTemplateFromSource: LanguageRef = new LanguageRef()

                before(async () => {
                    //Add alice as trusted agent for bob
                    const aliceDid = await ad4mClient.agent.me();
                    const aliceTrusted = await bobAd4mClient.runtime.addTrustedAgents([aliceDid.did]);
                    console.warn("Got result when adding trusted agent", aliceTrusted);
                })

                it("Bob can template Alice's perspective-diff-sync, and alice can install", async () => {
                    //Apply template on above holochain language
                    applyTemplateFromSource = await bobAd4mClient.languages.applyTemplateAndPublish(sourceLanguage.address, JSON.stringify({uid: "2eebb82b-9db1-401b-ba04-1e8eb78ac84c", name: "Bob's templated perspective-diff-sync"}))
                    expect(applyTemplateFromSource.name).to.be.equal("Bob's templated perspective-diff-sync");
                    expect(applyTemplateFromSource.address).not.be.equal(sourceLanguage.address);
                    
                    //Get language meta for above language and make sure it is correct
                    const langExpr = await bobAd4mClient.expression.get(`lang://${applyTemplateFromSource.address}`);
                    expect(langExpr.proof.valid).to.be.true;
                    const meta = await bobAd4mClient.languages.meta(applyTemplateFromSource.address);
                    expect(meta.name).to.be.equal("Bob's templated perspective-diff-sync")
                    expect(meta.description).to.be.equal("..here for you template")
                    expect(meta.author).to.be.equal((await bobAd4mClient.agent.status()).did)
                    expect(meta.templateAppliedParams).to.be.equal(JSON.stringify({
                        "name": "Bob's templated perspective-diff-sync",
                        "uid":"2eebb82b-9db1-401b-ba04-1e8eb78ac84c"
                    }))
                    expect(meta.templateSourceLanguageAddress).to.be.equal(sourceLanguage.address)

                    await ad4mClient.runtime.addTrustedAgents([(await bobAd4mClient.agent.me()).did]);

                    const installGetLanguage = await ad4mClient.languages.byAddress(applyTemplateFromSource.address);
                    expect(installGetLanguage.address).to.be.equal(applyTemplateFromSource.address);
                    expect(installGetLanguage.name).to.be.equal(meta.name);
                })

                it("Bob can install Alice's perspective-diff-sync", async () => {
                    //Test that bob can install source language when alice is in trusted agents
                    const installSourceTrusted = await bobAd4mClient.languages.byAddress(sourceLanguage.address);
                    expect(installSourceTrusted.address).to.be.equal(sourceLanguage.address);
                    expect(installSourceTrusted.constructorIcon).not.to.be.undefined;
                    expect(installSourceTrusted.settingsIcon).not.to.be.undefined;
                })

                it("Bob can install his own templated language", async () => {
                    //Test that bob can install language which is templated from source since source language was created by alice who is now a trusted agent
                    const installTemplated = await bobAd4mClient.languages.byAddress(applyTemplateFromSource.address);
                    expect(installTemplated.address).to.be.equal(applyTemplateFromSource.address);
                    expect(installTemplated.name).to.be.equal("Bob's templated perspective-diff-sync");
                    expect(installTemplated.constructorIcon).not.to.be.undefined;
                    expect(installTemplated.settingsIcon).not.to.be.undefined;
                })

                it('Bob can delete a language', async () => {
                    const deleteLanguage = await bobAd4mClient.languages.remove(sourceLanguage.address);
                    expect(deleteLanguage).to.be.true;
                })
            })
         })
    }
}
