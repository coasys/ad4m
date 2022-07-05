import { TestContext } from './integration.test'
import path from "path";
import fs from "fs";
import sleep from './sleep';
import { Ad4mClient, LanguageMetaInput, LanguageRef } from '@perspect3vism/ad4m';

export default function languageTests(testContext: TestContext) {
    return () => {
        describe('with a perspective-diff-sync templated by Alice', () => {
            let ad4mClient: Ad4mClient
            let bobAd4mClient: Ad4mClient
            let sourceLanguage: LanguageRef = new LanguageRef()
            let nonHCSourceLanguage: LanguageRef = new LanguageRef()
            let sourceLanguageMeta: LanguageMetaInput = new LanguageMetaInput("Newly published perspective-diff-sync", "..here for you template");
            sourceLanguageMeta.possibleTemplateParams = ["id", "description", "name"];

            beforeAll(async () => {
                ad4mClient = testContext.ad4mClient;
                bobAd4mClient = testContext.bob;

                //First edit bundle for perspective-diff-sync so we get a unique hash which does not clash with existing loaded perspective-diff-sync object in LanguageController
                let socialContextData = fs.readFileSync("./src/test-temp/languages/perspective-diff-sync/build/bundle.js").toString();
                socialContextData = socialContextData + "\n//Test";
                fs.writeFileSync("./src/test-temp/languages/perspective-diff-sync/build/bundle.js", socialContextData);

                //Publish a source language to start working from
                sourceLanguage = await ad4mClient.languages.publish(
                    path.join(__dirname, "../test-temp/languages/perspective-diff-sync/build/bundle.js"),
                    sourceLanguageMeta
                )
                expect(sourceLanguage.name).toBe(sourceLanguageMeta.name);
            })

            it('Alice can get the source of her own templated language', async () => {
                const sourceFromAd4m = await ad4mClient.languages.source(sourceLanguage.address)
                const sourceFromFile = fs.readFileSync(path.join(__dirname, "../test-temp/languages/perspective-diff-sync/build/bundle.js")).toString()
                expect(sourceFromAd4m).toBe(sourceFromFile)
            })

            it('Alice can install her own published language', async () => {
                const install = await ad4mClient.languages.byAddress(sourceLanguage.address);
                expect(install.address).toBeDefined();
                expect(install.constructorIcon).toBeNull();
                expect(install.settingsIcon).toBeDefined();
            })

            it('Alice can install her own non HC published language', async () => {
                let sourceLanguageMeta: LanguageMetaInput = new LanguageMetaInput("Newly published perspective-language", "..here for you template");
                let socialContextData = fs.readFileSync("./src/test-temp/languages/perspective-language/build/bundle.js").toString();
                socialContextData = socialContextData + "\n//Test";
                fs.writeFileSync("./src/test-temp/languages/perspective-language/build/bundle.js", socialContextData);

                //Publish a source language to start working from
                nonHCSourceLanguage = await ad4mClient.languages.publish(
                    path.join(__dirname, "../test-temp/languages/perspective-language/build/bundle.js"),
                    sourceLanguageMeta
                )
                expect(nonHCSourceLanguage.name).toBe(nonHCSourceLanguage.name);

                const install = await ad4mClient.languages.byAddress(nonHCSourceLanguage.address);
                expect(install.address).toBeDefined();
                expect(install.constructorIcon).not.toBeNull();
                expect(install.icon).not.toBeNull();
                expect(install.settingsIcon).toBeNull();
            })

            it('Alice can use language.meta() to get meta info of her Language', async() => {
                const meta = await ad4mClient.languages.meta(sourceLanguage.address)
                expect(meta.address).toBe(sourceLanguage.address)
                expect(meta.name).toBe(sourceLanguageMeta.name)
                expect(meta.description).toBe(sourceLanguageMeta.description)
                expect(meta.author).toBe((await ad4mClient.agent.status()).did)
                expect(meta.templated).toBeFalsy()
            })

            it('Alice can get her own templated perspective-diff-sync and it provides correct meta data', async () => {
                //Get the meta of the source language and make sure it is correct
                const foundSourceLanguageMeta = await ad4mClient.expression.get(`lang://${sourceLanguage.address}`);
                expect(foundSourceLanguageMeta.proof.valid).toBe(true);
                const sourceLanguageMetaData = JSON.parse(foundSourceLanguageMeta.data);
                expect(sourceLanguageMetaData.name).toBe(sourceLanguageMeta.name)
                expect(sourceLanguageMetaData.description).toBe(sourceLanguageMeta.description)
            })

            it('can publish and template a non-Holochain language and provide correct meta data', async() => {
                const ipfsMetaInfo = new LanguageMetaInput("Newly published note language", "Just to test non-HC language work as well");
                //Publish a source language without a holochain DNA
                const canPublishNonHolochainLang = await ad4mClient.languages.publish(
                    path.join(__dirname, "../test-temp/languages/note-ipfs/build/bundle.js"), 
                    ipfsMetaInfo
                );
                expect(canPublishNonHolochainLang.name).toBe(ipfsMetaInfo.name);
                //TODO/NOTE: this will break if the note-ipfs language version is changed
                expect(canPublishNonHolochainLang.address).toBe("QmbWg5VBFB1Zzce8X33GiGpMDXFPQjFQKS2T2rJtSYt7TJ");
            
                //Get meta for source language above and make sure it is correct
                const sourceLanguageMetaNonHC = await ad4mClient.expression.get(`lang://${canPublishNonHolochainLang.address}`);
                expect(sourceLanguageMetaNonHC.proof.valid).toBe(true);
                const sourceLanguageMetaNonHCData = JSON.parse(sourceLanguageMetaNonHC.data);
                expect(sourceLanguageMetaNonHCData.name).toBe(ipfsMetaInfo.name)
                expect(sourceLanguageMetaNonHCData.description).toBe(ipfsMetaInfo.description)
                expect(sourceLanguageMetaNonHCData.address).toBe("QmbWg5VBFB1Zzce8X33GiGpMDXFPQjFQKS2T2rJtSYt7TJ")
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

                //@ts-ignore
                expect(error.toString()).toBe(`Error: Language not created by trusted agent and is not templated... aborting language install. Language metadata: ${sourceLanguageMeta}`)
            })

            describe('with Bob having added Alice to list of trusted agents', () => {
                let applyTemplateFromSource: LanguageRef = new LanguageRef()

                beforeAll(async () => {
                    //Add alice as trusted agent for bob
                    const aliceDid = await ad4mClient.agent.me();
                    const aliceTrusted = await bobAd4mClient.runtime.addTrustedAgents([aliceDid.did]);
                    console.warn("Got result when adding trusted agent", aliceTrusted);
                })

                it("Bob can template Alice's perspective-diff-sync, and alice can install", async () => {
                    //Apply template on above holochain language
                    applyTemplateFromSource = await bobAd4mClient.languages.applyTemplateAndPublish(sourceLanguage.address, JSON.stringify({uid: "2eebb82b-9db1-401b-ba04-1e8eb78ac84c", name: "Bob's templated perspective-diff-sync"}))
                    expect(applyTemplateFromSource.name).toBe("Bob's templated perspective-diff-sync");
                    expect(applyTemplateFromSource.address).not.toEqual(sourceLanguage.address);
                    
                    //Get language meta for above language and make sure it is correct
                    const langExpr = await bobAd4mClient.expression.get(`lang://${applyTemplateFromSource.address}`);
                    expect(langExpr.proof.valid).toBe(true);
                    const meta = await bobAd4mClient.languages.meta(applyTemplateFromSource.address);
                    expect(meta.name).toBe("Bob's templated perspective-diff-sync")
                    expect(meta.description).toBe("..here for you template")
                    expect(meta.author).toBe((await bobAd4mClient.agent.status()).did)
                    expect(meta.templateAppliedParams).toBe(JSON.stringify({
                        "name": "Bob's templated perspective-diff-sync",
                        "uid":"2eebb82b-9db1-401b-ba04-1e8eb78ac84c"
                    }))
                    expect(meta.templateSourceLanguageAddress).toBe(sourceLanguage.address)

                    await ad4mClient.runtime.addTrustedAgents([(await bobAd4mClient.agent.me()).did]);

                    const installGetLanguage = await ad4mClient.languages.byAddress(applyTemplateFromSource.address);
                    expect(installGetLanguage.address).toBe(applyTemplateFromSource.address);
                    //expect(installGetLanguage.name).toBe(meta.name);
                })

                it("Bob can install Alice's perspective-diff-sync", async () => {
                    //Test that bob can install source language when alice is in trusted agents
                    const installSourceTrusted = await bobAd4mClient.languages.byAddress(sourceLanguage.address);
                    expect(installSourceTrusted.address).toBe(sourceLanguage.address);
                    expect(installSourceTrusted.constructorIcon).toBeDefined();
                    expect(installSourceTrusted.settingsIcon).toBeDefined();
                })

                it("Bob can install his own templated language", async () => {
                    //Test that bob can install language which is templated from source since source language was created by alice who is now a trusted agent
                    const installTemplated = await bobAd4mClient.languages.byAddress(applyTemplateFromSource.address);
                    expect(installTemplated.address).toBe(applyTemplateFromSource.address);
                    //expect(installTemplated.name).toBe("Bob's templated perspective-diff-sync");
                    expect(installTemplated.constructorIcon).toBeDefined();
                    expect(installTemplated.settingsIcon).toBeDefined();
                })
            })
         })
    }
}
