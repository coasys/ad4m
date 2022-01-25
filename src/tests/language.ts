import { TestContext } from './integration.test'
import path from "path";
import fs from "fs";
import sleep from './sleep';
import { Ad4mClient, LanguageMetaInput, LanguageRef } from '@perspect3vism/ad4m';

export default function languageTests(testContext: TestContext) {
    return () => {
        describe('with a social-context templated by Alice', () => {
            let ad4mClient: Ad4mClient
            let bobAd4mClient: Ad4mClient
            let sourceLanguage: LanguageRef = new LanguageRef()

            beforeAll(async () => {
                ad4mClient = testContext.ad4mClient;
                bobAd4mClient = testContext.bob;

                //Publish a source language to start working from
                sourceLanguage = await ad4mClient.languages.publish(
                    path.join(__dirname, "../test-temp/languages/social-context/build/bundle.js"),
                    new LanguageMetaInput("Newly published social-context", "..here for you template")
                )
                expect(sourceLanguage.name).toBe("Newly published social-context");
                //TODO/NOTE: this will break if the social-context language version is changed
                expect(sourceLanguage.address).toBe("QmfDoeJgiG5Hs4DJcwPqDWbwU2Ks8zLSJjv7bR8is84Qt5");
            })

            it('Alice can get the source of her own templated language', async () => {
                const sourceFromAd4m = await ad4mClient.languages.source(sourceLanguage.address)
                const sourceFromFile = fs.readFileSync(path.join(__dirname, "../test-temp/languages/social-context/build/bundle.js")).toString()
                expect(sourceFromAd4m).toBe(sourceFromFile)
            })

            it('Alice can install her own templated language', async () => {
                await ad4mClient.languages.byAddress(sourceLanguage.address)
            })

            it('Alice can use language.meta() to get meta info of her Language', async() => {
                const meta = await ad4mClient.languages.meta(sourceLanguage.address)
                expect(meta.address).toBe(sourceLanguage.address)
                expect(meta.name).toBe("Newly published social-context")
                expect(meta.description).toBe("..here for you template")
                expect(meta.author).toBe((await ad4mClient.agent.status()).did)
                expect(meta.templated).toBeFalsy()
            })

            it('Alice can get her own templated social-context and it provides correct meta data', async () => {
                //Get the meta of the source language and make sure it is correct
                const sourceLanguageMeta = await ad4mClient.expression.get(`lang://${sourceLanguage.address}`);
                expect(sourceLanguageMeta.proof.valid).toBe(true);
                const sourceLanguageMetaData = JSON.parse(sourceLanguageMeta.data);
                expect(sourceLanguageMetaData.name).toBe("Newly published social-context")
                expect(sourceLanguageMetaData.description).toBe("..here for you template")
                expect(sourceLanguageMetaData.address).toBe("QmfDoeJgiG5Hs4DJcwPqDWbwU2Ks8zLSJjv7bR8is84Qt5")
            })

            it('can publish and template a non-Holochain language and provide correct meta data', async() => {
                //Publish a source language without a holochain DNA
                const canPublishNonHolochainLang = await ad4mClient.languages.publish(
                    path.join(__dirname, "../test-temp/languages/note-ipfs/build/bundle.js"), 
                    new LanguageMetaInput("Newly published note language", "Just to test non-HC language work as well")
                );
                expect(canPublishNonHolochainLang.name).toBe("Newly published note language");
                //TODO/NOTE: this will break if the note-ipfs language version is changed
                expect(canPublishNonHolochainLang.address).toBe("Qmd6AZzLjfGWNAqWLGTGy354JC1bK26XNf7rTEEsJfv7Fe");
            
                //Get meta for source language above and make sure it is correct
                const sourceLanguageMetaNonHC = await ad4mClient.expression.get(`lang://${canPublishNonHolochainLang.address}`);
                expect(sourceLanguageMetaNonHC.proof.valid).toBe(true);
                const sourceLanguageMetaNonHCData = JSON.parse(sourceLanguageMetaNonHC.data);
                expect(sourceLanguageMetaNonHCData.name).toBe("Newly published note language")
                expect(sourceLanguageMetaNonHCData.description).toBe("Just to test non-HC language work as well")
                expect(sourceLanguageMetaNonHCData.address).toBe("Qmd6AZzLjfGWNAqWLGTGy354JC1bK26XNf7rTEEsJfv7Fe")
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
                expect(error.toString()).toBe("Error: Language not created by trusted agent and is not templated... aborting language install")
            })

            describe('with Bob having added Alice to list of trusted agents', () => {
                let applyTemplateFromSource: LanguageRef = new LanguageRef()

                beforeAll(async () => {
                    //Add alice as trusted agent for bob
                    const aliceDid = await ad4mClient.agent.me();
                    const aliceTrusted = await bobAd4mClient.runtime.addTrustedAgents([aliceDid.did]);
                    console.warn("Got result when adding trusted agent", aliceTrusted);
                })

                it("Bob can template Alice's social-context, and alice can install", async () => {
                    //Apply template on above holochain language
                    applyTemplateFromSource = await bobAd4mClient.languages.applyTemplateAndPublish(sourceLanguage.address, JSON.stringify({uid: "2eebb82b-9db1-401b-ba04-1e8eb78ac84c", name: "Bob's templated social-context"}))
                    expect(applyTemplateFromSource.name).toBe("Bob's templated social-context");
                    expect(applyTemplateFromSource.address).toBe("QmUosE5MEVAfV48TFwfrXNoRRy4GNeB2eopfBFBGqrmB3f");
                    
                    //Get language meta for above language and make sure it is correct
                    const langExpr = await bobAd4mClient.expression.get(`lang://${applyTemplateFromSource.address}`);
                    expect(langExpr.proof.valid).toBe(true);
                    const meta = await bobAd4mClient.languages.meta(applyTemplateFromSource.address);
                    expect(meta.name).toBe("Bob's templated social-context")
                    expect(meta.description).toBe("..here for you template")
                    expect(meta.author).toBe((await bobAd4mClient.agent.status()).did)
                    expect(meta.templateAppliedParams).toBe(JSON.stringify({
                        "name": "Bob's templated social-context",
                        "uid":"2eebb82b-9db1-401b-ba04-1e8eb78ac84c"
                    }))
                    expect(meta.address).toBe("QmUosE5MEVAfV48TFwfrXNoRRy4GNeB2eopfBFBGqrmB3f")
                    expect(meta.templateSourceLanguageAddress).toBe(sourceLanguage.address)

                    await ad4mClient.runtime.addTrustedAgents([(await ad4mClient.agent.me()).did]);
                    await sleep(500)

                    const installGetLanguage = await ad4mClient.languages.byAddress(applyTemplateFromSource.address);
                    expect(installGetLanguage.address).toBe(applyTemplateFromSource.address);
                })

                it("Bob can install Alice's social-context", async () => {
                    //Test that bob can install source language when alice is in trusted agents
                    const installSourceTrusted = await bobAd4mClient.languages.byAddress(sourceLanguage.address);
                    //console.warn("Got result when trying to install source language after trusted", installSourceTrusted);
                })

                it("Bob can install his own templated language", async () => {
                    //Test that bob can install language which is templated from source since source language was created by alice who is now a trusted agent
                    const installTemplated = await bobAd4mClient.languages.byAddress(applyTemplateFromSource.address);
                    //console.warn("Got result when trying to install language which was templated from source", installTemplated);
                })
            })
        })
    }
}
