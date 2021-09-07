import { TestContext } from './integration.test'
import path from "path";
import sleep from './sleep';

export default function languageTests(testContext: TestContext) {
    return () => {
        describe('basic language operations', () => {
            it('can apply language template to holochain language', async () => {
                const ad4mClient = testContext.ad4mClient;
                const bobAd4mClient = testContext.bob;

                //Publish a source language to start working from
                const sourceLanguage = await ad4mClient.languages.publish(path.join(__dirname, "../test-temp/languages/social-context/build/bundle.js"), JSON.stringify({uid: "001eb380-0dba-48fb-86d8-b3e58ea71b00", name: "A templated social-context"}))
                expect(sourceLanguage.name).toBe("A templated social-context");
                //TODO/NOTE: this will break if the social-context language version is changed
                expect(sourceLanguage.address).toBe("QmU2CmiD6wKExCZ2tfEfcY17c5xD6iNevLTsKBu5AF6Sqd");

                //Get the meta of the source language and make sure it is correct
                const sourceLanguageMeta = await ad4mClient.expression.get(`lang://${sourceLanguage.address}`);
                expect(sourceLanguageMeta.proof.valid).toBe(true);
                const sourceLanguageMetaData = JSON.parse(sourceLanguageMeta.data);
                expect(sourceLanguageMetaData).toBe(JSON.stringify({"name":"A templated social-context","description":null,"templateParams":{"name":"A templated social-context","uid":"001eb380-0dba-48fb-86d8-b3e58ea71b00"},"hash":"QmU2CmiD6wKExCZ2tfEfcY17c5xD6iNevLTsKBu5AF6Sqd","sourceLanguageHash":null,"dnaYamlHash":"QmdEHSaBxfEKE2BRSN7MLevPg3KiUaQkbaCBMB2mdCuWFi","dnaZomeWasmHash":"QmWiBzrV1qGwnWuH8ktawuTVqVTb3YEfGtg1KpaxJUG85Z"}))

                //Publish a source language without a holochain DNA
                const canPublishNonHolochainLang = await ad4mClient.languages.publish(path.join(__dirname, "../test-temp/languages/note-ipfs/build/bundle.js"), JSON.stringify({uid: "001eb380-0dba-48fb-86d8-b3e58ea71b00", name: "A templated note language"}))
                expect(canPublishNonHolochainLang.name).toBe("A templated note language");
                //TODO/NOTE: this will break if the note-ipfs language version is changed
                expect(canPublishNonHolochainLang.address).toBe("QmU4ZoFvxXn4i16CcaU3ZBMLk2RM33HdQLjYWywoYKtWc7");
            
                //Get meta for source language above and make sure it is correct
                const sourceLanguageMetaNonHC = await ad4mClient.expression.get(`lang://${canPublishNonHolochainLang.address}`);
                expect(sourceLanguageMetaNonHC.proof.valid).toBe(true);
                const sourceLanguageMetaNonHCData = JSON.parse(sourceLanguageMetaNonHC.data);
                expect(sourceLanguageMetaNonHCData).toBe(JSON.stringify({"name":"A templated note language","description":null,"templateParams":{"name":"A templated note language","uid":"001eb380-0dba-48fb-86d8-b3e58ea71b00"},"hash":"QmU4ZoFvxXn4i16CcaU3ZBMLk2RM33HdQLjYWywoYKtWc7","sourceLanguageHash":null,"dnaYamlHash":null,"dnaZomeWasmHash":null}));
            
                await sleep(5000);

                //Apply template on above holochain language
                const applyTemplateFromSource = await bobAd4mClient.languages.applyTemplateAndPublish(sourceLanguage.address, JSON.stringify({uid: "2eebb82b-9db1-401b-ba04-1e8eb78ac84c", name: "A templated templated social-context"}))
                expect(applyTemplateFromSource.name).toBe("A templated templated social-context");
                expect(applyTemplateFromSource.address).toBe("QmW196oBc3CncuUFMJqJEL8A5ZwCYDfXTKQQYgfrN1Qs5N");
                
                //Get language meta for above language and make sure it is correct
                const applyTemplateFromSourceMeta = await bobAd4mClient.expression.get(`lang://${applyTemplateFromSource.address}`);
                expect(applyTemplateFromSourceMeta.proof.valid).toBe(true);
                const applyTemplateFromSourceMetaData = JSON.parse(applyTemplateFromSourceMeta.data);
                expect(applyTemplateFromSourceMetaData).toBe(JSON.stringify({
                    "name": "A templated templated social-context",
                    "description": null,
                    "templateParams": {
                        "name": "A templated templated social-context",
                        "uid":"2eebb82b-9db1-401b-ba04-1e8eb78ac84c"
                    },
                    "hash": "QmW196oBc3CncuUFMJqJEL8A5ZwCYDfXTKQQYgfrN1Qs5N",
                    "sourceLanguageHash": "QmU2CmiD6wKExCZ2tfEfcY17c5xD6iNevLTsKBu5AF6Sqd",
                    "dnaYamlHash": "QmS6yUbyM565LtDG2M4JqTCXdzXqychVqWaZseKpXzhWRA",
                    "dnaZomeWasmHash": "QmWiBzrV1qGwnWuH8ktawuTVqVTb3YEfGtg1KpaxJUG85Z"
                }));

                //Test that bob cannot install source language which alice created since she is not in his trusted agents
                const installSource = await bobAd4mClient.languages.byAddress(sourceLanguage.address);
                console.warn("Got result when trying to install source language", installSource);

                //Add alice as trusted agent for bob
                const aliceDid = await ad4mClient.agent.me();
                const aliceTrusted = await bobAd4mClient.runtime.addTrustedAgents([aliceDid.did]);
                console.warn("Got result when adding trusted agent", aliceTrusted);

                //Test that bob can install source language when alice is in trusted agents
                const installSourceTrusted = await bobAd4mClient.languages.byAddress(sourceLanguage.address);
                console.warn("Got result when trying to install source language after trusted", installSourceTrusted);
                
                //Test that bob can install language which is templated from source since source language was created by alice who is now a trusted agent
                const installTemplated = await bobAd4mClient.languages.byAddress(applyTemplateFromSource.address);
                console.warn("Got result when trying to install language which was templated from source", installTemplated);
            })

            // it('can get and create unique language', async () => {
            //     const ad4mClient = testContext.ad4mClient!
                
            //     const languages = await ad4mClient.languages.byFilter("");
            //     expect(languages.length).toBe(4);

            //     const createUniqueLang = await ad4mClient.languages.cloneHolochainTemplate(path.join(__dirname, "../test-temp/languages/agent-expression-store"), "agent-store", "b98e53a8-5800-47b6-adb9-86d55a74871e");
            //     expect(createUniqueLang.name).toBe("agent-expression-store");

            //     const installLanguage = await ad4mClient.languages.byAddress(createUniqueLang.address);
            //     expect(installLanguage.name).toBe("agent-expression-store");

            //     expect(await async function() {
            //         let me = await ad4mClient.agent.me()
            //         me.directMessageLanguage = "test"
            //         //Test that we can use the language after creating the unique version
            //         await ad4mClient.expression.create(me, installLanguage.address!)
            //     }).not.toThrow()

            //     const languagesPostInstall = await ad4mClient.languages.byFilter("");
            //     expect(languagesPostInstall.length).toBe(5);

            //     const writeSettings = await ad4mClient.languages.writeSettings(createUniqueLang.address, JSON.stringify({"setting": "test"}));
            //     expect(writeSettings).toBe(true);

            //     const language = await ad4mClient.languages.byAddress(createUniqueLang.address);
            //     expect(language.settings).toBeDefined()
            //     expect(JSON.parse(language.settings!).setting).toBe("test");
            // })
        })
    }
}