import { TestContext } from './integration.test'
import path from "path"

export default function languageTests(testContext: TestContext) {
    return () => {
        describe('basic language operations', () => {
            it('can get and create unique language', async () => {
                const ad4mClient = testContext.ad4mClient
                
                const languages = await ad4mClient.languages.byFilter("");
                expect(languages.length).toBe(4);

                const createUniqueLang = await ad4mClient.languages.cloneHolochainTemplate(path.join(__dirname, "../test-temp/agent-expression-store"), "agent-store", "b98e53a8-5800-47b6-adb9-86d55a74871e");
                expect(createUniqueLang.name).toBe("agent-expression-store");

                const installLanguage = await ad4mClient.languages.byAddress(createUniqueLang.address);
                expect(installLanguage.name).toBe("agent-expression-store");

                const languagesPostInstall = await ad4mClient.languages.byFilter("");
                expect(languagesPostInstall.length).toBe(5);

                const writeSettings = await ad4mClient.languages.writeSettings(createUniqueLang.address, JSON.stringify({"setting": "test"}));
                expect(writeSettings).toBe(true);

                const language = await ad4mClient.languages.byAddress(createUniqueLang.address);
                expect(JSON.parse(language.settings).setting).toBe("test");
            })
        })
    }
}