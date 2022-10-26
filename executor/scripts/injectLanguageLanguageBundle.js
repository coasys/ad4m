import fs from "fs";

const languageLanguagePath = "./src/test-temp/languages/languages/build/bundle.js";
const bootstrapSeedPath = "./src/tests/bootstrapSeed.json";
const publishingBootstrapSeedPath = "./src/tests/publishBootstrapSeed.json";

async function main() {
    if (fs.existsSync(languageLanguagePath)) {
        const bundleData = fs.readFileSync(languageLanguagePath).toString();
        if (fs.existsSync(bootstrapSeedPath)) {
            const bootstrapSeed = JSON.parse(fs.readFileSync(bootstrapSeedPath).toString());
            bootstrapSeed["languageLanguageBundle"] = bundleData;
            fs.writeFileSync(bootstrapSeedPath, JSON.stringify(bootstrapSeed));
        } else {
            throw new Error(`Could not find boostrapSeed at path: ${bootstrapSeedPath}`)
        }

        if (fs.existsSync(publishingBootstrapSeedPath)) {
            const bootstrapSeed = JSON.parse(fs.readFileSync(publishingBootstrapSeedPath).toString());
            bootstrapSeed["languageLanguageBundle"] = bundleData;
            fs.writeFileSync(publishingBootstrapSeedPath, JSON.stringify(bootstrapSeed));
        } else {
            throw new Error(`Could not find publishingBoostrapSeed at path: ${publishingBootstrapSeedPath}`)
        }
    } else {
        throw new Error(`Could not find lanuageLanguage at path: ${languageLanguagePath}`)
    }
}

main();