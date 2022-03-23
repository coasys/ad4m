//Remove languageLanguage bundle so that when running tests & making commit to github, the languageLanguage bundle is not comitted also
const fs = require("fs");

const bootstrapSeedPath = "./src/tests/bootstrapSeed.json";
const publishingBootstrapSeedPath = "./src/tests/publishBootstrapSeed.json";
const dbJsonPath = "./src/tests/p-agent/ad4m/data/db.json";

async function main() {
    if (fs.existsSync(bootstrapSeedPath)) {
        const bootstrapSeed = JSON.parse(fs.readFileSync(bootstrapSeedPath).toString());
        bootstrapSeed["languageLanguageBundle"] = "";
        fs.writeFileSync(bootstrapSeedPath, JSON.stringify(bootstrapSeed));
    } else {
        throw new Error(`Could not find boostrapSeed at path: ${bootstrapSeedPath}`)
    }

    if (fs.existsSync(publishingBootstrapSeedPath)) {
        const bootstrapSeed = JSON.parse(fs.readFileSync(publishingBootstrapSeedPath).toString());
        bootstrapSeed["languageLanguageBundle"] = "";
        fs.writeFileSync(publishingBootstrapSeedPath, JSON.stringify(bootstrapSeed));
    } else {
        throw new Error(`Could not find publishingBoostrapSeed at path: ${publishingBootstrapSeedPath}`)
    }

    if (fs.existsSync(dbJsonPath)) {
        fs.rmSync(dbJsonPath);
    }
}

main();