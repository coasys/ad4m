///Get the languageLanguage and inject into the bootstrap seed

const fs = require("fs-extra");
const path = require("path");

const languagesDirectory = "../build/languages";
const bootstrapSeed = "../bootstrapSeed.json";
const languageLanguageBundlePath = path.join(__dirname, languagesDirectory, "languages", "build", "bundle.js");

console.log(__dirname, path.join(__dirname, languagesDirectory))

if (!fs.existsSync(path.join(__dirname, languagesDirectory))) {
    fs.mkdirSync(path.join(__dirname, languagesDirectory));
}

const languageLanguageBundle = fs.readFileSync(languageLanguageBundlePath).toString();
let bootstrapSeedData = JSON.parse(fs.readFileSync(path.join(__dirname, bootstrapSeed)).toString());
bootstrapSeedData["languageLanguageBundle"] = languageLanguageBundle;
fs.writeFileSync(path.join(__dirname, bootstrapSeed), JSON.stringify(bootstrapSeedData));