import path from "path";
import { Ad4mClient, LanguageMetaInput } from "@perspect3vism/ad4m";
import fs from "fs-extra";
import { exit } from "process";
import { fileURLToPath } from 'url';
import { apolloClient, sleep, startExecutor } from "./utils";
import fetch from 'node-fetch'

//@ts-ignore
global.fetch = fetch

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = path.resolve(__dirname, '..', 'tst-tmp');
const appDataPath = path.resolve(TEST_DIR, "agents", "p");
const publishLanguagesPath = path.resolve(TEST_DIR, "languages");
const publishingBootstrapSeedPath = path.resolve(__dirname, '..', 'publishBootstrapSeed.json');
const bootstrapSeedPath = path.resolve(__dirname, '..', 'bootstrapSeed.json');
const perspectiveDiffSyncHashPath = path.resolve(__dirname, '..', 'scripts', 'perspective-diff-sync-hash');
const gqlPort = 15700;
const hcAdminPort = 15701;
const hcAppPort = 15702;
const ipfsSwarmPort = 15703;

//Update this as new languages are needed within testing code
const languagesToPublish = {
    "agent-expression-store": {name: "agent-expression-store", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput, 
    "direct-message-language": {name: "direct-message-language", description: "", possibleTemplateParams: ["uid", "recipient_did", "recipient_hc_agent_pubkey"]} as LanguageMetaInput, 
    "neighbourhood-store": {name: "neighbourhood-store", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput, 
    "perspective-diff-sync": {name: "perspective-diff-sync", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput,
    "perspective-language": {name: "perspective-language", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput,
}

const languageHashes = {
    "directMessageLanguage": "",
    "agentLanguage": "",
    "perspectiveLanguage": "",
    "neighbourhoodLanguage": "",
    "perspectiveDiffSync": ""
}

function createTestingAgent() {
    if (!fs.existsSync(appDataPath)) {
        fs.mkdirSync(appDataPath);
    }
}

function injectSystemLanguages() {
    if (fs.existsSync(bootstrapSeedPath)) {
        const bootstrapSeed = JSON.parse(fs.readFileSync(bootstrapSeedPath).toString());
        bootstrapSeed["directMessageLanguage"] = languageHashes["directMessageLanguage"];
        bootstrapSeed["agentLanguage"] = languageHashes["agentLanguage"];
        bootstrapSeed["perspectiveLanguage"] = languageHashes["perspectiveLanguage"];
        bootstrapSeed["neighbourhoodLanguage"] = languageHashes["neighbourhoodLanguage"];
        bootstrapSeed["knownLinkLanguages"] = [languageHashes["perspectiveDiffSync"]];
        fs.writeFileSync(bootstrapSeedPath, JSON.stringify(bootstrapSeed));
    } else {
        throw new Error(`Could not find boostrapSeed at path: ${bootstrapSeedPath}`)
    }
}

function injectLangAliasHashes() {
    fs.writeFileSync(perspectiveDiffSyncHashPath, languageHashes["perspectiveDiffSync"]);
}

async function publish() {
    createTestingAgent();

    const executorProcess = await startExecutor(appDataPath, publishingBootstrapSeedPath, gqlPort, hcAdminPort, hcAppPort, ipfsSwarmPort, true);
    
    const ad4mClient = new Ad4mClient(apolloClient(gqlPort));
    await ad4mClient.agent.generate("passphrase");

    for (const [language, languageMeta] of Object.entries(languagesToPublish)) {
        let bundlePath = path.join(publishLanguagesPath, language, "build", "bundle.js").replace(/\\/g, "/");
        console.log("Attempting to publish language", bundlePath);
        let publishedLang = await ad4mClient.languages.publish(bundlePath, languageMeta);
        console.log("Published with result", publishedLang);
        if (language === "agent-expression-store") {
            languageHashes["agentLanguage"] = publishedLang.address;
        }
        if (language === "neighbourhood-store") {
            languageHashes["neighbourhoodLanguage"] = publishedLang.address;
        }
        if (language === "direct-message-language") {
            languageHashes["directMessageLanguage"] = publishedLang.address;
        }
        if (language === "perspective-language") {
            languageHashes["perspectiveLanguage"] = publishedLang.address;
        }
        if (language === "perspective-diff-sync") {
            languageHashes["perspectiveDiffSync"] = publishedLang.address;
        }
    }
    injectSystemLanguages()
    injectLangAliasHashes();

    if (executorProcess) {
        while (!executorProcess.killed){
            let status = executorProcess.kill()
            console.log("killed executor with", status);
            await sleep(500); 
        }
    }

    exit();
}

publish()