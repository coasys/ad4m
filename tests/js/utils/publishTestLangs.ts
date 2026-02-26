import path from "path";
import { Ad4mClient, LanguageMetaInput } from "@coasys/ad4m";
import fs from "fs-extra";
import { exit } from "process";
import { execSync } from "child_process";
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
// Allow env-var override so concurrent CI jobs can each use a unique port range
// and avoid stomping on each other during the setup phase.
// Defaults: 15700/15701/15702 (used by integration-tests-js / test-main)
const gqlPort = parseInt(process.env.AD4M_SETUP_GQL_PORT || '15700', 10);
const hcAdminPort = parseInt(process.env.AD4M_SETUP_HC_ADMIN_PORT || '15701', 10);
const hcAppPort = parseInt(process.env.AD4M_SETUP_HC_APP_PORT || '15702', 10);

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
    // Pre-clean: kill any orphaned executor from a previous CI job that may be
    // squatting on our ports. Self-hosted runners reuse workdirs between jobs
    // and don't clean up automatically. Use TCP:LISTEN so we only kill the
    // server (executor), not any other process with a connection on those ports.
    console.log(`Pre-cleaning ports ${gqlPort}/${hcAdminPort}/${hcAppPort} before starting executor...`);
    for (const port of [gqlPort, hcAdminPort, hcAppPort]) {
        try { execSync(`lsof -ti TCP:${port} -s TCP:LISTEN | xargs -r kill -9`, { stdio: 'ignore' }); } catch(e) {}
    }
    await sleep(500);

    createTestingAgent();

    const executorProcess = await startExecutor(appDataPath, publishingBootstrapSeedPath, gqlPort, hcAdminPort, hcAppPort, true);
    
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

    // Kill the executor by port (LISTEN-state only), not by process object.
    // exec() wraps the command in a shell; executorProcess.kill() only kills
    // the shell — the actual ad4m-executor (grandchild) survives and becomes
    // an orphan that blocks the port in future CI runs.
    // IMPORTANT: use TCP:LISTEN filter so we only kill the listening process
    // (the executor), NOT this node process which has a connection TO the port.
    console.log(`Killing executor on ports ${gqlPort}/${hcAdminPort}/${hcAppPort}...`);
    for (const port of [gqlPort, hcAdminPort, hcAppPort]) {
        try { execSync(`lsof -ti TCP:${port} -s TCP:LISTEN | xargs -r kill -9`, { stdio: 'ignore' }); } catch(e) {}
    }
    await sleep(1000);

    exit();
}

publish()