import path from "path";
import { Ad4mClient, LanguageMetaInput } from "@coasys/ad4m";
import fs from "fs-extra";
import { exit } from "process";
import { execSync } from "child_process";
import { fileURLToPath } from 'url';
import { baseUrl, sleep, startExecutor } from "./utils";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = path.resolve(__dirname, '..', 'tst-tmp');
const appDataPath = path.resolve(TEST_DIR, "agents", "p");
const publishLanguagesPath = path.resolve(TEST_DIR, "languages");
const publishingBootstrapSeedPath = path.resolve(__dirname, '..', 'publishBootstrapSeed.json');
const bootstrapSeedPath = path.resolve(__dirname, '..', 'bootstrapSeed.json');
const perspectiveDiffSyncHashPath = path.resolve(__dirname, '..', 'scripts', 'perspective-diff-sync-hash');
const serverLinkLanguageHashPath = path.resolve(__dirname, '..', 'scripts', 'server-link-language-hash');
// Allow env-var override so concurrent CI jobs can each use a unique port range
// and avoid stomping on each other during the setup phase.
// Defaults: 15700/15701/15702 (used by integration-tests-js / test-main)
const apiPort = parseInt(process.env.AD4M_SETUP_API_PORT || '15700', 10);
const hcAdminPort = parseInt(process.env.AD4M_SETUP_HC_ADMIN_PORT || '15701', 10);
const hcAppPort = parseInt(process.env.AD4M_SETUP_HC_APP_PORT || '15702', 10);

//Update this as new languages are needed within testing code
const languagesToPublish = {
    "agent-expression-store": {name: "agent-expression-store", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput,
    "neighbourhood-store": {name: "neighbourhood-store", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput,
    "perspective-diff-sync": {name: "perspective-diff-sync", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput,
    "perspective-language": {name: "perspective-language", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput,
    // SERVER_URL + ROOM_ID match the //!@ad4m-template-variable declarations in
    // bootstrap-languages/server-link-language/index.ts. `name` + `description`
    // aren't code-templated — they get through to the language meta so tests
    // can assert on `socialContext.name` the same way they do for p-diff-sync.
    "server-link-language": {name: "server-link-language", description: "", possibleTemplateParams: ["SERVER_URL", "ROOM_ID", "name", "description"]} as LanguageMetaInput,
}

const languageHashes = {
    "agentLanguage": "",
    "perspectiveLanguage": "",
    "neighbourhoodLanguage": "",
    "perspectiveDiffSync": "",
    "serverLinkLanguage": ""
}

// Kill the listening process on each port (TCP:LISTEN filter ensures we only
// kill the executor server, NOT this node process which has a *connection* to it).
function killExecutorPorts(ports: number[]) {
    for (const port of ports) {
        try {
            execSync(`lsof -ti TCP:${port} -s TCP:LISTEN | xargs -r kill -9`, { stdio: 'ignore' });
        } catch (e) {
            console.warn(`Port cleanup warning for ${port}:`, e);
        }
    }
}

function createTestingAgent() {
    if (!fs.existsSync(appDataPath)) {
        fs.mkdirSync(appDataPath);
    }
}

function injectSystemLanguages() {
    if (fs.existsSync(bootstrapSeedPath)) {
        const bootstrapSeed = JSON.parse(fs.readFileSync(bootstrapSeedPath).toString());
        bootstrapSeed["directMessageLanguage"] = "";
        bootstrapSeed["agentLanguage"] = languageHashes["agentLanguage"];
        bootstrapSeed["perspectiveLanguage"] = languageHashes["perspectiveLanguage"];
        bootstrapSeed["neighbourhoodLanguage"] = languageHashes["neighbourhoodLanguage"];
        bootstrapSeed["knownLinkLanguages"] = [languageHashes["perspectiveDiffSync"]];
        fs.writeFileSync(bootstrapSeedPath, JSON.stringify(bootstrapSeed));
    } else {
        throw new Error(`Could not find bootstrapSeed at path: ${bootstrapSeedPath}`)
    }
}

function injectLangAliasHashes() {
    fs.writeFileSync(perspectiveDiffSyncHashPath, languageHashes["perspectiveDiffSync"]);
    fs.writeFileSync(serverLinkLanguageHashPath, languageHashes["serverLinkLanguage"]);
}

async function publish() {
    const setupPorts = [apiPort, hcAdminPort, hcAppPort];

    // Pre-clean: kill any orphaned executor from a previous CI job that may be
    // squatting on our ports. Self-hosted runners reuse workdirs between jobs
    // and don't clean up automatically.
    console.log(`Pre-cleaning ports ${setupPorts.join('/')} before starting executor...`);
    killExecutorPorts(setupPorts);
    await sleep(500);

    createTestingAgent();

    const executorProcess = await startExecutor(appDataPath, publishingBootstrapSeedPath, apiPort, hcAdminPort, hcAppPort, true);

    try {
        const ad4mClient = new Ad4mClient(baseUrl(apiPort));
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
            if (language === "perspective-language") {
                languageHashes["perspectiveLanguage"] = publishedLang.address;
            }
            if (language === "perspective-diff-sync") {
                languageHashes["perspectiveDiffSync"] = publishedLang.address;
            }
            if (language === "server-link-language") {
                languageHashes["serverLinkLanguage"] = publishedLang.address;
            }
        }
        injectSystemLanguages();
        injectLangAliasHashes();
    } finally {
        // Always kill the executor on the way out — success or failure.
        // Uses TCP:LISTEN filter so we only kill the listening server (the executor),
        // NOT this node process which has an outbound connection to that port.
        console.log(`Killing executor on ports ${setupPorts.join('/')}...`);
        killExecutorPorts(setupPorts);
        await sleep(1000);
    }

    exit();
}

publish()