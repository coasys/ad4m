import path from "path";
import { Ad4mClient, LanguageMetaInput } from "@coasys/ad4m";
import fs from "fs-extra";
import { exit } from "process";
import { execSync } from "child_process";
import { fileURLToPath } from 'url';
import { baseUrl, sleep, startExecutor } from "./utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = path.resolve(__dirname, '..', 'tst-tmp');
const appDataPath = path.resolve(TEST_DIR, "agents", "p");
const publishLanguagesPath = path.resolve(TEST_DIR, "languages");
const publishingBootstrapSeedPath = path.resolve(__dirname, '..', 'publishBootstrapSeed.json');
const bootstrapSeedPath = path.resolve(__dirname, '..', 'bootstrapSeed.json');
const perspectiveDiffSyncHashPath = path.resolve(__dirname, '..', 'scripts', 'perspective-diff-sync-hash');
const serverLinkLanguageHashPath = path.resolve(__dirname, '..', 'scripts', 'server-link-language-hash');
// Local mode: skip Holochain when preparing local-only test environments.
// Set LOCAL_MODE=true in the prepare-test-local npm scripts.
const localMode = process.env.LOCAL_MODE === 'true';

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
    // Allocate random free ports to avoid collisions with stale executors
    // from previous CI jobs on the same self-hosted runner.
    const [apiPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
    const setupPorts = [apiPort, hcAdminPort, hcAppPort];
    console.log(`Setup ports: ${setupPorts.join('/')}`);

    // Register with the port cleanup registry so cleanup.js can kill the
    // executor if this process is killed ungracefully (SIGKILL, runner cancel).
    registerPorts(setupPorts);

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

        // Copy published language bundles to a shared directory so test executors
        // with different data paths can find them on disk. In HC mode the HC DHT
        // handles distribution; in local mode there is no shared network, so we
        // use a filesystem copy instead.
        //
        // Strategy: copy from the SOURCE bundles using the published hashes as
        // directory names. This avoids depending on where the executor stores
        // its internal data (which may differ from appDataPath due to the
        // temp-dir hashing in startExecutor).
        const sharedLangsDir = path.resolve(TEST_DIR, "published-languages");
        const langFolderToHash: Record<string, keyof typeof languageHashes> = {
            "agent-expression-store": "agentLanguage",
            "neighbourhood-store": "neighbourhoodLanguage",
            "perspective-diff-sync": "perspectiveDiffSync",
            "perspective-language": "perspectiveLanguage",
            "server-link-language": "serverLinkLanguage",
        };
        for (const [langFolder, hashKey] of Object.entries(langFolderToHash)) {
            const srcBundle = path.join(publishLanguagesPath, langFolder, "build", "bundle.js");
            const hash = languageHashes[hashKey];
            if (hash && fs.existsSync(srcBundle)) {
                const destDir = path.join(sharedLangsDir, hash);
                fs.ensureDirSync(destDir);
                fs.copySync(srcBundle, path.join(destDir, "bundle.js"), { overwrite: true });
                console.log(`Published ${langFolder} → ${destDir}/bundle.js`);
            }
        }
        console.log(`Published languages staged in ${sharedLangsDir}`);
    } finally {
        // Always kill the executor on the way out — success or failure.
        // Uses TCP:LISTEN filter so we only kill the listening server (the executor),
        // NOT this node process which has an outbound connection to that port.
        console.log(`Killing executor on ports ${setupPorts.join('/')}...`);
        killExecutorPorts(setupPorts);
        deregisterPorts(setupPorts);
        await sleep(1000);
    }

    exit();
}

publish()