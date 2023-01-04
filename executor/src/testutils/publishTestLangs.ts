import { ApolloClient, InMemoryCache } from "@apollo/client/core";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { createClient } from "graphql-ws";
import Websocket from "ws";
import main from "../main";
import path from "path";
import { OuterConfig } from "../types";
import { Ad4mClient, LanguageMetaInput } from "@perspect3vism/ad4m";
import fs from "fs-extra";
import { exit } from "process";
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = path.join(`${__dirname}/../../src/tst-tmp`);
const appDataPath = path.join(TEST_DIR, "agents", "p");
const ipfsRepoPath = path.join(appDataPath);
const publishLanguagesPath = path.join(TEST_DIR, "languages");
const publishingAgentPath = path.join(`${__dirname}/../../src/tests/p`);
const publishingBootstrapSeedPath = path.join(`${__dirname}/../../src/tests/publishBootstrapSeed.json`);
const bootstrapSeedPath = path.join(`${__dirname}/../../src/tests/bootstrapSeed.json`);
const noteIpfsHashPath = path.join(`${__dirname}/../../scripts/note-ipfs-hash`);
const perspectiveDiffSyncHashPath = path.join(`${__dirname}/../../scripts/perspective-diff-sync-hash`);

//Update this as new languages are needed within testing code
const languagesToPublish = {
    "agent-expression-store": {name: "agent-expression-store", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput, 
    "direct-message-language": {name: "direct-message-language", description: "", possibleTemplateParams: ["uid", "recipient_did", "recipient_hc_agent_pubkey"]} as LanguageMetaInput, 
    "neighbourhood-store": {name: "neighbourhood-store", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput, 
    "note-ipfs": {name: "note-ipfs", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput, 
    "perspective-diff-sync": {name: "perspective-diff-sync", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput,
    "perspective-language": {name: "perspective-language", description: "", possibleTemplateParams: ["uid", "name", "description"]} as LanguageMetaInput,
}

const languageHashes = {
    "directMessageLanguage": "",
    "agentLanguage": "",
    "perspectiveLanguage": "",
    "neighbourhoodLanguage": "",
    "noteIpfs": "",
    "perspectiveDiffSync": ""
}

function apolloClient(port: number, token?: string): ApolloClient<any> {
    const wsLink = new GraphQLWsLink(createClient({
        url: `ws://localhost:${port}/graphql`,
        webSocketImpl: Websocket,
        connectionParams: () => {
            return {
                headers: {
                    authorization: token
                }
            }
        },
    }));

    return new ApolloClient({
        link: wsLink,
        cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
        defaultOptions: {
            watchQuery: {
                fetchPolicy: "no-cache",
            },
            query: {
                fetchPolicy: "no-cache",
            }
        },
    });
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
        fs.writeFileSync(bootstrapSeedPath, JSON.stringify(bootstrapSeed));
    } else {
        throw new Error(`Could not find boostrapSeed at path: ${bootstrapSeedPath}`)
    }
}

function injectLangAliasHashes() {
    fs.writeFileSync(noteIpfsHashPath, languageHashes["noteIpfs"]);
    fs.writeFileSync(perspectiveDiffSyncHashPath, languageHashes["perspectiveDiffSync"]);
}

async function publish() {
    createTestingAgent();
    const core = await main.init({
        appDataPath,
        resourcePath: TEST_DIR,
        networkBootstrapSeed: publishingBootstrapSeedPath,
        languageLanguageOnly: true,
        bootstrapFixtures: {
          languages: [],
          perspectives: [],
        },
        mocks: false,
        ipfsRepoPath,
        hcUseBootstrap: false,
        hcUseProxy: false,
        hcUseLocalProxy: false,
        hcUseMdns: true
    } as OuterConfig)
    
    const ad4mClient = new Ad4mClient(apolloClient(4000));
    await ad4mClient.agent.generate("passphrase");
    
    await core.waitForAgent();
    core.initControllers()
    await core.initLanguages()

    for (const [language, languageMeta] of Object.entries(languagesToPublish)) {
        let bundlePath = path.join(publishLanguagesPath, language, "build", "bundle.js");
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
        if (language === "note-ipfs") {
            languageHashes["noteIpfs"] = publishedLang.address;
        }
        if (language === "perspective-diff-sync") {
            languageHashes["perspectiveDiffSync"] = publishedLang.address;
        }
    }
    injectSystemLanguages()
    injectLangAliasHashes();
    await core!.exit();
    exit();
}

publish()