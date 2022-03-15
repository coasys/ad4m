import {
    ApolloClient,
    InMemoryCache,
} from "@apollo/client/core";
import { WebSocketLink } from '@apollo/client/link/ws';
import ws from "ws"
import main from "../main";
import path from "path";
import { OuterConfig } from "../types";
import { Ad4mClient, LanguageMetaInput } from "@perspect3vism/ad4m";
import fs from "fs-extra";

const TEST_DIR = path.join(`${__dirname}/../../src/test-temp`);
const appDataPath = path.join(TEST_DIR, "agents", "publishing-agent");
const ipfsRepoPath = path.join(appDataPath);
const publishLanguagesPath = path.join(TEST_DIR, "languages");
const publishingAgentPath = path.join(`${__dirname}/../../src/tests/publishing-agent`);
const publishBootstrapSeedPath = path.join(`${__dirname}/../../src/tests/publishBootstrapSeed.json`);

//Update this as new languages are needed within testing code
const languagesToPublish = {
    "agent-expression-store": {name: "agent-expression-store", description: "", possibleTemplateParams: ["id", "name", "description"]} as LanguageMetaInput, 
    "direct-message-language": {name: "direct-message-language", description: "", possibleTemplateParams: ["id", "name", "description"]} as LanguageMetaInput, 
    "neighbourhood-store": {name: "neighbourhood-store", description: "", possibleTemplateParams: ["id", "name", "description"]} as LanguageMetaInput, 
    "note-ipfs": {name: "note-ipfs", description: "", possibleTemplateParams: ["id", "name", "description"]} as LanguageMetaInput, 
    "social-context": {name: "social-context", description: "", possibleTemplateParams: ["id", "name", "description"]} as LanguageMetaInput
}

function apolloClient(port: number): ApolloClient<any> {
    return new ApolloClient({
      link: new WebSocketLink({
          uri: `http://localhost:${port}/graphql`,
          options: { reconnect: true },
          webSocketImpl: ws,
      }),
      cache: new InMemoryCache({resultCaching: false, addTypename: false}),
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
        fs.copySync(publishingAgentPath, appDataPath);
    }
}

async function publish() {
    createTestingAgent();
    const core = await main.init({
        appDataPath,
        resourcePath: TEST_DIR,
        networkBootstrapSeed: publishBootstrapSeedPath,
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

    core.initControllers()
    await core.initLanguages()

    const ad4mClient = new Ad4mClient(apolloClient(4000));
    await ad4mClient.agent.unlock("passphrase");

    for (const [language, languageMeta] of Object.entries(languagesToPublish)) {
        let bundlePath = path.join(publishLanguagesPath, language, "build", "bundle.js");
        console.log("Attempting to publish language", bundlePath);
        let publishedLang = await ad4mClient.languages.publish(bundlePath, languageMeta);
        console.log("Published with result", publishedLang);
    }
}

publish()