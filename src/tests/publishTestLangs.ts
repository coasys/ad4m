import {
    ApolloClient,
    InMemoryCache,
} from "@apollo/client/core";
import { WebSocketLink } from '@apollo/client/link/ws';
import ws from "ws"
import main from "../main";
import path from "path";
import { OuterConfig } from "../types";
import { Ad4mClient } from "@perspect3vism/ad4m";
import fs from "fs-extra";

const TEST_DIR = path.join(`${__dirname}/../../src/test-temp`);
const appDataPath = path.join(TEST_DIR, "agents", "publishing-agent");
const ipfsRepoPath = path.join(appDataPath);
const publishLanguagesPath = path.join(TEST_DIR, "languages");
const publishingAgentPath = path.join(`${__dirname}/../../src/tests/publishing-agent`);
const publishBootstrapSeedPath = path.join(`${__dirname}/../../src/tests/publishBootstrapSeed.json`);
const languagesToPublish = fs.readdirSync(publishLanguagesPath);

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

    console.log("Got me", await ad4mClient.agent.me());
}

publish()