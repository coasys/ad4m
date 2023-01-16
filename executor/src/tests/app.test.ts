import { ApolloClient, InMemoryCache } from "@apollo/client/core";
import Websocket from "ws";
import main from "../main";
import path from "path";
import { OuterConfig } from "../types";
import { Ad4mClient } from "@perspect3vism/ad4m";
import fs from "fs-extra";
import PerspectivismCore from "../core/PerspectivismCore";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { createClient } from "graphql-ws";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

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

describe("Apps integration tests", () => {
  const TEST_DIR = path.join(`${__dirname}/../../src/tst-tmp`);
  const appDataPath = path.join(TEST_DIR, "agents", "apps-agent");
  const bootstrapSeedPath = path.join(`${__dirname}/../../src/tests/bootstrapSeed.json`);
  const ipfsRepoPath = path.join(appDataPath);
  const gqlPort = 15000
  const hcPortAdmin = 15001
  const hcPortApp = 15002
  const ipfsSwarmPort = 15003

  let agentCore: PerspectivismCore | null = null
  let adminAd4mClient: Ad4mClient | null = null
  let unAuthenticatedAppAd4mClient: Ad4mClient | null = null
  let requestId: string;

  before(async () => {
      if (!fs.existsSync(appDataPath)) {
          fs.mkdirSync(appDataPath, { recursive: true });
      }

      agentCore = await main.init({
          appDataPath,
          resourcePath: TEST_DIR,
          networkBootstrapSeed: bootstrapSeedPath,
          bootstrapFixtures: {
              languages: [],
              perspectives: [],
          },
          mocks: false,
          gqlPort,
          hcPortAdmin,
          hcPortApp,
          ipfsSwarmPort,
          ipfsRepoPath,
          hcUseBootstrap: false,
          hcUseProxy: false,
          hcUseLocalProxy: false,
          hcUseMdns: true,
          reqCredential: "123"
      } as OuterConfig)

      // @ts-ignore            
      adminAd4mClient = new Ad4mClient(apolloClient(gqlPort, "123"))
      await adminAd4mClient.agent.generate("passphrase")
      // await agentCore.waitForAgent();
      // agentCore.initControllers()
      // await agentCore.initLanguages()
      

      // @ts-ignore
      unAuthenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort))
  })

  after(async () => {
      await agentCore!.exit();
  })

  it("once token issued user can get all authenticated apps", async () => {
      requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]')
      let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]}}`)
      let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)
  
      // @ts-ignore
      let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort, jwt))
  
      const call = async () => {
          return await authenticatedAppAd4mClient!.agent.getApps();
      }
  
      await expect((await call()).length).to.be.equal(1);
  });
  
  it("can revoke token", async () => {
      const oldApps = await adminAd4mClient!.agent.getApps();
  
      expect(oldApps.length).to.be.equal(1);
  
      const newApps = await adminAd4mClient!.agent.revokeToken(requestId);
  
      expect(newApps.length).to.be.equal(1);
      expect(newApps[0].revoked).to.be.equal(true);

      // check if the app can request another token.
      requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]')
      let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]}}`)
      let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

      // @ts-ignore
      let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort, jwt))
  
      const call = async () => {
        return await authenticatedAppAd4mClient!.agent.getApps();
      }

      await expect((await call()).length).to.be.equal(2);
  });
  
  it("can remove apps", async () => {
      const oldApps = await adminAd4mClient!.agent.getApps();
  
      expect(oldApps.length).to.be.equal(2);
  
      const newApps = await adminAd4mClient!.agent.removeApp(requestId);
  
      expect(newApps.length).to.be.equal(1);

      // check if the app can request another token.
      requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability("demo-app", "demo-desc", "https://demo-link", '[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]')
      let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appUrl":"demo-url","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]}}`)
      let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

      // @ts-ignore
      let authenticatedAppAd4mClient = new Ad4mClient(apolloClient(gqlPort, jwt))

      const call = async () => {
          return await authenticatedAppAd4mClient!.agent.getApps();
      }

      await expect((await call()).length).to.be.equal(2);
  });
})
