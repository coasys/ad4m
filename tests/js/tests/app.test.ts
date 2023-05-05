import path from "path";
import { Ad4mClient, CapabilityInput, AuthInfoInput } from "@perspect3vism/ad4m";
import fs from "fs";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient, startExecutor } from "../utils/utils";
import fetch from 'node-fetch'
import { ChildProcess } from "child_process";

//@ts-ignore
global.fetch = fetch

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("Apps integration tests", () => {
  const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
  const appDataPath = path.join(TEST_DIR, "agents", "apps-agent");
  const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
  const gqlPort = 15000
  const hcAdminPort = 15001
  const hcAppPort = 15002
  const ipfsSwarmPort = 15003

  let adminAd4mClient: Ad4mClient | null = null
  let unAuthenticatedAppAd4mClient: Ad4mClient | null = null
  let requestId: string;

  let executorProcess: ChildProcess | null = null

  before(async () => {
    if (!fs.existsSync(appDataPath)) {
        fs.mkdirSync(appDataPath, { recursive: true });
    }

    executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
      gqlPort, hcAdminPort, hcAppPort, ipfsSwarmPort);
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
    if (executorProcess) {
        executorProcess.kill()
    }
  })

  it("once token issued user can get all authenticated apps", async () => {
      requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
        appName: "demo-app",
        appDesc: "demo-desc",
        appDomain: "test.ad4m.org",
        appUrl: "https://demo-link",
        capabilities: [
            {
                with: {
                    domain:"agent",
                    pointers:["*"]
                },
                can: ["*"]
            }
        ] as CapabilityInput[]
      } as AuthInfoInput)
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
      requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
        appName: "demo-app",
        appDesc: "demo-desc",
        appDomain: "test.ad4m.org",
        appUrl: "https://demo-link",
        capabilities: [
            {
                with: {
                    domain:"agent",
                    pointers:["*"]
                },
                can: ["*"]
            }
        ] as CapabilityInput[]
      } as AuthInfoInput)
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
      requestId = await unAuthenticatedAppAd4mClient!.agent.requestCapability({
        appName: "demo-app",
        appDesc: "demo-desc",
        appDomain: "test.ad4m.org",
        appUrl: "https://demo-link",
        capabilities: [
            {
                with: {
                    domain:"agent",
                    pointers:["*"]
                },
                can: ["*"]
            }
        ] as CapabilityInput[]
      } as AuthInfoInput)
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