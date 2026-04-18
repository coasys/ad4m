import path from "path";
import { Ad4mClient, CapabilityInput, AuthInfoInput } from "@coasys/ad4m";
import fs from "fs";
import { fileURLToPath } from 'url';
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { baseUrl, sleep, startExecutor, quitExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { ChildProcess } from "child_process";

const expect = chai.expect;
chai.use(chaiAsPromised);

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("Apps integration tests", () => {
  const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
  const appDataPath = path.join(TEST_DIR, "agents", "apps-agent");
  const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
  let gqlPort: number;
  let hcAdminPort: number;
  let hcAppPort: number;

  let adminAd4mClient: Ad4mClient | null = null
  let unAuthenticatedAppAd4mClient: Ad4mClient | null = null
  let requestId: string;

  let executorProcess: ChildProcess | null = null

  before(async () => {
    [gqlPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
    registerPorts([gqlPort, hcAdminPort, hcAppPort]);
    if(!fs.existsSync(TEST_DIR)) {
        throw Error("Please ensure that prepare-test is run before running tests!");
    }
    if(!fs.existsSync(path.join(TEST_DIR, 'agents')))
        fs.mkdirSync(path.join(TEST_DIR, 'agents'))
    if(!fs.existsSync(appDataPath))
        fs.mkdirSync(appDataPath)

    executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
      gqlPort, hcAdminPort, hcAppPort , false, "123");

    adminAd4mClient = new Ad4mClient(baseUrl(gqlPort), "123", false)
    await adminAd4mClient.agent.generate("passphrase")
    
    unAuthenticatedAppAd4mClient = new Ad4mClient(baseUrl(gqlPort), undefined, false)
  })

  after(async () => {
    if (executorProcess) {
      await quitExecutor(executorProcess, gqlPort, "123");
    }
    deregisterPorts([gqlPort, hcAdminPort, hcAppPort]);
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
      let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appDomain": "test.ad4m.org","appUrl":"https://demo-link","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]}}`)
      let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

      let authenticatedAppAd4mClient = new Ad4mClient(baseUrl(gqlPort), jwt, false)
  
      const call = async () => {
          return await authenticatedAppAd4mClient!.agent.getApps();
      }
  
      await expect((await call()).length).to.be.equal(1);
  });
  
  it("can revoke token", async () => {
      const oldApps = await adminAd4mClient!.agent.getApps();
  
      expect(oldApps.length).to.be.equal(1);
      expect(oldApps[0].revoked).to.be.false;
  
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
      let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appDomain":"test.ad4m.org","appUrl":"https://demo-link","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]}}`)
      let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

      let authenticatedAppAd4mClient = new Ad4mClient(baseUrl(gqlPort), jwt, false)
  
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
      let rand = await adminAd4mClient!.agent.permitCapability(`{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appDomain":"test.ad4m.org","appUrl":"https://demo-link","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]}}`)
      let jwt = await adminAd4mClient!.agent.generateJwt(requestId, rand)

      // @ts-ignore
      let authenticatedAppAd4mClient = new Ad4mClient(baseUrl(gqlPort), jwt, false)

      const call = async () => {
          return await authenticatedAppAd4mClient!.agent.getApps();
      }

      await expect((await call()).length).to.be.equal(2);
  });
})
