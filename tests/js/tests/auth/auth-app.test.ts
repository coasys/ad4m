import { Ad4mClient, CapabilityInput, AuthInfoInput } from "@coasys/ad4m";
import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { apolloClient } from "../../utils/utils";
import { startAgent, connectClient, AgentHandle } from "../../helpers/executor";

const expect = chai.expect;
chai.use(chaiAsPromised);

describe("Apps integration tests", () => {
  let agent: AgentHandle;
  let adminAd4mClient: Ad4mClient;
  let unAuthenticatedAppAd4mClient: Ad4mClient;
  let requestId: string;

  before(async () => {
    agent = await startAgent("apps-agent", { adminCredential: "123" });
    adminAd4mClient = agent.client;
    unAuthenticatedAppAd4mClient = connectClient(agent.gqlPort);
  });

  after(async () => {
    await agent.stop();
  });

  it("once token issued user can get all authenticated apps", async () => {
    requestId = await unAuthenticatedAppAd4mClient.agent.requestCapability({
      appName: "demo-app",
      appDesc: "demo-desc",
      appDomain: "test.ad4m.org",
      appUrl: "https://demo-link",
      capabilities: [
        {
          with: {
            domain: "agent",
            pointers: ["*"],
          },
          can: ["*"],
        },
      ] as CapabilityInput[],
    } as AuthInfoInput);
    let rand = await adminAd4mClient.agent.permitCapability(
      `{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appDomain": "test.ad4m.org","appUrl":"https://demo-link","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]}}`,
    );
    let jwt = await adminAd4mClient.agent.generateJwt(requestId, rand);

    let authenticatedAppAd4mClient = new Ad4mClient(
      apolloClient(agent.gqlPort, jwt),
      false,
    );

    const call = async () => {
      return await authenticatedAppAd4mClient.agent.getApps();
    };

    await expect((await call()).length).to.be.equal(1);
  });

  it("can revoke token", async () => {
    const oldApps = await adminAd4mClient.agent.getApps();

    expect(oldApps.length).to.be.equal(1);
    expect(oldApps[0].revoked).to.be.false;

    const newApps = await adminAd4mClient.agent.revokeToken(requestId);

    expect(newApps.length).to.be.equal(1);
    expect(newApps[0].revoked).to.be.equal(true);

    // check if the app can request another token.
    requestId = await unAuthenticatedAppAd4mClient.agent.requestCapability({
      appName: "demo-app",
      appDesc: "demo-desc",
      appDomain: "test.ad4m.org",
      appUrl: "https://demo-link",
      capabilities: [
        {
          with: {
            domain: "agent",
            pointers: ["*"],
          },
          can: ["*"],
        },
      ] as CapabilityInput[],
    } as AuthInfoInput);
    let rand = await adminAd4mClient.agent.permitCapability(
      `{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appDomain":"test.ad4m.org","appUrl":"https://demo-link","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]}}`,
    );
    let jwt = await adminAd4mClient.agent.generateJwt(requestId, rand);

    let authenticatedAppAd4mClient = new Ad4mClient(
      apolloClient(agent.gqlPort, jwt),
      false,
    );

    const call = async () => {
      return await authenticatedAppAd4mClient.agent.getApps();
    };

    await expect((await call()).length).to.be.equal(2);
  });

  it("can remove apps", async () => {
    const oldApps = await adminAd4mClient.agent.getApps();

    expect(oldApps.length).to.be.equal(2);

    const newApps = await adminAd4mClient.agent.removeApp(requestId);

    expect(newApps.length).to.be.equal(1);

    // check if the app can request another token.
    requestId = await unAuthenticatedAppAd4mClient.agent.requestCapability({
      appName: "demo-app",
      appDesc: "demo-desc",
      appDomain: "test.ad4m.org",
      appUrl: "https://demo-link",
      capabilities: [
        {
          with: {
            domain: "agent",
            pointers: ["*"],
          },
          can: ["*"],
        },
      ] as CapabilityInput[],
    } as AuthInfoInput);
    let rand = await adminAd4mClient.agent.permitCapability(
      `{"requestId":"${requestId}","auth":{"appName":"demo-app","appDesc":"demo-desc","appDomain":"test.ad4m.org","appUrl":"https://demo-link","capabilities":[{"with":{"domain":"agent","pointers":["*"]},"can":["*"]}]}}`,
    );
    let jwt = await adminAd4mClient.agent.generateJwt(requestId, rand);

    let authenticatedAppAd4mClient = new Ad4mClient(
      apolloClient(agent.gqlPort, jwt),
      false,
    );

    const call = async () => {
      return await authenticatedAppAd4mClient.agent.getApps();
    };

    await expect((await call()).length).to.be.equal(2);
  });
});
