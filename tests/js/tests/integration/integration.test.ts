import fs from "fs-extra";
import path from "path";
import {
  Ad4mClient,
  ExpressionProof,
  Link,
  LinkExpression,
  Perspective,
} from "@coasys/ad4m";
import { fileURLToPath } from "url";
import { expect } from "chai";
import {
  startExecutor,
  apolloClient,
  runHcLocalServices,
  sleep,
  waitForExit,
} from "../../utils/utils";
import { ChildProcess } from "child_process";
import { getFreePorts } from "../../helpers/ports";
import perspectiveTests from "./perspective.suite";
import agentTests from "./agent.suite";
import aiTests from "./ai.suite";
import languageTests from "./language.suite";
import expressionTests from "./expression.suite";
import neighbourhoodTests from "./neighbourhood.suite";
import runtimeTests from "./runtime.suite";
import agentLanguageTests from "./agent-language.suite";
import socialDNATests from "./social-dna-flow.suite";
import tripleAgentTests from "./triple-agent-test.suite";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const TEST_DIR = `${__dirname}/../../tst-tmp`;

export class TestContext {
  #alice: Ad4mClient | undefined;
  #bob: Ad4mClient | undefined;
  #jim: Ad4mClient | undefined;

  #aliceCore: ChildProcess | undefined;
  #bobCore: ChildProcess | undefined;
  #jimCore: ChildProcess | undefined;

  get ad4mClient(): Ad4mClient {
    return this.#alice!;
  }

  get alice(): Ad4mClient {
    return this.#alice!;
  }

  get bob(): Ad4mClient {
    return this.#bob!;
  }

  get jim(): Ad4mClient {
    return this.#jim!;
  }

  set alice(client: Ad4mClient) {
    this.#alice = client;
  }

  set bob(client: Ad4mClient) {
    this.#bob = client;
  }

  set jim(client: Ad4mClient) {
    this.#jim = client;
  }

  set aliceCore(aliceCore: ChildProcess) {
    this.#aliceCore = aliceCore;
  }

  set bobCore(bobCore: ChildProcess) {
    this.#bobCore = bobCore;
  }

  set jimCore(jimCore: ChildProcess) {
    this.#jimCore = jimCore;
  }

  async makeAllNodesKnown() {
    const aliceAgentInfo = await this.#alice!.runtime.hcAgentInfos();
    const bobAgentInfo = await this.#bob!.runtime.hcAgentInfos();

    await this.#alice!.runtime.hcAddAgentInfos(bobAgentInfo);
    await this.#bob!.runtime.hcAddAgentInfos(aliceAgentInfo);
  }

  async makeAllThreeNodesKnown() {
    const aliceAgentInfo = await this.#alice!.runtime.hcAgentInfos();
    const bobAgentInfo = await this.#bob!.runtime.hcAgentInfos();
    const jimAgentInfo = await this.#jim!.runtime.hcAgentInfos();

    await this.#alice!.runtime.hcAddAgentInfos(bobAgentInfo);
    await this.#alice!.runtime.hcAddAgentInfos(jimAgentInfo);
    await this.#bob!.runtime.hcAddAgentInfos(aliceAgentInfo);
    await this.#bob!.runtime.hcAddAgentInfos(jimAgentInfo);
    await this.#jim!.runtime.hcAddAgentInfos(aliceAgentInfo);
    await this.#jim!.runtime.hcAddAgentInfos(bobAgentInfo);
  }
}
let testContext: TestContext = new TestContext();

describe("Integration tests", function () {
  this.timeout(360000);
  const appDataPath = path.join(TEST_DIR, "agents", "alice");
  const bootstrapSeedPath = path.join(`${__dirname}/../../bootstrapSeed.json`);
  let gqlPort: number;
  let hcAdminPort: number;
  let hcAppPort: number;

  let executorProcess: ChildProcess | null = null;

  let proxyUrl: string | null = null;
  let bootstrapUrl: string | null = null;
  let localServicesProcess: ChildProcess | null = null;
  let relayUrl: string | null = null;

  before(async () => {
    [gqlPort, hcAdminPort, hcAppPort] = await getFreePorts(3);

    if (!fs.existsSync(TEST_DIR)) {
      throw Error(
        "Please ensure that prepare-test is run before running tests!",
      );
    }
    if (!fs.existsSync(path.join(TEST_DIR, "agents")))
      fs.mkdirSync(path.join(TEST_DIR, "agents"));
    if (!fs.existsSync(appDataPath)) fs.mkdirSync(appDataPath);

    let localServices = await runHcLocalServices();
    proxyUrl = localServices.proxyUrl;
    bootstrapUrl = localServices.bootstrapUrl;
    localServicesProcess = localServices.process;
    relayUrl = localServices.relayUrl;

    executorProcess = await startExecutor(
      appDataPath,
      bootstrapSeedPath,
      gqlPort,
      hcAdminPort,
      hcAppPort,
      false,
      undefined,
      proxyUrl!,
      bootstrapUrl!,
      relayUrl!,
    );

    testContext.alice = new Ad4mClient(apolloClient(gqlPort));
    testContext.aliceCore = executorProcess;
  });

  after(async () => {
    await waitForExit(executorProcess);
    await waitForExit(localServicesProcess);
  });

  describe("Agent / Agent-Setup", agentTests(testContext));
  describe("Artificial Intelligence", aiTests(testContext));
  describe("Runtime", runtimeTests(testContext));
  describe("Expression", expressionTests(testContext));
  describe("Perspective", perspectiveTests(testContext));
  describe("Social DNA", socialDNATests(testContext));

  describe("with Alice and Bob", () => {
    let bobExecutorProcess: ChildProcess | null = null;
    before(async () => {
      const bobAppDataPath = path.join(TEST_DIR, "agents", "bob");
      const bobBootstrapSeedPath = path.join(
        `${__dirname}/../../bootstrapSeed.json`,
      );
      const [bobGqlPort, bobHcAdminPort, bobHcAppPort] = await getFreePorts(3);

      if (!fs.existsSync(path.join(TEST_DIR, "agents")))
        fs.mkdirSync(path.join(TEST_DIR, "agents"));
      if (!fs.existsSync(bobAppDataPath)) fs.mkdirSync(bobAppDataPath);

      bobExecutorProcess = await startExecutor(
        bobAppDataPath,
        bobBootstrapSeedPath,
        bobGqlPort,
        bobHcAdminPort,
        bobHcAppPort,
        false,
        undefined,
        proxyUrl!,
        bootstrapUrl!,
        relayUrl!,
      );

      testContext.bob = new Ad4mClient(apolloClient(bobGqlPort));
      testContext.bobCore = bobExecutorProcess;
      await testContext.bob.agent.generate("passphrase");

      const status = await testContext.bob.agent.status();

      expect(status.isInitialized).to.be.true;
      expect(status.isUnlocked).to.be.true;

      let link = new LinkExpression();
      link.author = "did:test";
      link.timestamp = new Date().toISOString();
      link.data = new Link({
        source: "ad4m://src",
        target: "test://target",
        predicate: "ad4m://pred",
      });
      link.proof = new ExpressionProof("sig", "key");

      await testContext.bob.agent.updatePublicPerspective(
        new Perspective([link]),
      );

      await testContext.makeAllNodesKnown();
    });

    after(async () => {
      await waitForExit(bobExecutorProcess);
    });

    describe("Agent Language", agentLanguageTests(testContext));
    describe("Language", languageTests(testContext));
    describe("Neighbourhood", neighbourhoodTests(testContext));
    //describe('Direct Messages', directMessageTests(testContext))

    describe("with Alice, Bob and Jim", () => {
      let jimExecutorProcess: ChildProcess | null = null;
      before(async () => {
        const jimAppDataPath = path.join(TEST_DIR, "agents", "jim");
        const [jimGqlPort, jimHcAdminPort, jimHcAppPort] =
          await getFreePorts(3);

        if (!fs.existsSync(jimAppDataPath)) fs.mkdirSync(jimAppDataPath);

        jimExecutorProcess = await startExecutor(
          jimAppDataPath,
          bootstrapSeedPath,
          jimGqlPort,
          jimHcAdminPort,
          jimHcAppPort,
          false,
          undefined,
          proxyUrl!,
          bootstrapUrl!,
          relayUrl!,
        );

        testContext.jim = new Ad4mClient(apolloClient(jimGqlPort));
        testContext.jimCore = jimExecutorProcess;
        await testContext.jim.agent.generate("passphrase");

        const status = await testContext.jim.agent.status();
        expect(status.isInitialized).to.be.true;
        expect(status.isUnlocked).to.be.true;
      });

      after(async () => {
        await waitForExit(jimExecutorProcess);
      });

      describe("Triple Agent", tripleAgentTests(testContext));
    });
  });
});
