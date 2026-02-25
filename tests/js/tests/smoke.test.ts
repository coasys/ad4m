import { expect } from "chai";
import { ChildProcess } from "node:child_process";
import { Ad4mClient } from "@coasys/ad4m";
import { startExecutor, apolloClient, waitForExit } from "../utils/utils";
import { getFreePorts } from "../helpers/ports";
import path from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("Integration", () => {
  const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
  const appDataPath = path.join(TEST_DIR, "agents", "simpleAlice");
  const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
  let gqlPort: number;
  let hcAdminPort: number;
  let hcAppPort: number;

  let ad4m: Ad4mClient | null = null;
  let executorProcess: ChildProcess | null = null;

  before(async () => {
    [gqlPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
    executorProcess = await startExecutor(
      appDataPath,
      bootstrapSeedPath,
      gqlPort,
      hcAdminPort,
      hcAppPort,
    );

    console.log("Creating ad4m client");
    ad4m = new Ad4mClient(apolloClient(gqlPort));
    console.log("Generating agent");
    await ad4m.agent.generate("secret");
    console.log("Done");
  });

  after(async () => {
    await waitForExit(executorProcess);
  });

  it("should get agent status", async () => {
    let result = await ad4m!.agent.status();
    expect(result).to.not.be.null;
    expect(result!.isInitialized).to.be.true;
  });
});
