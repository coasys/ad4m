import { expect } from "chai";
import { ChildProcess } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { Ad4mClient } from "@coasys/ad4m";
import { startExecutor, baseUrl, sleep, gracefulShutdown } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import path from "path";
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe("Integration", () => {
  const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
  const appDataPath = path.join(TEST_DIR, "agents", "simpleAlice");
  const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);
  let gqlPort: number;
  let hcAdminPort: number;
  let hcAppPort: number;

  let ad4m: Ad4mClient | null = null
  let executorProcess: ChildProcess | null = null

  before(async () => {
    [gqlPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
    registerPorts([gqlPort, hcAdminPort, hcAppPort]);
    executorProcess = await startExecutor(appDataPath, bootstrapSeedPath,
        gqlPort, hcAdminPort, hcAppPort);

    console.log("Creating ad4m client")
    ad4m = new Ad4mClient(baseUrl(gqlPort))
    console.log("Generating agent")
    await ad4m.agent.generate("secret")
    console.log("Done")
  })

  after(async () => {
    await gracefulShutdown(executorProcess, "executor");
    deregisterPorts([gqlPort, hcAdminPort, hcAppPort]);
  })

  it("should get agent status", async () => {
    let result = await ad4m!.agent.status()
    expect(result).to.not.be.null
    expect(result!.isInitialized).to.be.true
  })

  it("uses the Rust-authored agent-language bundle", () => {
    const bundlePath = path.join(
      __dirname,
      "../../../bootstrap-languages/agent-language/build/bundle.js",
    );
    const bundle = readFileSync(bundlePath, "utf8");
    // inline.mjs embeds the wasm bytes as base64 under this exact name;
    // a TS-authored fallback would not contain it.
    expect(bundle).to.include("__wasmB64");
  })
})