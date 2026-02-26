import fs from "fs";

const publishingAgentPath = "./tst-tmp/agents/p/ad4m/agent.json";
const bootstrapSeedPath = "./bootstrapSeed.json";

async function main() {
  if (fs.existsSync(publishingAgentPath)) {
    const didData = JSON.parse(fs.readFileSync(publishingAgentPath).toString());
    if (fs.existsSync(bootstrapSeedPath)) {
      const bootstrapSeed = JSON.parse(
        fs.readFileSync(bootstrapSeedPath).toString(),
      );
      const did = didData["did"];
      if (!bootstrapSeed["trustedAgents"].includes(did)) {
        bootstrapSeed["trustedAgents"].push(did);
      }
      fs.writeFileSync(bootstrapSeedPath, JSON.stringify(bootstrapSeed));
    } else {
      throw new Error(
        `Could not find boostrapSeed at path: ${bootstrapSeedPath}`,
      );
    }
  } else {
    throw new Error(
      `Could not find publishingAgent at path: ${publishingAgentPath}`,
    );
  }
}

// Retry wrapper — the publishing-agent file is written by a preceding build
// step and may not be visible immediately (FS sync lag, slow CI machines).
// Retries on ENOENT up to TIMEOUT_MS with exponential backoff; any other error
// is rethrown immediately.
const TIMEOUT_MS = 30_000;
const INITIAL_DELAY_MS = 200;

async function mainWithRetry() {
  const deadline = Date.now() + TIMEOUT_MS;
  let delay = INITIAL_DELAY_MS;

  while (true) {
    try {
      await main();
      return; // success
    } catch (err) {
      const isTransient =
        err?.code === "ENOENT" ||
        (err?.message ?? "").includes("Could not find");

      if (!isTransient) throw err; // hard error — fail immediately

      if (Date.now() + delay > deadline) {
        throw new Error(
          `injectPublishingAgent timed out after ${TIMEOUT_MS}ms waiting for files to appear. Last error: ${err.message}`,
        );
      }

      console.warn(
        `injectPublishingAgent: retrying in ${delay}ms — ${err.message}`,
      );
      await new Promise((resolve) => setTimeout(resolve, delay));
      delay = Math.min(delay * 2, 5_000);
    }
  }
}

mainWithRetry().catch((err) => {
  console.error("injectPublishingAgent failed:", err.message);
  process.exit(1);
});
