import { generateRandomPassphrase } from "./config";

export interface AgentResult {
  did: string;
  passphrase?: string;
}

/**
 * Ensure the AD4M agent is initialized and unlocked.
 *
 * Creates a REST Ad4mClient connection to the executor, checks agent
 * status, and generates (first run) or unlocks (subsequent runs) as needed.
 *
 * Returns { did, passphrase? } on success (passphrase only set when a new
 * agent was generated), or null if agent management failed.
 */
export async function ensureAgentReady(
  executorUrl: string,
  adminCredential: string,
  logger: any,
  agentPassphrase?: string,
  /** @internal — pass a pre-built Ad4mClient for testing */
  _testClient?: any,
): Promise<AgentResult | null> {
  const MAX_CONNECT_ATTEMPTS = 10;
  const CONNECT_RETRY_DELAY_MS = 2000;

  let client: any = null;

  try {
    if (_testClient) {
      client = _testClient;
    } else {
      logger.info(
        `[ad4m] Connecting to executor at ${executorUrl} for agent management...`,
      );
    }

    // Retry loop: the REST server may not be ready immediately after
    // the MCP endpoint comes up. We retry the initial status check.
    // When _testClient is provided (tests), skip retries.
    const maxAttempts = _testClient ? 1 : MAX_CONNECT_ATTEMPTS;
    let agentStatus: any = null;
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      try {
        if (!_testClient) {
          const { Ad4mClient } = require("@coasys/ad4m");
          client = new Ad4mClient(executorUrl, adminCredential, false);
        }
        agentStatus = await client.agent.status();
        logger.info(
          `[ad4m] Agent status: initialized=${agentStatus.isInitialized}, unlocked=${agentStatus.isUnlocked}`,
        );
        break; // Connected successfully
      } catch (e: any) {
        if (attempt < maxAttempts) {
          logger.info(
            `[ad4m] Server not ready yet (${e.message}), retrying in ${CONNECT_RETRY_DELAY_MS}ms... (${attempt}/${maxAttempts})`,
          );
          await new Promise((r: any) =>
            setTimeout(r, CONNECT_RETRY_DELAY_MS),
          );
        } else {
          logger.error(
            `[ad4m] Failed to connect to executor after ${maxAttempts} attempt(s): ${e.message}`,
          );
          return null;
        }
      }
    }

    if (!agentStatus) {
      return null;
    }

    if (!agentStatus.isInitialized) {
      // First run — generate new agent
      const passphrase = agentPassphrase || generateRandomPassphrase(32);
      logger.info(`[ad4m] Agent not initialized, generating new agent...`);
      try {
        await client.agent.generate(passphrase);
        // Re-fetch status to get the DID
        const newStatus = await client.agent.status();
        logger.info(
          `[ad4m] Agent generated successfully. DID: ${newStatus.did}`,
        );
        return { did: newStatus.did, passphrase: agentPassphrase ? undefined : passphrase };
      } catch (e: any) {
        logger.error(`[ad4m] Failed to generate agent: ${e.message}`);
        return null;
      }
    } else if (agentStatus.isUnlocked === false) {
      // Previously initialized but locked — try to unlock
      const passphrase = agentPassphrase;
      if (passphrase) {
        logger.info(`[ad4m] Agent is locked, attempting to unlock...`);
        try {
          await client.agent.unlock(passphrase);
          const newStatus = await client.agent.status();
          logger.info(
            `[ad4m] Agent unlocked successfully. DID: ${newStatus.did}`,
          );
          return { did: newStatus.did };
        } catch (e: any) {
          logger.error(`[ad4m] Failed to unlock agent: ${e.message}`);
          logger.warn(
            `[ad4m] You may need to provide the correct agentPassphrase in config or reconfigure.`,
          );
          return null;
        }
      } else {
        logger.error(
          `[ad4m] Agent is locked but no passphrase available. Set agentPassphrase in plugin config.`,
        );
        return null;
      }
    } else {
      // Already initialized and unlocked
      logger.info(`[ad4m] Agent is ready. DID: ${agentStatus.did}`);
      return { did: agentStatus.did };
    }
  } catch (err: any) {
    logger.error(`[ad4m] Agent management failed: ${err.message}`);
    logger.error(
      `[ad4m] Make sure @coasys/ad4m and dependencies are installed (npm install in the plugin directory).`,
    );
    return null;
  }
}
