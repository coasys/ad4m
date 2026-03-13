import type { PluginConfig, WakerSubscription } from "./types";

export function buildWakeMessage(
  config: PluginConfig,
  sub: WakerSubscription,
  agentDid: string,
  parent: string,
): string {
  const event =
    sub.type === "mention"
      ? "You were @mentioned in an AD4M neighbourhood."
      : "New messages in an AD4M neighbourhood.";

  return [
    event,
    "Read the AD4M skill for instructions on how to handle this.",
    "",
    `Agent DID: ${agentDid}`,
    `Perspective: ${sub.perspective}`,
    parent ? `Parent: ${parent}` : null,
    `Subscription: ${sub.id}`,
    `Event type: ${sub.type}`,
  ]
    .filter(Boolean)
    .join("\n");
}

export async function postWake(
  config: PluginConfig,
  sub: WakerSubscription,
  agentDid: string,
  logger: any,
  parentChannel?: string,
): Promise<void> {
  const effectiveChannel = parentChannel || sub.channel;
  logger.info(
    `[ad4m-waker] postWake: sub=${sub.id}, type=${sub.type}, parentChannel=${parentChannel ?? "(none)"}, effectiveChannel=${effectiveChannel ?? "(none)"}`,
  );
  const message = buildWakeMessage(config, sub, agentDid, effectiveChannel);
  const body = JSON.stringify({ text: message, mode: "now" });
  logger.debug(`[ad4m-waker] wake body: ${message}`);

  try {
    const resp = await fetch(config.wakeUrl!, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${config.wakeToken}`,
      },
      body,
      signal: AbortSignal.timeout(5000),
    });
    if (!resp.ok) {
      logger.error(`[ad4m-waker] wake POST failed: ${resp.status}`);
    } else {
      logger.info(`[ad4m-waker] wake POST sent successfully`);
    }
  } catch (e: any) {
    logger.error(`[ad4m-waker] wake POST error: ${e.message}`);
  }
}

