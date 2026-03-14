import type { PluginConfig, WakerSubscription } from "./types";

export function buildWakeMessage(
  config: PluginConfig,
  sub: WakerSubscription,
  agentDid: string,
  parent: string,
  allParents?: string[],
): string {
  const event =
    sub.type === "mention"
      ? "You were @mentioned in an AD4M neighbourhood."
      : "New messages in an AD4M neighbourhood.";

  // Build parent info: list all parents so the agent can determine
  // which is the channel vs conversation thread etc.
  let parentLines: string[] = [];
  if (allParents && allParents.length > 1) {
    // Multiple parents — the message belongs to several containers
    // (e.g., a channel AND a conversation thread).
    // List all so the agent can use get_children or Channel_list to identify
    // which is the channel.
    parentLines.push(`Parents (${allParents.length}): ${allParents.join(", ")}`);
    parentLines.push(
      "Note: this message has multiple parents. Use Channel_list or get_children to identify which parent is the channel.",
    );
  } else if (parent) {
    parentLines.push(`Parent: ${parent}`);
  }

  return [
    event,
    "Read the AD4M skill for instructions on how to handle this.",
    "",
    `Agent DID: ${agentDid}`,
    `Perspective: ${sub.perspective}`,
    ...parentLines,
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
  allParents?: string[],
): Promise<void> {
  const effectiveChannel = parentChannel || sub.channel;
  logger.info(
    `[ad4m-waker] postWake: sub=${sub.id}, type=${sub.type}, parentChannel=${parentChannel ?? "(none)"}, allParents=${allParents?.join(", ") ?? "(none)"}, effectiveChannel=${effectiveChannel ?? "(none)"}`,
  );
  const message = buildWakeMessage(config, sub, agentDid, effectiveChannel, allParents);
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
