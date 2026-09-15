import type { PluginConfig, WakerSubscription } from "./types";
import type { MentionMessage } from "./wakerSubscriptionManager";

export function buildWakeMessage(
  config: PluginConfig,
  sub: WakerSubscription,
  agentDid: string,
  mentions?: MentionMessage[],
): string {
  const event =
    sub.type === "mention"
      ? "You were @mentioned in an AD4M neighbourhood."
      : "New items in an AD4M neighbourhood.";

  const lines: string[] = [
    event,
    "Read the AD4M skill for instructions on how to handle this.",
    "",
    `Agent DID: ${agentDid}`,
    `Perspective: ${sub.perspective}`,
    `Subscription: ${sub.id}`,
    `Event type: ${sub.type}`,
  ];

  if (mentions && mentions.length > 0) {
    lines.push("");
    // "items", not "messages": the mention query matches any link whose text
    // carries the agent's name, so the address can be an instance of any class
    // the space defines — the agent types it from the ontology, not from here.
    lines.push(`Mentioned items (${mentions.length}):`);
    for (const m of mentions) {
      lines.push(`  Item: ${m.address}`);
      if (m.parents.length > 0) {
        lines.push(`  Parents: ${m.parents.join(", ")}`);
      } else {
        lines.push(`  Parents: (unknown)`);
      }
    }
  }

  return lines.filter(Boolean).join("\n");
}

export async function postWake(
  config: PluginConfig,
  sub: WakerSubscription,
  agentDid: string,
  logger: any,
  mentions?: MentionMessage[],
): Promise<void> {
  logger.info(
    `[ad4m-waker] postWake: sub=${sub.id}, type=${sub.type}, mentions=${mentions?.length ?? 0}`,
  );
  const message = buildWakeMessage(config, sub, agentDid, mentions);
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
