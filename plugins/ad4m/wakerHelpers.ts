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
    // "items", not "messages": any record can carry the mention link, so the
    // address can be an instance of any class the space defines — the agent
    // types it from the ontology, not from here.
    lines.push(`Mentioned items (${mentions.length}):`);
    for (const m of mentions) {
      lines.push(`  Item: ${m.address}`);
      // Who put the mention on the item, and when: the signer of the link it is
      // in (the mention link to the agent, or the text link that names it), not
      // necessarily whoever wrote the rest of the item.
      lines.push(
        `  Mentioned by: ${m.author ?? "(unknown)"} (signature ${m.verified ? "verified" : "NOT verified"})`,
      );
      if (m.timestamp) lines.push(`  Mentioned at: ${m.timestamp}`);
      // A mention link names the agent on purpose; a name in the text may only use the word.
      if (m.via === "link") lines.push("  Matched by: a mention link to you");
      if (m.via === "text") {
        lines.push(
          "  Matched by: your name or DID in the text, with no mention link (a name used as a word can be a false alarm)",
        );
      }
      if (m.parents.length > 0) {
        lines.push(`  Parents: ${m.parents.join(", ")}`);
      } else {
        lines.push(`  Parents: (unknown)`);
      }
      // Everything above the parents, nearest first — the context an item sits in, given rather
      // than left for the agent to walk to one call at a time.
      if (m.ancestors?.length) {
        lines.push(`  Ancestors: ${m.ancestors.join(", ")}`);
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
): Promise<boolean> {
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
      return false;
    }
    logger.info(`[ad4m-waker] wake POST sent successfully`);
    return true;
  } catch (e: any) {
    logger.error(`[ad4m-waker] wake POST error: ${e.message}`);
    return false;
  }
}
