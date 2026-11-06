import type { Agent } from "./Agent";

/**
 * What kind of agent this is, as the agent says itself: one link in its public perspective,
 * `<its did> ad4m://agent_kind <kind>`.
 *
 * Only the agent can write its public perspective, so the kind is its own claim, the same in
 * every space and every app. Nothing checks it: a bot that leaves it out reads as an agent of no
 * declared kind. It exists so an app can tell a program speaking from a person without each
 * community marking its bots by hand.
 */
export const AGENT_KIND = "ad4m://agent_kind";

/** The kind of an agent that is a program acting by itself: a bot, an AI agent, an integration. */
export const AGENT_KIND_BOT = "ad4m://bot";

/** The kind `agent` declares in its public perspective, or `undefined` when it declares none. */
export function agentKind(agent: Pick<Agent, "did" | "perspective"> | null | undefined): string | undefined {
  const link = agent?.perspective?.links?.find(
    (l) => l.data.source === agent.did && l.data.predicate === AGENT_KIND,
  );
  return link?.data.target || undefined;
}

/** Whether `agent` says it is a bot. */
export function isBot(agent: Pick<Agent, "did" | "perspective"> | null | undefined): boolean {
  return agentKind(agent) === AGENT_KIND_BOT;
}
