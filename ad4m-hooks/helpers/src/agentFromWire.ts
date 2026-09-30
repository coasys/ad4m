import { Agent, EventMap, LinkExpression, Perspective } from "@coasys/ad4m";

/** Build the SDK `Agent` class from the plain agent an `agent-updated` event carries. */
export function agentFromWire(wire: EventMap["agent-updated"]["agent"]): Agent {
  const agent = new Agent(wire.did, new Perspective((wire.perspective?.links ?? []).map(LinkExpression.fromWire)));
  if (wire.directMessageLanguage) agent.directMessageLanguage = wire.directMessageLanguage;
  return agent;
}
