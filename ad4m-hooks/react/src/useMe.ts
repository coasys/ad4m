import { useMemo } from "react";
import { Ad4mClient, Agent, AgentStatus, LinkExpression } from "@coasys/ad4m";
import { useLive } from "./useLive";

type MyInfo<T> = {
  me?: Agent;
  status?: AgentStatus;
  profile: T | null;
  error: string | undefined;
};

/**
 * The agent and its status, read from the client's mirror: `agent-updated` and
 * `agent-status-changed` replace them and re-render, a reconnect reloads them. Two
 * components using this hook with one client share the entries; two clients share nothing.
 */
export function useMe<T>(client: Ad4mClient | undefined, formatter: (links: LinkExpression[]) => T): MyInfo<T> {
  const status = useLive(useMemo(() => client?.agent.live.status(), [client]));
  const me = useLive(useMemo(() => client?.agent.live.me(), [client]));

  const perspective = me?.value?.perspective;
  const profile = useMemo(
    () => (perspective && formatter ? formatter(perspective.links) : null),
    [perspective]
  );
  const failure = me?.error ?? status?.error;

  return {
    status: status?.value,
    me: me?.value,
    profile,
    error: failure === undefined ? undefined : String(failure),
  };
}
