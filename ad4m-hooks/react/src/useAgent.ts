import { useMemo } from "react";
import { AgentClient, LinkExpression } from "@coasys/ad4m";
import { useLive } from "./useLive";

type Props<T> = {
    client: AgentClient;
    did: string | (() => string);
    formatter: (links: LinkExpression[]) => T;
};

/**
 * The agent with this DID, read from the client's mirror: fresh for 5 minutes, refreshed
 * at once by an `agent-updated` event, read again after a reconnect. Components that read
 * one DID with one client share one entry; two clients share nothing.
 */
export function useAgent<T>(props: Props<T>) {
  const did = typeof props.did === "function" ? props.did() : props.did;
  const live = useMemo(() => (did ? props.client.live.byDID(did) : undefined), [props.client, did]);
  const snapshot = useLive(live);

  const agent = snapshot?.value ?? undefined;
  const perspective = agent?.perspective;
  const profile = useMemo(
    () => (perspective && props.formatter ? props.formatter(perspective.links) : null),
    [perspective]
  );

  return {
    agent,
    profile,
    error: snapshot?.error === undefined ? undefined : String(snapshot.error),
  };
}
