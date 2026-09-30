import { useState, useCallback, useEffect } from "react";
import { getCache, setCache, subscribe, unsubscribe } from "@coasys/hooks-helpers";
import { Ad4mClient, Agent, AgentStatus, LinkExpression } from "@coasys/ad4m";

type MeData = {
  agent?: Agent;
  status?: AgentStatus;
};

type MyInfo<T> = {
  me?: Agent;
  status?: AgentStatus;
  profile: T | null;
  error: string | undefined;
  mutate: Function;
  reload: Function;
};

export function useMe<T>(client: Ad4mClient | undefined, formatter: (links: LinkExpression[]) => T): MyInfo<T> {
  const forceUpdate = useForceUpdate();
  const [error, setError] = useState<string | undefined>(undefined);

  // Create cache key for entry
  const cacheKey = `agents/me`;

  // Mutate shared/cached data for all subscribers
  const mutate = useCallback(
    (data: MeData | null) => setCache(cacheKey, data),
    [cacheKey]
  );

  // Fetch data from AD4M and save to cache
  const getData = useCallback(() => {
    if (!client) {
      return;
    }

    const promises = Promise.all([client.agent.status(), client.agent.me()]);

    promises
      .then(async ([status, agent]) => {
        setError(undefined);
        mutate({ agent, status });
      })
      .catch((error) => setError(error.toString()));
  }, [client, mutate]);

  // Trigger initial fetch
  useEffect(getData, [getData]);

  // Subscribe to changes (re-render on data change)
  useEffect(() => {
    subscribe(cacheKey, forceUpdate);
    return () => unsubscribe(cacheKey, forceUpdate);
  }, [cacheKey, forceUpdate]);

  // Listen to remote changes
  useEffect(() => {
    if (!client) return;

    const releases = [
      client.on("agent-status-changed", ({ agent: status }) => {
        const current = getCache<MeData>(cacheKey);
        mutate({ agent: current?.agent, status: new AgentStatus(status) });
      }),
      client.on("agent-updated", ({ agent }) => {
        const current = getCache<MeData>(cacheKey);
        mutate({ agent: Agent.fromWire(agent), status: current?.status });
      }),
    ];

    return () => releases.forEach((release) => release());
  }, [client, cacheKey, mutate]);

  const data = getCache<MeData>(cacheKey);
  let profile = null as T | null;
  const perspective = data?.agent?.perspective;

  if (perspective) {
    if (formatter) {
        profile = formatter(perspective.links)
    }
 
  }

  return {
    status: data?.status,
    me: data?.agent,
    profile,
    error,
    mutate,
    reload: getData,
  };
}

function useForceUpdate() {
  const [, setState] = useState<number[]>([]);
  return useCallback(() => setState([]), [setState]);
}
