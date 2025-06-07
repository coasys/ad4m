import { useState, useCallback, useEffect, useMemo } from "react";
import { getCache, setCache, subscribe, unsubscribe } from "@coasys/hooks-helpers";
import { Agent, AgentStatus, LinkExpression } from "@coasys/ad4m";
import { AgentClient } from "@coasys/ad4m";

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

export function useMe<T>(agent: AgentClient | undefined, formatter: (links: LinkExpression[]) => T): MyInfo<T> {
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
    if (!agent) {
      return;
    }

    const promises = Promise.all([agent.status(), agent.me()]);

    promises
      .then(async ([status, agent]) => {
        setError(undefined);
        mutate({ agent, status });
      })
      .catch((error) => setError(error.toString()));
  }, [agent, mutate]);

  // Trigger initial fetch
  useEffect(getData, [getData]);

  // Subscribe to changes (re-render on data change)
  useEffect(() => {
    subscribe(cacheKey, forceUpdate);
    return () => unsubscribe(cacheKey, forceUpdate);
  }, [cacheKey, forceUpdate]);

  // Listen to remote changes
  useEffect(() => {
    const changed = (status: AgentStatus) => {
      const newMeData = { agent: data?.agent, status };
      mutate(newMeData);
      return null;
    };

    const updated = (agent: Agent) => {
      const newMeData = { agent, status: data?.status };
      mutate(newMeData);
      return null;
    };

    if (agent) {
      agent.addAgentStatusChangedListener(changed);
      agent.addUpdatedListener(updated);

      // TODO need a way to remove listeners
    }
  }, [agent]);

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
