import { useState, useCallback, useEffect } from "react";
import { getCache, setCache, subscribe, unsubscribe, getProfile } from "@coasys/hooks-helpers";
import { AgentClient, LinkExpression } from "@coasys/ad4m";
import { Agent } from '@coasys/ad4m'

type Props<T> = {
    client: AgentClient;
    did: string | (() => string);
    formatter: (links: LinkExpression[]) => T;
};

export function useAgent<T>(props: Props<T>) {
  const forceUpdate = useForceUpdate();
  const [error, setError] = useState<string | undefined>(undefined);
  const [profile, setProfile] = useState<T | null>(null);
  const didRef = typeof props.did === "function" ? props.did() : props.did;

  // Create cache key for entry
  const cacheKey = `agents/${didRef}`;

  // Mutate shared/cached data for all subscribers
  const mutate = useCallback(
    (agent: Agent | null) => setCache(cacheKey, agent),
    [cacheKey]
  );

  // Fetch data from AD4M and save to cache
  const getData = useCallback(() => {
    if (didRef) {
        if (props.formatter) {
            getProfile(didRef).then(profile => setProfile(props.formatter(profile.perspective.links)))
        }

      props.client
        .byDID(didRef)
        .then(async (agent) => {
          setError(undefined);
          mutate(agent);
        })
        .catch((error) => setError(error.toString()));
    }
  }, [cacheKey]);

  // Trigger initial fetch
  useEffect(getData, [getData]);

  // Subscribe to changes (re-render on data change)
  useEffect(() => {
    subscribe(cacheKey, forceUpdate);
    return () => unsubscribe(cacheKey, forceUpdate);
  }, [cacheKey, forceUpdate]);

  const agent = getCache<Agent>(cacheKey);

  return { agent, profile, error, mutate, reload: getData };
}

function useForceUpdate() {
  const [, setState] = useState<number[]>([]);
  return useCallback(() => setState([]), [setState]);
}
