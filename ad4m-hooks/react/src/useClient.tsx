import { useState, useCallback, useEffect } from "react";
import { getCache, setCache, subscribe, unsubscribe } from "@coasys/hooks-helpers";
// @ts-ignore
import { getAd4mClient } from "@coasys/ad4m-connect/utils";
import { Ad4mClient } from "@coasys/ad4m";

export function useClient() {
  const forceUpdate = useForceUpdate();
  const [error, setError] = useState<string | undefined>(undefined);

  // Create cache key for entry
  const cacheKey = `client`;

  // Mutate shared/cached data for all subscribers
  const mutate = useCallback(
    (client: Ad4mClient | undefined) => setCache(cacheKey, client),
    [cacheKey]
  );

  // Fetch data from AD4M and save to cache
  const getData = useCallback(() => {
    console.log("🪝 useClient - running getAd4mClient");
    getAd4mClient()
      .then((client) => {
        setError(undefined);
        mutate(client);
      })
      .catch((error) => setError(error.toString()));
  }, [mutate]);

  // Trigger initial fetch
  useEffect(getData, [getData]);

  // Subscribe to changes (re-render on data change)
  useEffect(() => {
    subscribe(cacheKey, forceUpdate);
    return () => unsubscribe(cacheKey, forceUpdate);
  }, [cacheKey, forceUpdate]);

  const client = getCache<Ad4mClient>(cacheKey);

  return {
    client,
    error,
    mutate,
    reload: getData,
  };
}

function useForceUpdate() {
  const [, setState] = useState<number[]>([]);
  return useCallback(() => setState([]), [setState]);
}
