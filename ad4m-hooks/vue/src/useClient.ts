import { ref, onMounted, watch } from 'vue';
import { getCache, setCache } from '@coasys/hooks-helpers';
// @ts-ignore
import { getAd4mClient } from '@coasys/ad4m-connect/utils';

export function useClient() {
  const error = ref(null);
  const client = ref(null);

  // Create cache key for entry
  const cacheKey = 'client';

  // Mutate shared/cached data for all subscribers
  const mutate = (client) => {
    setCache(cacheKey, client);
  };

  // Fetch data from AD4M and save to cache
  const getData = async () => {
    console.debug('🪝 useClient - running getAd4mClient');
    try {
      const client = await getAd4mClient();
      error.value = null;
      mutate(client);
    } catch (error) {
      error.value = error.toString();
    }
  };

  // Trigger initial fetch
  onMounted(getData);

  // Subscribe to changes (re-render on data change)
  watch(
    () => getCache(cacheKey),
    (newClient) => {
      client.value = newClient;
    },
    { immediate: true }
  );

  return {
    client,
    error,
    mutate,
    reload: getData,
  };
}