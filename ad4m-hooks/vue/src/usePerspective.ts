import { ref, watch, shallowRef } from "vue";
import { usePerspectives } from "./usePerspectives";
import { Ad4mClient, PerspectiveProxy } from "@coasys/ad4m";

export function usePerspective(client: Ad4mClient, uuid: string | Function) {
  const uuidRef = typeof uuid === "function" ? ref(uuid()) : ref(uuid);

  const { perspectives } = usePerspectives(client);

  const data = shallowRef<{
    perspective: PerspectiveProxy | null;
    synced: boolean;
  }>({
    perspective: null,
    synced: false,
  });

  watch(
    [perspectives, uuidRef],
    ([perspectives, id]) => {
      const pers = perspectives[id];
      data.value = { ...data.value, perspective: pers };
    },
    { immediate: true }
  );

  watch(
    // @ts-ignore
    uuid,
    (id) => {
      uuidRef.value = id as string;
    },
    { immediate: true }
  );

  return { data };
}
