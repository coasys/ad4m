import { ref, effect, shallowRef, watch } from "vue";
import { Ad4mClient, PerspectiveProxy } from "@coasys/ad4m";

type UUID = string;

const perspectives = shallowRef<{ [x: UUID]: PerspectiveProxy }>({});
const neighbourhoods = shallowRef<{ [x: UUID]: PerspectiveProxy }>({});
const onAddedLinkCbs = ref<Function[]>([]);
const onRemovedLinkCbs = ref<Function[]>([]);
const hasFetched = ref(false);

watch(
  () => perspectives.value,
  (newPers) => {
    neighbourhoods.value = Object.keys(newPers).reduce((acc, key) => {
      if (newPers[key]?.sharedUrl) {
        return {
          ...acc,
          [key]: newPers[key],
        };
      } else {
        return acc;
      }
    }, {});
  },
  { immediate: true }
);

function addListeners(p: PerspectiveProxy) {
  p.addListener("link-added", (link) => {
    onAddedLinkCbs.value.forEach((cb) => {
      cb(p, link);
    });
    return null;
  });

  p.removeListener("link-removed", (link) => {
    onAddedLinkCbs.value.forEach((cb) => {
      cb(p, link);
    });
    return null;
  });
}

export function usePerspectives(client: Ad4mClient) {
  effect(async () => {
    if (hasFetched.value) return;
    // First component that uses this hook will set this to true,
    // so the next components will not fetch and add listeners
    hasFetched.value = true;

    // Get all perspectives
    const allPerspectives = await client.perspective.all();

    perspectives.value = allPerspectives.reduce((acc, p) => {
      return { ...acc, [p.uuid]: p };
    }, {});

    // Add each perspective to our state
    allPerspectives.forEach((p) => {
      addListeners(p);
    });

    // @ts-ignore
    client.perspective.addPerspectiveUpdatedListener(async (handle) => {
      const perspective = await client.perspective.byUUID(handle.uuid);

      if (perspective) {
        perspectives.value = {
          ...perspectives.value,
          [handle.uuid]: perspective,
        };
      }
      return null;
    });

    // Add new incoming perspectives
    // @ts-ignore
    client.perspective.addPerspectiveAddedListener(async (handle) => {
      const perspective = await client.perspective.byUUID(handle.uuid);

      if (perspective) {
        perspectives.value = {
          ...perspectives.value,
          [handle.uuid]: perspective,
        };
        addListeners(perspective);
      }
    });

    // Remove new deleted perspectives
    client.perspective.addPerspectiveRemovedListener((uuid) => {
      perspectives.value = Object.keys(perspectives.value).reduce(
        (acc, key) => {
          const p = perspectives.value[key];
          return key === uuid ? acc : { ...acc, [key]: p };
        },
        {}
      );
      return null;
    });
  }, {});

  function fetchPerspectives() {}

  function onLinkAdded(cb: Function) {
    onAddedLinkCbs.value.push(cb);
  }

  function onLinkRemoved(cb: Function) {
    onRemovedLinkCbs.value.push(cb);
  }

  return { perspectives, neighbourhoods, onLinkAdded, onLinkRemoved };
}
