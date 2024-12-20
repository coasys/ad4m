import { useState, useCallback, useEffect, useMemo, useRef } from "react";
import {
  getCache,
  setCache,
  subscribe,
  subscribeToPerspective,
  unsubscribe,
  unsubscribeFromPerspective,
} from "@coasys/hooks-helpers";
import { PerspectiveProxy, LinkExpression } from "@coasys/ad4m";
import { QueryOptions, SubjectRepository } from "@coasys/hooks-helpers";

type Props<SubjectClass> = {
  source: string;
  perspective: PerspectiveProxy;
  subject: (new () => SubjectClass) | string;
  query?: QueryOptions;
};

export function useSubjects<SubjectClass>(props: Props<SubjectClass>) {
  const forceUpdate = useForceUpdate();
  const [query, setQuery] = useState(props.query);
  const [isMore, setIsMore] = useState(false);
  const [error, setError] = useState<string | undefined>(undefined);
  const [isLoading, setIsLoading] = useState(false);
  const { perspective, source, subject } = props;
  const timeout = useRef<any>(null);

  // Create cache key for entry
  const cacheKey = `${perspective.uuid}/${source || ""}/${
    typeof subject === "string" ? subject : subject.prototype.className
  }/${query?.uniqueKey}`;

  // Create model
  const Repo = useMemo(() => {
    return new SubjectRepository(subject, {
      perspective: perspective,
      source,
    });
  }, [cacheKey]);

  // Mutate shared/cached data for all subscribers
  const mutate = useCallback(
    (entries: SubjectClass[]) => setCache(cacheKey, entries),
    [cacheKey]
  );

  // Fetch data from AD4M and save to cache
  const getData = useCallback(() => {
    if (source) {
      setIsLoading(true);
      console.debug(`fetching data from remote`, source, query, cacheKey);
      Repo.getAllData(source, query)
        .then((newEntries) => {
          setError(undefined);
          if (query?.infinite) {
            setIsMore(newEntries.length >= query.size);
            // @ts-ignore
            const updated = mergeArrays(entries, newEntries);
            mutate(updated);
          } else {
            mutate(newEntries);
          }
        })
        .catch((error) => {
          setError(error.toString());
        })
        .finally(() => {
          setIsLoading(false);
        });
    }
  }, [cacheKey, query?.page, query?.infinite, query?.size]);

  // Trigger initial fetch
  useEffect(getData, [cacheKey, query?.page, query?.infinite, query?.size]);

  // Get single entry
  async function fetchEntry(id) {
    const entry = (await Repo.getData(id)) as SubjectClass;
    const oldEntries = (getCache(cacheKey) as SubjectClass[]) || [];
    // @ts-ignore
    const isOldEntry = oldEntries?.some((i) => i.id === id);

    const newEntries = isOldEntry
      ? oldEntries?.map((oldEntry) => {
          // @ts-ignore
          const isUpdatedEntry = id === oldEntry.id;
          return isUpdatedEntry ? entry : oldEntry;
        })
      : [...oldEntries, entry];

    mutate(newEntries);
  }

  async function linkAdded(link: LinkExpression) {
    if (timeout.current) clearTimeout(timeout.current);
    timeout.current = setTimeout(getData, 1000);

    const isNewEntry = link.data.source === source;
    const allEntries = (getCache(cacheKey) || []) as SubjectClass[];
    const isUpdated = allEntries?.find((e: any) => e.id === link.data.source);

    const id = isNewEntry
      ? link.data.target
      : isUpdated
        ? link.data.source
        : false;

    if (id) {
      const isInstance = await perspective.isSubjectInstance(
        id,
        typeof subject === "string" ? subject : new subject()
      );

      if (isInstance) {
        fetchEntry(id);
      }
    }

    return null;
  }

  async function linkRemoved(link: LinkExpression) {
    const allEntries = (getCache(cacheKey) || []) as SubjectClass[];

    // Check if an association/property was removed
    const removedAssociation = allEntries.some(
      // @ts-ignore
      (e) => e.id === link.data.source
    );

    if (removedAssociation) {
      getData();
    }

    // Remove entries if they are removed from source
    if (link.data.source === source) {
      // @ts-ignore
      const newEntries = allEntries?.filter((e) => e.id !== link.data.target);
      mutate(newEntries || []);
    }
    return null;
  }

  // Listen to remote changes
  useEffect(() => {
    if (perspective.uuid) {
      subscribeToPerspective(perspective, linkAdded, linkRemoved);

      return () => {
        unsubscribeFromPerspective(perspective, linkAdded, linkRemoved);
      };
    }
  }, [perspective.uuid, cacheKey, query]);

  // Subscribe to changes (re-render on data change)
  useEffect(() => {
    subscribe(cacheKey, forceUpdate);
    return () => unsubscribe(cacheKey, forceUpdate);
  }, [cacheKey, forceUpdate, query]);

  type ExtendedSubjectClass = SubjectClass & {
    id: string;
    timestamp: number;
    author: string;
  };

  const entries = (getCache(cacheKey) || []) as ExtendedSubjectClass[];

  return {
    entries: [...entries],
    error,
    mutate,
    setQuery,
    repo: Repo,
    isLoading,
    reload: getData,
    isMore,
  };
}

function useForceUpdate() {
  const [, setState] = useState<number[]>([]);
  return useCallback(() => setState([]), [setState]);
}

interface MyObject {
  id: number;
  [key: string]: any;
}

function mergeArrays(arr1: MyObject[], arr2: MyObject[]): MyObject[] {
  const map = new Map<number, MyObject>();

  // Function to add objects from array to map
  function addArrayToMap(arr: MyObject[]) {
    for (const obj of arr) {
      if (obj && obj.id != null) {
        // Ensure object and id property exist
        map.set(obj.id, obj);
      }
    }
  }

  // Add objects from both arrays to map
  addArrayToMap(arr1);
  addArrayToMap(arr2);

  // Convert map values to an array and return it
  return Array.from(map.values());
}
