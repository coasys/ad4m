import { useState, useCallback, useEffect, useMemo } from "react";
import {
  getCache,
  setCache,
  subscribe,
  subscribeToPerspective,
  unsubscribe,
  unsubscribeToPerspective,
} from "@coasys/hooks-helpers";
import { PerspectiveProxy, LinkExpression } from "@coasys/ad4m";
import { SubjectRepository } from "@coasys/hooks-helpers";

type Props<SubjectClass> = {
  id: string;
  perspective: PerspectiveProxy;
  subject: string | (new () => SubjectClass);
};

export function useSubject<SubjectClass>(props: Props<SubjectClass>) {
  const forceUpdate = useForceUpdate();
  const [error, setError] = useState<string | undefined>(undefined);
  const { perspective, id, subject } = props;

  // Create subject
  const Repo = useMemo(() => {
    return new SubjectRepository(subject, {
      perspective: perspective,
      source: null,
    });
  }, [perspective.uuid, subject]);

  // Create cache key for entry
  // @ts-ignore
  const cacheKey = `${perspective.uuid}/${subject.name}/${id}`;

  // Mutate shared/cached data for all subscribers
  const mutate = useCallback(
    (entry: SubjectClass | null) => setCache(cacheKey, entry),
    [cacheKey]
  );

  // Fetch data from AD4M and save to cache
  const getData = useCallback(() => {
    if (id) {
      Repo.getData(id)
        .then(async (entry) => {
          setError(undefined);
          mutate(entry);
        })
        .catch((error) => setError(error.toString()));
    }
  }, [cacheKey]);

  // Trigger initial fetch
  useEffect(getData, [getData]);

  async function linkAdded(link: LinkExpression) {
    const isUpdated = link.data.source === id;

    if (isUpdated) {
      getData();
    }

    return null;
  }

  async function linkRemoved(link: LinkExpression) {
    if (link.data.source === id) {
      getData();
    }
    return null;
  }

  // Listen to remote changes
  useEffect(() => {
    if (perspective.uuid) {
      subscribeToPerspective(perspective, linkAdded, linkRemoved);

      return () => {
        unsubscribeToPerspective(perspective, linkAdded, linkRemoved);
      };
    }
  }, [perspective.uuid, id]);

  // Subscribe to changes (re-render on data change)
  useEffect(() => {
    subscribe(cacheKey, forceUpdate);
    return () => unsubscribe(cacheKey, forceUpdate);
  }, [cacheKey, forceUpdate]);

  type ExtendedSubjectClass = SubjectClass & {
    id: string;
    timestamp: number;
    author: string;
  };

  const entry = getCache<ExtendedSubjectClass>(cacheKey);

  return { entry, error, mutate, repo: Repo, reload: getData };
}

function useForceUpdate() {
  const [, setState] = useState<number[]>([]);
  return useCallback(() => setState([]), [setState]);
}
