import { useState, useEffect, useRef, useCallback } from "react";
import { PerspectiveProxy, Ad4mModel, Query, Subscription } from "@coasys/ad4m";

type ModelCtor<T extends Ad4mModel> = (new (...args: any[]) => T) & typeof Ad4mModel;

type Props<T extends Ad4mModel> = {
  perspective: PerspectiveProxy;
  model: string | ModelCtor<T>;
  query?: Query;
  /** When set, enables load-more / infinite-scroll mode. */
  pageSize?: number;
  preserveReferences?: boolean;
};

type Result<T extends Ad4mModel> = {
  entries: T[];
  loading: boolean;
  error: string;
  totalCount: number;
  loadMore: () => void;
};

export function useModel<T extends Ad4mModel>(props: Props<T>): Result<T> {
  const { perspective, model, query = {}, preserveReferences = false, pageSize } = props;

  const [loading, setLoading] = useState(true);
  const [entries, setEntries] = useState<T[]>([]);
  const [error, setError] = useState<string>("");
  const [pageNumber, setPageNumber] = useState(1);
  const [totalCount, setTotalCount] = useState(0);

  // Stable ref to the active subscription — cleanup always has the right handle
  const subRef = useRef<Subscription | null>(null);

  // Stable serialisation for query object dependency
  const queryKey = JSON.stringify(query);

  function buildLiveQuery(): Query {
    // Growing-window strategy for load-more: always fetch from the start up to
    // the current limit so reactions/replies on earlier items stay live.
    return pageSize ? { ...query, limit: pageSize * pageNumber } : query;
  }

  function mergeEntries(oldEntries: T[], newEntries: T[]): T[] {
    if (!preserveReferences) return newEntries;
    const existingMap = new Map(oldEntries.map((e) => [e.id, e]));
    return newEntries.map((n) => existingMap.get(n.id) ?? n);
  }

  function buildQueryBuilder(q: Query) {
    return typeof model === "string"
      ? Ad4mModel.query(perspective, q).overrideModelClassName(model)
      : (model as ModelCtor<T>).query(perspective, q);
  }

  const subscribe = useCallback(() => {
    if (!perspective) return;

    // Tear down previous subscription before creating a new one
    subRef.current?.unsubscribe();
    subRef.current = null;
    setLoading(true);

    try {
      subRef.current = buildQueryBuilder(buildLiveQuery()).live(
        (results) => {
          setEntries((old) => mergeEntries(old, results as T[]));
          setLoading(false);
        },
        {
          onError: (err) => {
            setError(err.message);
            setLoading(false);
          },
        }
      );
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
      setLoading(false);
    }

    // Fetch totalCount independently — only used in load-more UI ("X of Y")
    if (pageSize) {
      buildQueryBuilder(query)
        .count()
        .then(setTotalCount)
        .catch(console.error);
    }
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [perspective?.uuid, model, pageNumber, queryKey]);

  useEffect(() => {
    subscribe();
    return () => {
      subRef.current?.unsubscribe();
      subRef.current = null;
    };
  }, [subscribe]);

  const loadMore = useCallback(() => {
    if (pageSize) setPageNumber((p) => p + 1);
  }, [pageSize]);

  return { entries, loading, error, totalCount, loadMore };
}
