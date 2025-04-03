import { useState, useEffect, useMemo } from "react";
import { PerspectiveProxy, Ad4mModel, Query, PaginationResult } from "@coasys/ad4m";

type Props<T extends Ad4mModel> = {
  perspective: PerspectiveProxy;
  model: string | ((new (...args: any[]) => T) & typeof Ad4mModel);
  query?: Query;
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
  const [subjectEnsured, setSubjectEnsured] = useState(false);
  const [loading, setLoading] = useState(true);
  const [entries, setEntries] = useState<T[]>([]);
  const [error, setError] = useState<string>("");
  const [pageNumber, setPageNumber] = useState(1);
  const [totalCount, setTotalCount] = useState(0);

  async function ensureSubject() {
    if (typeof model !== "string") await perspective.ensureSDNASubjectClass(model);
    setSubjectEnsured(true);
  }

  function preserveEntryReferences(oldEntries: T[], newEntries: T[]): T[] {
    // Merge new results into old results, preserving references for optimized rendering
    const existingMap = new Map(oldEntries.map((entry) => [entry.baseExpression, entry]));
    return newEntries.map((newEntry) => existingMap.get(newEntry.baseExpression) || newEntry);
  }

  function handleNewEntires(newEntries: T[]) {
    setEntries((oldEntries) => (preserveReferences ? preserveEntryReferences(oldEntries, newEntries) : newEntries));
  }

  function paginateSubscribeCallback({ results, totalCount: count }: PaginationResult<T>) {
    handleNewEntires(results);
    setTotalCount(count as number);
  }

  async function subscribeToCollection() {
    try {
      const modelQuery =
        typeof model === "string"
          ? Ad4mModel.query(perspective, query).overrideModelClassName(model)
          : model.query(perspective, query);
      if (pageSize) {
        // Handle paginated results
        const totalPageSize = pageSize * pageNumber;
        const { results, totalCount: count } = await modelQuery.paginateSubscribe(
          totalPageSize,
          1,
          paginateSubscribeCallback as (results: PaginationResult<Ad4mModel>) => void
        );
        setEntries(results as T[]);
        setTotalCount(count as number);
      } else {
        // Handle non-paginated results
        const results = await modelQuery.subscribe(handleNewEntires as (results: Ad4mModel[]) => void);
        setEntries(results as T[]);
      }
    } catch (err) {
      console.log("useAd4mModel error", err);
      setError(err instanceof Error ? err.message : String(err));
    } finally {
      setLoading(false);
    }
  }

  function loadMore() {
    if (pageSize) {
      setLoading(true);
      setPageNumber((prevPage) => prevPage + 1);
    }
  }

  useEffect(() => {
    ensureSubject();
  }, []);

  useEffect(() => {
    if (subjectEnsured) subscribeToCollection();
  }, [subjectEnsured, model, JSON.stringify(query), pageNumber]);

  return useMemo(
    () => ({ entries, loading, error, totalCount, loadMore }),
    [entries, loading, error, totalCount, loadMore]
  );
}
