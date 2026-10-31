/**
 * Type test: the results of the calls that always ask for the count
 * (`findAllAndCount()`, `paginate()`, `paginateSubscribe()`) have a required
 * `totalCount`, so a consumer under `strictNullChecks` does not see
 * "possibly undefined". ts-jest type-checks this file; the optional modifier
 * is visible to `IsRequired` with or without `strictNullChecks`.
 */
import { Ad4mModel } from './Ad4mModel';
import { ModelQueryBuilder } from './ModelQueryBuilder';
import type { PaginationResult, ResultsWithTotalCount } from './types';

type IsRequired<T, K extends keyof T> = {} extends Pick<T, K> ? false : true;

class Note extends Ad4mModel {}

type FindAllAndCount = Awaited<ReturnType<typeof Note.findAllAndCount<Note>>>;
type StaticPaginate = Awaited<ReturnType<typeof Note.paginate<Note>>>;
type BuilderPaginate = Awaited<ReturnType<ModelQueryBuilder<Note>['paginate']>>;
type BuilderPaginateSubscribe = Awaited<ReturnType<ModelQueryBuilder<Note>['paginateSubscribe']>>;
type PaginateCallbackPage = Parameters<Parameters<ModelQueryBuilder<Note>['paginateSubscribe']>[2]>[0];

describe('count results type', () => {
  it('totalCount is required where the call always asks for the count', () => {
    const required: [
      IsRequired<ResultsWithTotalCount<Note>, 'totalCount'>,
      IsRequired<PaginationResult<Note>, 'totalCount'>,
      IsRequired<FindAllAndCount, 'totalCount'>,
      IsRequired<StaticPaginate, 'totalCount'>,
      IsRequired<BuilderPaginate, 'totalCount'>,
      IsRequired<BuilderPaginateSubscribe, 'totalCount'>,
      IsRequired<PaginateCallbackPage, 'totalCount'>,
    ] = [true, true, true, true, true, true, true];
    expect(required.every(Boolean)).toBe(true);
  });
});
