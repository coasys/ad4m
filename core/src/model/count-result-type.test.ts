/**
 * Type test: the results of the calls that always ask for the count
 * (`findAllAndCount()`, `paginate()`, `paginateSubscribe()`) have a required
 * `totalCount`, so a consumer under `strictNullChecks` does not see
 * "possibly undefined". ts-jest type-checks this file; the optional modifier
 * is visible to `IsRequired` with or without `strictNullChecks`.
 *
 * At run time those calls, and `count()`, throw when the executor's answer has
 * no `totalCount` instead of reporting zero matches.
 */
import { Ad4mModel } from './Ad4mModel';
import { ModelQueryBuilder } from './ModelQueryBuilder';
import { Model, Property } from './decorators';
import { PerspectiveProxy } from '../perspectives/PerspectiveProxy';
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

describe('an answer without the count that was asked for', () => {
  @Model({ name: 'CountedNote' })
  class CountedNote extends Ad4mModel {
    @Property({ through: 'cn://title' })
    title: string = '';
  }

  // A perspective whose executor answers every model query without `totalCount`.
  const perspective = new PerspectiveProxy(
    { uuid: 'cn-uuid', name: 'cn', owners: [], sharedUrl: null, neighbourhood: null, state: 'Synced' } as any,
    new Proxy({} as any, {
      get: (_target, method: string) => {
        if (method === 'then') return undefined;
        return async () => {
          if (method === 'modelQuery') return { instances: [] };
          throw new Error(`unexpected RPC ${method}`);
        };
      },
    }),
  );

  it.each([
    ['Model.count', () => CountedNote.count(perspective)],
    ['Model.findAllAndCount', () => CountedNote.findAllAndCount(perspective)],
    ['Model.paginate', () => CountedNote.paginate(perspective, 10, 1)],
    ['query().count', () => CountedNote.query(perspective).count()],
    ['query().paginate', () => CountedNote.query(perspective).paginate(10, 1)],
  ])('%s throws naming totalCount', async (_name, call) => {
    await expect(call()).rejects.toThrow(/totalCount/);
  });
});
