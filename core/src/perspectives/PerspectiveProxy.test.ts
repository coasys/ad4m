import { QuerySubscriptionProxy } from './PerspectiveProxy';

describe('QuerySubscriptionProxy', () => {
  it('treats the initial subscribeQuery() result as a completed initialization', async () => {
    const initialResult = [
      {
        source: 'literal:string:test-channel',
        predicate: 'flux://has_channel_name',
        target: 'literal:string:Runtime Fix Channel',
      },
    ];

    const unsubscribe = jest.fn();
    const mockClient = {
      subscribeQuery: jest.fn().mockResolvedValue({
        subscriptionId: 'sub-1',
        result: initialResult,
      }),
      subscribeToQueryUpdates: jest.fn().mockReturnValue(unsubscribe),
      keepAliveQuery: jest.fn().mockResolvedValue(true),
      disposeQuerySubscription: jest.fn().mockResolvedValue(true),
    } as any;

    const subscription = new QuerySubscriptionProxy('perspective-1', 'SELECT * WHERE { ?s ?p ?o }', mockClient);

    await subscription.subscribe();

    const initialized = await Promise.race([
      subscription.initialized.then(() => 'resolved'),
      new Promise<string>((resolve) => setTimeout(() => resolve('timeout'), 25)),
    ]);

    expect(initialized).toBe('resolved');
    expect(subscription.result).toEqual(initialResult);
    expect(mockClient.subscribeQuery).toHaveBeenCalledWith('perspective-1', 'SELECT * WHERE { ?s ?p ?o }');
    expect(mockClient.subscribeToQueryUpdates).toHaveBeenCalledWith('sub-1', expect.any(Function));

    subscription.dispose();
    expect(unsubscribe).toHaveBeenCalled();
  });
});
