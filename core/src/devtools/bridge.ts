import type { AD4MDevTools, DevToolsState, NotificationRecord, OperationRecord, SubscriptionRecord } from './types';
import { PerformanceTracker } from './performance';
import { OperationInterceptor } from './interceptor';
import { SubscriptionTracker } from './subscription-tracker';
import { NotificationMonitor } from './notification-monitor';

export function initDevToolsBridge(client: any): void {
  if (typeof globalThis === 'undefined') return;
  if ((globalThis as any).__AD4M_DEVTOOLS__) return; // already initialized

  const perf = new PerformanceTracker();
  const interceptor = new OperationInterceptor(perf);
  const subscriptions = new SubscriptionTracker();
  const notifications = new NotificationMonitor();

  const devtools: AD4MDevTools = {
    _version: '1.0.0',
    _client: client,

    getState(): DevToolsState {
      const activeSubs = subscriptions.getActiveCount();
      const perfState = perf.getState(interceptor.estimateMemory());
      perfState.activeSubscriptions = activeSubs;

      return {
        operations: interceptor.getAll(),
        subscriptions: subscriptions.getAll(),
        notifications: notifications.getAll(),
        performance: perfState,
        connection: {
          wsConnected: true, // TODO: hook into actual WS state
          url: '',
          authenticated: true,
        },
      };
    },

    logOperation(op: Partial<OperationRecord>): number {
      return interceptor.log(op);
    },

    completeOperation(id: number, result: any, errors?: any[]) {
      interceptor.complete(id, result, errors);
    },

    trackSubscription(sub: Partial<SubscriptionRecord>): number {
      return subscriptions.track(sub);
    },

    updateSubscription(id: number, update: Partial<SubscriptionRecord>) {
      subscriptions.update(id, update);
    },

    logSparqlQuery(info: { query: string; modelName: string; perspectiveUUID: string }) {
      interceptor.log({
        type: 'query',
        operationName: `SPARQL:${info.modelName}`,
        query: info.query,
        sparqlQuery: info.query,
        startTime: Date.now(),
      });
      perf.sparqlQueryCount++;
    },

    registerNotification(notification: NotificationRecord) {
      notifications.register(notification);
    },

    updateNotification(id: string, update: Partial<NotificationRecord>) {
      notifications.update(id, update);
    },
  };

  (globalThis as any).__AD4M_DEVTOOLS__ = devtools;
}

export type { AD4MDevTools, DevToolsState } from './types';
