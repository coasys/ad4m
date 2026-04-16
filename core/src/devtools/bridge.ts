import type {
  AD4MDevTools,
  CompleteOperationOptions,
  DevToolsState,
  ErrorDetail,
  GetterTraceRecord,
  LanguageRecord,
  NotificationRecord,
  OperationRecord,
  SubscriptionRecord,
  SubscriptionUpdateRecord,
} from './types';
import { PerformanceTracker } from './performance';
import { OperationInterceptor } from './interceptor';
import { SubscriptionTracker } from './subscription-tracker';
import { NotificationMonitor } from './notification-monitor';

const MAX_GETTER_TRACES = 200;
const MAX_SUB_UPDATES = 500;
const MAX_LANGUAGES = 100;

let nextGetterTraceId = 1;

export function initDevToolsBridge(client: any): void {
  if (typeof globalThis === 'undefined') return;

  const existing = (globalThis as any).__AD4M_DEVTOOLS__ as AD4MDevTools | undefined;
  if (existing) {
    existing._client = client;
    if ((globalThis as any).window) {
      (globalThis as any).window.__AD4M_DEVTOOLS__ = existing;
    }
    return;
  }

  const perf = new PerformanceTracker();
  const interceptor = new OperationInterceptor(perf);
  const subscriptions = new SubscriptionTracker();
  const notifications = new NotificationMonitor();

  const subscriptionUpdates: SubscriptionUpdateRecord[] = [];
  const getterTraces: GetterTraceRecord[] = [];
  const languages: LanguageRecord[] = [];
  const getClient = () => ((globalThis as any).__AD4M_DEVTOOLS__?._client || client);

  const connectionState = () => {
    const activeClient = getClient();
    const url = activeClient?.baseUrl || activeClient?.executorUrl || '';
    const authenticated = Boolean(activeClient?.hasAuthToken || activeClient?.authenticated);
    const activeEventStreams = Number(activeClient?.activeEventStreams || 0);

    return {
      connected: Boolean(activeClient),
      transport: 'rest' as const,
      url,
      authenticated,
      eventStreamConnected: activeEventStreams > 0,
      activeEventStreams,
    };
  };

  const enrichErrors = (errors?: any[]): ErrorDetail[] | undefined =>
    errors?.map(e => ({
      message: e?.message || String(e),
      type: e?.type || e?.name || e?.constructor?.name || (e?.status ? `HTTP ${e.status}` : 'Error'),
      stack: e?.stack,
      nested: e?.networkError
        ? [{
            message: e.networkError.message,
            type: 'NetworkError',
            stack: e.networkError.stack,
          }]
        : undefined,
    }));

  const devtools: AD4MDevTools = {
    _version: '2.1.0',
    _client: client,

    getState(): DevToolsState {
      const activeSubs = subscriptions.getActiveCount();
      const perfState = perf.getState(interceptor.estimateMemory());
      perfState.activeSubscriptions = activeSubs;

      return {
        operations: interceptor.getAll(),
        subscriptions: subscriptions.getAll(),
        subscriptionUpdates,
        notifications: notifications.getAll(),
        performance: perfState,
        getterTraces,
        languages,
        connection: connectionState(),
      };
    },

    logOperation(op: Partial<OperationRecord>): number {
      return interceptor.log(op);
    },

    completeOperation(id: number, result: any, errors?: any[], options?: CompleteOperationOptions) {
      interceptor.complete(id, result, enrichErrors(errors), options);
    },

    recordEventStreamMessage() {
      perf.recordEventStreamMessage();
    },

    trackSubscription(sub: Partial<SubscriptionRecord>): number {
      return subscriptions.track(sub);
    },

    updateSubscription(id: number, update: Partial<SubscriptionRecord>) {
      subscriptions.update(id, update);
    },

    logSparqlQuery(info: { query: string; modelName: string; perspectiveUUID: string }) {
      const now = Date.now();
      interceptor.log({
        type: 'trace',
        transport: 'sparql',
        queryLanguage: 'sparql',
        operationName: info.modelName ? `SPARQL Trace • ${info.modelName}` : 'SPARQL Trace',
        query: info.query,
        sparqlQuery: info.query,
        startTime: now,
        endTime: now,
        duration: 0,
      });
      perf.recordSparqlTrace();
    },

    logSubscriptionUpdate(update: SubscriptionUpdateRecord) {
      subscriptionUpdates.push(update);
      if (subscriptionUpdates.length > MAX_SUB_UPDATES) subscriptionUpdates.shift();
      perf.recordSubscriptionUpdate();

      const sub = subscriptions.getAll().find(s => s.id === update.subscriptionId);
      if (sub) {
        subscriptions.update(update.subscriptionId, {
          updateCount: sub.updateCount + 1,
          lastUpdateTimestamp: update.timestamp,
          fingerprintHits: sub.fingerprintHits + (update.fingerprintChanged ? 0 : 1),
          fingerprintMisses: sub.fingerprintMisses + (update.fingerprintChanged ? 1 : 0),
        });
      }
    },

    logGetterTrace(trace: Omit<GetterTraceRecord, 'id' | 'timestamp'>) {
      getterTraces.push({
        ...trace,
        id: nextGetterTraceId++,
        timestamp: Date.now(),
      });
      if (getterTraces.length > MAX_GETTER_TRACES) getterTraces.shift();
    },

    logLanguageEvent(lang: LanguageRecord) {
      const idx = languages.findIndex(l => l.address === lang.address);
      if (idx >= 0) languages[idx] = lang;
      else {
        languages.push(lang);
        if (languages.length > MAX_LANGUAGES) languages.shift();
      }
    },

    registerNotification(notification: NotificationRecord) {
      notifications.register(notification);
    },

    updateNotification(id: string, update: Partial<NotificationRecord>) {
      notifications.update(id, update);
    },

    async testNotificationTrigger(notificationId: string, perspectiveId: string): Promise<any> {
      const n = notifications.getAll().find(n => n.id === notificationId);
      if (!n) return { error: 'Notification not found' };
      try {
        const activeClient = getClient();
        const proxy = await activeClient?.perspective?.byUUID?.(perspectiveId);
        if (!proxy) return { error: 'Perspective not found' };
        const result = await proxy.infer(n.triggerQuery);
        return { success: true, result };
      } catch (e: any) {
        return { error: e.message };
      }
    },

    async queryLinks(perspectiveId: string, filter?: { source?: string; predicate?: string; target?: string }): Promise<any[]> {
      try {
        const activeClient = getClient();
        const proxy = await activeClient?.perspective?.byUUID?.(perspectiveId);
        if (!proxy) return [];
        return await proxy.get(filter || {});
      } catch {
        return [];
      }
    },

    async getSubjectClasses(perspectiveId: string): Promise<any[]> {
      try {
        const activeClient = getClient();
        const proxy = await activeClient?.perspective?.byUUID?.(perspectiveId);
        if (!proxy) return [];
        if (proxy.subjectClasses) return await proxy.subjectClasses();
        const result = await proxy.infer(`SELECT ?class ?property WHERE { ?class a sh:NodeShape . ?class sh:property ?prop . ?prop sh:path ?property } LIMIT 100`);
        return result || [];
      } catch {
        return [];
      }
    },

    async getLanguages(): Promise<any[]> {
      try {
        return await getClient()?.languages?.all?.() || [];
      } catch {
        return [];
      }
    },
  };

  (globalThis as any).__AD4M_DEVTOOLS__ = devtools;
  if ((globalThis as any).window) {
    (globalThis as any).window.__AD4M_DEVTOOLS__ = devtools;
  }
}

export type { AD4MDevTools, DevToolsState } from './types';
