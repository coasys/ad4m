import type { AD4MDevTools, DevToolsState, ErrorDetail, GetterTraceRecord, LanguageRecord, NotificationRecord, OperationRecord, SubscriptionRecord, SubscriptionUpdateRecord } from './types';
import { PerformanceTracker } from './performance';
import { OperationInterceptor } from './interceptor';
import { SubscriptionTracker } from './subscription-tracker';
import { NotificationMonitor } from './notification-monitor';

const MAX_GETTER_TRACES = 200;
const MAX_SUB_UPDATES = 500;
const MAX_LANGUAGES = 100;

let nextGetterTraceId = 1;

export function initDevToolsBridge(client: any): void {
  console.log("[AD4M DevTools] initDevToolsBridge called");
  if (typeof globalThis === 'undefined') return;
  if ((globalThis as any).__AD4M_DEVTOOLS__) return;

  const perf = new PerformanceTracker();
  const interceptor = new OperationInterceptor(perf);
  const subscriptions = new SubscriptionTracker();
  const notifications = new NotificationMonitor();

  const subscriptionUpdates: SubscriptionUpdateRecord[] = [];
  const getterTraces: GetterTraceRecord[] = [];
  const languages: LanguageRecord[] = [];

  const devtools: AD4MDevTools = {
    _version: '2.0.0',
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
        connection: {
          wsConnected: true,
          url: '',
          authenticated: true,
        },
      };
    },

    logOperation(op: Partial<OperationRecord>): number {
      return interceptor.log(op);
    },

    completeOperation(id: number, result: any, errors?: any[]) {
      const enriched: ErrorDetail[] | undefined = errors?.map(e => ({
        message: e?.message || String(e),
        type: e?.constructor?.name || e?.extensions?.code || 'Error',
        stack: e?.stack,
        nested: e?.networkError ? [{ message: e.networkError.message, type: 'NetworkError', stack: e.networkError.stack }] : undefined,
      }));
      interceptor.complete(id, result, enriched);
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

    logSubscriptionUpdate(update: SubscriptionUpdateRecord) {
      subscriptionUpdates.push(update);
      if (subscriptionUpdates.length > MAX_SUB_UPDATES) subscriptionUpdates.shift();
      perf.recordSubscriptionUpdate();
      // Also bump the subscription record
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
        const proxy = await client.perspective.byUUID(perspectiveId);
        if (!proxy) return { error: 'Perspective not found' };
        const result = await proxy.infer(n.triggerQuery);
        return { success: true, result };
      } catch (e: any) {
        return { error: e.message };
      }
    },

    async queryLinks(perspectiveId: string, filter?: { source?: string; predicate?: string; target?: string }): Promise<any[]> {
      try {
        const proxy = await client.perspective.byUUID(perspectiveId);
        if (!proxy) return [];
        return await proxy.get(filter || {});
      } catch {
        return [];
      }
    },

    async getSubjectClasses(perspectiveId: string): Promise<any[]> {
      try {
        const proxy = await client.perspective.byUUID(perspectiveId);
        if (!proxy) return [];
        // Try to get subject classes via the SDNA method
        if (proxy.subjectClasses) return await proxy.subjectClasses();
        // Fallback: infer SHACL shapes
        const result = await proxy.infer(`SELECT ?class ?property WHERE { ?class a sh:NodeShape . ?class sh:property ?prop . ?prop sh:path ?property } LIMIT 100`);
        return result || [];
      } catch {
        return [];
      }
    },

    async getLanguages(): Promise<any[]> {
      try {
        if (client.languages?.all) return await client.languages.all();
        return [];
      } catch {
        return [];
      }
    },
  };

  (globalThis as any).__AD4M_DEVTOOLS__ = devtools;
  (globalThis as any).window && ((globalThis as any).window.__AD4M_DEVTOOLS__ = devtools);
  console.log("[AD4M DevTools] Bridge initialized on globalThis and window", Object.keys(devtools));
}

export type { AD4MDevTools, DevToolsState } from './types';
