export interface OperationRecord {
  id: number;
  type: 'query' | 'mutation' | 'subscription';
  operationName: string;
  query: string;
  variables?: Record<string, any>;
  response?: any;
  errors?: ErrorDetail[];
  startTime: number;
  endTime?: number;
  duration?: number;
  payloadSize?: number;
  sparqlQuery?: string;
  sparqlResult?: any;
}

export interface ErrorDetail {
  message: string;
  type?: string;
  stack?: string;
  nested?: ErrorDetail[];
}

export interface SubscriptionRecord {
  id: number;
  query: string;
  perspectiveUUID: string;
  modelName: string;
  updateCount: number;
  lastUpdateTimestamp: number;
  fingerprintHits: number;
  fingerprintMisses: number;
  callbackTimings: number[];
  active: boolean;
}

export interface SubscriptionUpdateRecord {
  subscriptionId: number;
  rawResultCount: number;
  processedCount: number;
  fingerprintChanged: boolean;
  timestamp: number;
}

export interface NotificationRecord {
  id: string;
  triggerQuery: string;
  lastResult?: any;
  lastError?: string;
  matchHistory: Array<{ timestamp: number; matched: boolean }>;
  registered: number;
}

export interface GetterTraceRecord {
  id: number;
  property: string;
  getterType: 'sparql' | 'legacy';
  query: string;
  result: any;
  error: string | null;
  duration: number;
  instanceId: string;
  timestamp: number;
}

export interface LanguageRecord {
  name: string;
  address: string;
  loadStatus: 'loading' | 'loaded' | 'error';
  loadTime?: number;
  error?: string;
  timestamp: number;
}

export interface PerformanceState {
  totalQueries: number;
  totalErrors: number;
  avgRTT: number;
  peakRTT: number;
  queriesPerSecond: number;
  sparqlQueryCount: number;
  prologQueryCount: number;
  activeSubscriptions: number;
  subscriptionUpdateRate: number;
  wsMessageRate: number;
  estimatedMemory: number;
}

export interface DevToolsState {
  operations: OperationRecord[];
  subscriptions: SubscriptionRecord[];
  subscriptionUpdates: SubscriptionUpdateRecord[];
  notifications: NotificationRecord[];
  performance: PerformanceState;
  getterTraces: GetterTraceRecord[];
  languages: LanguageRecord[];
  connection: {
    wsConnected: boolean;
    url: string;
    authenticated: boolean;
  };
}

export interface AD4MDevTools {
  getState(): DevToolsState;
  trackSubscription(sub: Partial<SubscriptionRecord>): number;
  updateSubscription(id: number, update: Partial<SubscriptionRecord>): void;
  logSparqlQuery(info: { query: string; modelName: string; perspectiveUUID: string }): void;
  logOperation(op: Partial<OperationRecord>): number;
  completeOperation(id: number, result: any, errors?: any[]): void;
  registerNotification(notification: NotificationRecord): void;
  updateNotification(id: string, update: Partial<NotificationRecord>): void;
  logSubscriptionUpdate(update: SubscriptionUpdateRecord): void;
  logGetterTrace(trace: Omit<GetterTraceRecord, 'id' | 'timestamp'>): void;
  logLanguageEvent(lang: LanguageRecord): void;
  testNotificationTrigger(notificationId: string, perspectiveId: string): Promise<any>;
  queryLinks(perspectiveId: string, filter?: { source?: string; predicate?: string; target?: string }): Promise<any[]>;
  getSubjectClasses(perspectiveId: string): Promise<any[]>;
  getLanguages(): Promise<any[]>;
  _client?: any;
  _version: string;
}

declare global {
  interface Window {
    __AD4M_DEVTOOLS__?: AD4MDevTools;
  }
}
