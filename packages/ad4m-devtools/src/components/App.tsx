import { useState, useEffect, useCallback } from 'preact/hooks';
import { PerformanceBar } from './PerformanceBar';
import { ConnectionTab } from './ConnectionTab';
import { PerspectivesTab } from './PerspectivesTab';
import { QueriesTab } from './QueriesTab';
import { NotificationsTab } from './NotificationsTab';
import { AgentTab } from './AgentTab';

type Tab = 'connection' | 'perspectives' | 'queries' | 'notifications' | 'agent';

interface DevToolsState {
  operations: any[];
  subscriptions: any[];
  subscriptionUpdates: any[];
  notifications: any[];
  performance: any;
  connection: any;
  getterTraces: any[];
  languages: any[];
}

const EMPTY_STATE: DevToolsState = {
  operations: [],
  subscriptions: [],
  subscriptionUpdates: [],
  notifications: [],
  performance: {
    totalQueries: 0, totalErrors: 0, avgRTT: 0, peakRTT: 0,
    queriesPerSecond: 0, sparqlQueryCount: 0, prologQueryCount: 0,
    activeSubscriptions: 0, subscriptionUpdateRate: 0, wsMessageRate: 0,
    estimatedMemory: 0,
  },
  connection: { wsConnected: false, url: '', authenticated: false },
  getterTraces: [],
  languages: [],
};

function evalInPage(expr: string): Promise<any> {
  return new Promise((resolve) => {
    if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
      chrome.devtools.inspectedWindow.eval(expr, (result: any, err: any) => {
        if (err) resolve(null);
        else resolve(result);
      });
    } else {
      try { resolve(eval(expr)); } catch { resolve(null); }
    }
  });
}

function hasNotificationErrors(notifications: any[]): boolean {
  const surrealPatterns = ['FROM link', 'fn::', 'in.uri', 'out.uri'];
  return notifications.some(n => {
    const q = n.triggerQuery || '';
    return surrealPatterns.some(p => q.includes(p)) || n.lastError;
  });
}

function exportState(state: DevToolsState) {
  const exportData = {
    operations: state.operations,
    subscriptions: state.subscriptions,
    subscriptionUpdates: state.subscriptionUpdates,
    notifications: state.notifications,
    performance: state.performance,
    getterTraces: state.getterTraces,
    languages: state.languages,
    timestamp: Date.now(),
  };
  const blob = new Blob([JSON.stringify(exportData, null, 2)], { type: 'application/json' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = `ad4m-devtools-export-${Date.now()}.json`;
  a.click();
  URL.revokeObjectURL(url);
}

export function App() {
  const [tab, setTab] = useState<Tab>('connection');
  const [state, setState] = useState<DevToolsState>(EMPTY_STATE);
  const [connected, setConnected] = useState(false);

  const refresh = useCallback(async () => {
    const raw = await evalInPage(
      'window.__AD4M_DEVTOOLS__ ? JSON.stringify(window.__AD4M_DEVTOOLS__.getState()) : null'
    );
    if (raw) {
      const parsed = typeof raw === 'string' ? JSON.parse(raw) : raw;
      setState(parsed);
      setConnected(true);
    } else {
      setConnected(false);
    }
  }, []);

  useEffect(() => {
    refresh();
    const timer = setInterval(refresh, 1000);
    return () => clearInterval(timer);
  }, [refresh]);

  const notifHasErrors = hasNotificationErrors(state.notifications);

  const tabs: { id: Tab; label: string; badge?: boolean }[] = [
    { id: 'connection', label: 'Connection' },
    { id: 'perspectives', label: 'Perspectives' },
    { id: 'queries', label: 'Queries' },
    { id: 'notifications', label: 'Notifications', badge: notifHasErrors },
    { id: 'agent', label: 'Agent' },
  ];

  return (
    <div class="devtools-root">
      <PerformanceBar perf={state.performance} connected={connected} />
      <div class="tab-bar">
        {tabs.map(t => (
          <button
            key={t.id}
            class={`tab-btn ${tab === t.id ? 'active' : ''}`}
            onClick={() => setTab(t.id)}
          >
            {t.label}
            {t.badge && <span class="error-badge">!</span>}
          </button>
        ))}
        <button class="tab-btn export-btn" onClick={() => exportState(state)} title="Export DevTools state as JSON">
          ⬇ Export
        </button>
      </div>
      <div class="tab-content">
        {tab === 'connection' && <ConnectionTab state={state} connected={connected} />}
        {tab === 'perspectives' && <PerspectivesTab />}
        {tab === 'queries' && (
          <QueriesTab
            operations={state.operations}
            subscriptions={state.subscriptions}
            subscriptionUpdates={state.subscriptionUpdates || []}
            getterTraces={state.getterTraces || []}
          />
        )}
        {tab === 'notifications' && <NotificationsTab notifications={state.notifications} />}
        {tab === 'agent' && <AgentTab languages={state.languages || []} />}
      </div>
    </div>
  );
}
