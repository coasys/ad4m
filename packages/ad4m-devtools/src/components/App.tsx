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
  notifications: any[];
  performance: any;
  connection: any;
}

const EMPTY_STATE: DevToolsState = {
  operations: [],
  subscriptions: [],
  notifications: [],
  performance: {
    totalQueries: 0, totalErrors: 0, avgRTT: 0, peakRTT: 0,
    queriesPerSecond: 0, sparqlQueryCount: 0, prologQueryCount: 0,
    activeSubscriptions: 0, subscriptionUpdateRate: 0, wsMessageRate: 0,
    estimatedMemory: 0,
  },
  connection: { wsConnected: false, url: '', authenticated: false },
};

function evalInPage(expr: string): Promise<any> {
  return new Promise((resolve) => {
    if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
      chrome.devtools.inspectedWindow.eval(expr, (result: any, err: any) => {
        if (err) resolve(null);
        else resolve(result);
      });
    } else {
      // Dev mode — direct access
      try {
        resolve(eval(expr));
      } catch {
        resolve(null);
      }
    }
  });
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

  const tabs: { id: Tab; label: string }[] = [
    { id: 'connection', label: 'Connection' },
    { id: 'perspectives', label: 'Perspectives' },
    { id: 'queries', label: 'Queries' },
    { id: 'notifications', label: 'Notifications' },
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
          </button>
        ))}
      </div>
      <div class="tab-content">
        {tab === 'connection' && <ConnectionTab state={state} connected={connected} />}
        {tab === 'perspectives' && <PerspectivesTab />}
        {tab === 'queries' && <QueriesTab operations={state.operations} />}
        {tab === 'notifications' && <NotificationsTab notifications={state.notifications} />}
        {tab === 'agent' && <AgentTab />}
      </div>
    </div>
  );
}
