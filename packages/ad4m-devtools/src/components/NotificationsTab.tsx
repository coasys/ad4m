import { useState } from 'preact/hooks';
import { JsonViewer } from './JsonViewer';

interface Props {
  notifications: any[];
}

const SURREAL_PATTERNS = ['FROM link', 'fn::', 'in.uri', 'out.uri'];

function detectQueryErrors(triggerQuery: string): string | null {
  if (!triggerQuery) return null;
  for (const p of SURREAL_PATTERNS) {
    if (triggerQuery.includes(p)) {
      return `Contains SurrealDB syntax: "${p}" — this will not work as SPARQL`;
    }
  }
  return null;
}

export function NotificationsTab({ notifications }: Props) {
  const [testResults, setTestResults] = useState<Record<string, any>>({});
  const [testing, setTesting] = useState<Record<string, boolean>>({});

  const testTrigger = (notifId: string) => {
    setTesting(prev => ({ ...prev, [notifId]: true }));
    const expr = `
      (async () => {
        const dt = window.__AD4M_DEVTOOLS__;
        if (!dt) return JSON.stringify({ error: 'DevTools not available' });
        // Get first perspective as default
        const client = dt._client;
        const perspectives = await client.perspective.all();
        const perspId = perspectives?.[0]?.uuid || '';
        const result = await dt.testNotificationTrigger('${notifId}', perspId);
        return JSON.stringify(result);
      })()
    `;
    if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
      chrome.devtools.inspectedWindow.eval(expr, (res: any, err: any) => {
        setTesting(prev => ({ ...prev, [notifId]: false }));
        if (res) {
          try { setTestResults(prev => ({ ...prev, [notifId]: JSON.parse(res) })); }
          catch { setTestResults(prev => ({ ...prev, [notifId]: { error: 'Parse error' } })); }
        }
      });
    }
  };

  return (
    <div class="tab-panel">
      <h2>Notifications ({notifications.length})</h2>
      {notifications.length === 0 && <p class="empty">No notifications registered</p>}
      {notifications.map(n => {
        const queryError = detectQueryErrors(n.triggerQuery);
        return (
          <div key={n.id} class={`notification-item ${queryError ? 'has-query-error' : ''}`}>
            <div class="notification-header">
              <span class="notification-id">{n.id}</span>
              {queryError && <span class="error-badge" title={queryError}>⚠</span>}
              <span class="notification-registered">
                Registered: {new Date(n.registered).toLocaleTimeString()}
              </span>
            </div>
            {queryError && <div class="query-error-banner">{queryError}</div>}
            <div class="notification-query">
              <h4>Trigger Query</h4>
              <pre class={`code-block ${queryError ? 'code-error' : ''}`}>{n.triggerQuery}</pre>
            </div>
            <div class="notification-actions">
              <button
                class="btn btn-sm"
                onClick={() => testTrigger(n.id)}
                disabled={testing[n.id]}
              >
                {testing[n.id] ? 'Testing...' : '▶ Test Trigger'}
              </button>
            </div>
            {testResults[n.id] && (
              <div class="test-result">
                <h4>Test Result</h4>
                {testResults[n.id].error ? (
                  <div class="error-msg">{testResults[n.id].error}</div>
                ) : (
                  <JsonViewer data={testResults[n.id]} />
                )}
              </div>
            )}
            {n.lastError && <div class="error-msg">{n.lastError}</div>}
            {n.lastResult && (
              <div><h4>Last Result</h4><JsonViewer data={n.lastResult} /></div>
            )}
            <div class="match-history">
              <span>Matches: {n.matchHistory?.filter((m: any) => m.matched).length || 0}</span>
              <span> / Total checks: {n.matchHistory?.length || 0}</span>
            </div>
          </div>
        );
      })}
    </div>
  );
}
