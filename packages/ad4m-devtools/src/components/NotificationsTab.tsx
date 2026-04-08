import { JsonViewer } from './JsonViewer';

interface Props {
  notifications: any[];
}

export function NotificationsTab({ notifications }: Props) {
  return (
    <div class="tab-panel">
      <h2>Notifications ({notifications.length})</h2>
      {notifications.length === 0 && <p class="empty">No notifications registered</p>}
      {notifications.map(n => (
        <div key={n.id} class="notification-item">
          <div class="notification-header">
            <span class="notification-id">{n.id}</span>
            <span class="notification-registered">
              Registered: {new Date(n.registered).toLocaleTimeString()}
            </span>
          </div>
          <div class="notification-query">
            <h4>Trigger Query</h4>
            <pre class="code-block">{n.triggerQuery}</pre>
          </div>
          {n.lastError && <div class="error-msg">{n.lastError}</div>}
          {n.lastResult && (
            <div>
              <h4>Last Result</h4>
              <JsonViewer data={n.lastResult} />
            </div>
          )}
          <div class="match-history">
            <span>Matches: {n.matchHistory?.filter((m: any) => m.matched).length || 0}</span>
            <span> / Total checks: {n.matchHistory?.length || 0}</span>
          </div>
        </div>
      ))}
    </div>
  );
}
