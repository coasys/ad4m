import { StatusBadge } from './StatusBadge';

interface Props {
  state: any;
  connected: boolean;
}

export function ConnectionTab({ state, connected }: Props) {
  const conn = state.connection;
  return (
    <div class="tab-panel">
      <h2>Connection Health</h2>
      <div class="info-grid">
        <div class="info-row">
          <span class="info-label">WebSocket</span>
          <StatusBadge ok={connected} label={connected ? 'Connected' : 'Disconnected'} />
        </div>
        <div class="info-row">
          <span class="info-label">Authenticated</span>
          <StatusBadge ok={conn.authenticated} label={conn.authenticated ? 'Yes' : 'No'} />
        </div>
        <div class="info-row">
          <span class="info-label">URL</span>
          <span class="info-value">{conn.url || '(not available)'}</span>
        </div>
        <div class="info-row">
          <span class="info-label">DevTools Bridge</span>
          <StatusBadge ok={connected} label={connected ? 'Active' : 'Not detected'} />
        </div>
      </div>

      <h3>Performance Breakdown</h3>
      <div class="info-grid">
        <div class="info-row">
          <span class="info-label">SPARQL Queries</span>
          <span class="info-value">{state.performance.sparqlQueryCount}</span>
        </div>
        <div class="info-row">
          <span class="info-label">Prolog Queries</span>
          <span class="info-value">{state.performance.prologQueryCount}</span>
        </div>
        <div class="info-row">
          <span class="info-label">Active Subscriptions</span>
          <span class="info-value">{state.performance.activeSubscriptions}</span>
        </div>
        <div class="info-row">
          <span class="info-label">Sub Update Rate</span>
          <span class="info-value">{state.performance.subscriptionUpdateRate}/s</span>
        </div>
      </div>
    </div>
  );
}
