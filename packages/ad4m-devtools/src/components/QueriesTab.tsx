import { useState } from 'preact/hooks';
import { JsonViewer } from './JsonViewer';

interface Props {
  operations: any[];
  subscriptions: any[];
  subscriptionUpdates: any[];
  getterTraces: any[];
}

type SubTab = 'operations' | 'subscriptions' | 'getters';

export function QueriesTab({ operations, subscriptions, subscriptionUpdates, getterTraces }: Props) {
  const [selected, setSelected] = useState<any>(null);
  const [filter, setFilter] = useState('');
  const [subTab, setSubTab] = useState<SubTab>('operations');

  const filtered = operations
    .filter(op => !filter || op.operationName?.toLowerCase().includes(filter.toLowerCase()))
    .sort((a, b) => (b.startTime || 0) - (a.startTime || 0));

  return (
    <div class="tab-panel">
      <div class="sub-tab-bar">
        <button class={`sub-tab-btn ${subTab === 'operations' ? 'active' : ''}`} onClick={() => setSubTab('operations')}>
          Operations ({operations.length})
        </button>
        <button class={`sub-tab-btn ${subTab === 'subscriptions' ? 'active' : ''}`} onClick={() => setSubTab('subscriptions')}>
          Subscriptions ({subscriptions.filter((s: any) => s.active).length})
        </button>
        <button class={`sub-tab-btn ${subTab === 'getters' ? 'active' : ''}`} onClick={() => setSubTab('getters')}>
          Getters ({getterTraces.length})
        </button>
      </div>

      {subTab === 'operations' && (
        <>
          <input
            class="filter-input"
            placeholder="Filter by operation name..."
            value={filter}
            onInput={(e) => setFilter((e.target as HTMLInputElement).value)}
          />
          <div class="operations-list">
            {filtered.slice(0, 100).map(op => (
              <div
                key={op.id}
                class={`operation-item ${selected?.id === op.id ? 'selected' : ''} ${op.errors?.length ? 'has-error' : ''}`}
                onClick={() => setSelected(op)}
              >
                <span class="op-time">{new Date(op.startTime).toLocaleTimeString('en-GB', {hour12: false, hour: '2-digit', minute: '2-digit', second: '2-digit', fractionalSecondDigits: 3})}</span>
                <span class={`op-type op-${op.type}`}>{op.type?.toUpperCase()?.slice(0, 3)}</span>
                <span class="op-name">{op.operationName}</span>
                <span class="op-duration">{op.duration != null ? `${op.duration}ms` : '⏳'}</span>
                {op.errors?.length > 0 && <span class="op-error-badge">❌</span>}
              </div>
            ))}
          </div>
          {selected && (
            <div class="operation-detail">
              <h3>{selected.operationName}</h3>
              <div class="info-grid">
                <div class="info-row"><span class="info-label">Timestamp</span><span>{new Date(selected.startTime).toLocaleTimeString('en-GB', {hour12: false, hour: '2-digit', minute: '2-digit', second: '2-digit', fractionalSecondDigits: 3})}</span></div>
                <div class="info-row"><span class="info-label">Type</span><span>{selected.type}</span></div>
                <div class="info-row"><span class="info-label">Duration</span><span>{selected.duration ?? '-'}ms</span></div>
                <div class="info-row"><span class="info-label">Payload Size</span><span>{selected.payloadSize ?? '-'} bytes</span></div>
              </div>
              {selected.query && (
                <div>
                  <h4>GraphQL Query</h4>
                  <pre class="code-block">{selected.query}</pre>
                </div>
              )}
              {selected.sparqlQuery && (
                <div>
                  <h4>SPARQL Query</h4>
                  <pre class="code-block sparql-highlight">{selected.sparqlQuery}</pre>
                </div>
              )}
              {selected.variables && (
                <div><h4>Variables</h4><JsonViewer data={selected.variables} /></div>
              )}
              {selected.response && (
                <div><h4>Response</h4><JsonViewer data={selected.response} /></div>
              )}
              {selected.stackTrace && (
                <div>
                  <h4>Call Stack</h4>
                  <pre class="code-block stack-trace">{selected.stackTrace}</pre>
                </div>
              )}
              {selected.errors?.length > 0 && (
                <div class="error-detail-panel">
                  <h4>Errors</h4>
                  {selected.errors.map((err: any, i: number) => (
                    <div key={i} class="error-item">
                      <div class="error-type">{err.type || 'Error'}</div>
                      <div class="error-message">{err.message || String(err)}</div>
                      {err.stack && (
                        <details>
                          <summary>Stack Trace</summary>
                          <pre class="code-block stack-trace">{err.stack}</pre>
                        </details>
                      )}
                      {err.nested?.map((ne: any, j: number) => (
                        <div key={j} class="nested-error">
                          <div class="error-type">↳ {ne.type}</div>
                          <div class="error-message">{ne.message}</div>
                        </div>
                      ))}
                    </div>
                  ))}
                </div>
              )}
            </div>
          )}
        </>
      )}

      {subTab === 'subscriptions' && (
        <div class="subscriptions-panel">
          {subscriptions.length === 0 && <p class="empty">No subscriptions tracked</p>}
          {subscriptions.map((sub: any) => (
            <div key={sub.id} class={`subscription-item ${sub.active ? '' : 'inactive'}`}>
              <div class="subscription-header">
                <span class={`status-dot ${sub.active ? 'active' : 'inactive'}`} />
                <span class="subscription-model">{sub.modelName || 'Unknown'}</span>
                <span class="subscription-updates">{sub.updateCount} updates</span>
              </div>
              <div class="info-grid">
                <div class="info-row"><span class="info-label">Perspective</span><span class="mono">{sub.perspectiveUUID?.slice(0, 8)}...</span></div>
                <div class="info-row"><span class="info-label">FP Hits/Misses</span><span>{sub.fingerprintHits}/{sub.fingerprintMisses}</span></div>
                <div class="info-row"><span class="info-label">Last Update</span><span>{sub.lastUpdateTimestamp ? new Date(sub.lastUpdateTimestamp).toLocaleTimeString() : '-'}</span></div>
              </div>
              {sub.query && <pre class="code-block">{sub.query}</pre>}
              {sub.stackTrace && (
                <details>
                  <summary>Call Stack</summary>
                  <pre class="code-block stack-trace">{sub.stackTrace}</pre>
                </details>
              )}
              {/* Show recent updates for this subscription */}
              {(() => {
                const updates = subscriptionUpdates.filter((u: any) => u.subscriptionId === sub.id).slice(-5).reverse();
                if (updates.length === 0) return null;
                return (
                  <div class="sub-updates">
                    <h5>Recent Updates</h5>
                    {updates.map((u: any, i: number) => (
                      <div key={i} class="sub-update-item">
                        <span>{new Date(u.timestamp).toLocaleTimeString()}</span>
                        <span>raw: {u.rawResultCount}</span>
                        <span>processed: {u.processedCount}</span>
                        <span class={u.fingerprintChanged ? 'fp-changed' : 'fp-same'}>
                          {u.fingerprintChanged ? '⚡ changed' : '= same'}
                        </span>
                      </div>
                    ))}
                  </div>
                );
              })()}
            </div>
          ))}
        </div>
      )}

      {subTab === 'getters' && (
        <div class="getters-panel">
          {getterTraces.length === 0 && <p class="empty">No getter evaluations traced</p>}
          {getterTraces.slice().reverse().slice(0, 100).map((g: any) => (
            <div key={g.id} class={`getter-item ${g.error ? 'has-error' : ''}`}>
              <div class="getter-header">
                <span class="getter-property">{g.property}</span>
                <span class={`getter-type getter-${g.getterType}`}>{g.getterType}</span>
                <span class="getter-duration">{g.duration}ms</span>
              </div>
              <div class="info-row">
                <span class="info-label">Instance</span>
                <span class="mono">{g.instanceId}</span>
              </div>
              {g.query && <pre class="code-block">{g.query}</pre>}
              {g.error && <div class="error-msg">{g.error}</div>}
              {g.result !== undefined && g.result !== null && (
                <details>
                  <summary>Result</summary>
                  <JsonViewer data={g.result} />
                </details>
              )}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
