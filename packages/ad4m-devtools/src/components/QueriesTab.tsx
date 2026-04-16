import { useState } from 'preact/hooks';
import { JsonViewer } from './JsonViewer';

interface Props {
  operations: any[];
  subscriptions: any[];
  subscriptionUpdates: any[];
  getterTraces: any[];
}

type SubTab = 'requests' | 'subscriptions' | 'getters';

function formatTimestamp(value?: number) {
  if (!value) return '-';
  const date = new Date(value);
  const pad = (part: number, size = 2) => String(part).padStart(size, '0');
  return `${pad(date.getHours())}:${pad(date.getMinutes())}:${pad(date.getSeconds())}.${pad(date.getMilliseconds(), 3)}`;
}

function formatDuration(op: any) {
  if (op.duration != null) return `${op.duration}ms`;
  if (op.type === 'trace') return 'trace';
  return '⏳';
}

function getBadge(op: any) {
  if (op.method) {
    return {
      label: op.method,
      className: `op-type op-http op-${String(op.method).toLowerCase()}`,
    };
  }

  if (op.transport === 'sparql') {
    return { label: 'SPQ', className: 'op-type op-trace op-sparql' };
  }

  if (op.transport === 'prolog') {
    return { label: 'PRO', className: 'op-type op-trace op-prolog' };
  }

  if (op.transport === 'sse') {
    return { label: 'SSE', className: 'op-type op-trace op-sse' };
  }

  return {
    label: String(op.type || 'REQ').toUpperCase().slice(0, 3),
    className: 'op-type op-request',
  };
}

function renderStructuredValue(data: any, className = 'code-block') {
  if (typeof data === 'string') {
    return <pre class={className}>{data}</pre>;
  }
  return <JsonViewer data={data} />;
}

export function QueriesTab({ operations, subscriptions, subscriptionUpdates, getterTraces }: Props) {
  const [selected, setSelected] = useState<any>(null);
  const [filter, setFilter] = useState('');
  const [subTab, setSubTab] = useState<SubTab>('requests');

  const filtered = operations
    .filter(op => {
      if (!filter) return true;
      const haystack = [
        op.operationName,
        op.method,
        op.path,
        op.url,
        op.transport,
        op.queryLanguage,
      ].filter(Boolean).join(' ').toLowerCase();
      return haystack.includes(filter.toLowerCase());
    })
    .sort((a, b) => (b.startTime || 0) - (a.startTime || 0));

  const selectedBadge = selected ? getBadge(selected) : null;

  return (
    <div class="tab-panel">
      <div class="sub-tab-bar">
        <button class={`sub-tab-btn ${subTab === 'requests' ? 'active' : ''}`} onClick={() => setSubTab('requests')}>
          Requests & Traces ({operations.length})
        </button>
        <button class={`sub-tab-btn ${subTab === 'subscriptions' ? 'active' : ''}`} onClick={() => setSubTab('subscriptions')}>
          Subscriptions ({subscriptions.filter((s: any) => s.active).length})
        </button>
        <button class={`sub-tab-btn ${subTab === 'getters' ? 'active' : ''}`} onClick={() => setSubTab('getters')}>
          Getters ({getterTraces.length})
        </button>
      </div>

      {subTab === 'requests' && (
        <div class="queries-split">
          <div class="queries-left">
            <input
              class="filter-input"
              placeholder="Filter by method, endpoint, transport, or label..."
              value={filter}
              onInput={(e) => setFilter((e.target as HTMLInputElement).value)}
            />
            <div class="operations-list">
              {filtered.slice(0, 100).map(op => {
                const badge = getBadge(op);
                return (
                  <div
                    key={op.id}
                    class={`operation-item ${selected?.id === op.id ? 'selected' : ''} ${op.errors?.length ? 'has-error' : ''}`}
                    onClick={() => setSelected(op)}
                  >
                    <span class="op-time">{formatTimestamp(op.startTime)}</span>
                    <span class={badge.className}>{badge.label}</span>
                    <span class="op-name">{op.operationName}</span>
                    <span class="op-duration">{formatDuration(op)}</span>
                    {op.errors?.length > 0 && <span class="op-error-badge">❌</span>}
                  </div>
                );
              })}
            </div>
          </div>
          <div class="queries-right">
            {selected ? (
              <div class="operation-detail">
                <div class="request-detail-header">
                  <h3>{selected.operationName}</h3>
                  {selectedBadge && <span class={selectedBadge.className}>{selectedBadge.label}</span>}
                </div>
                <div class="info-grid">
                  <div class="info-row"><span class="info-label">Timestamp</span><span>{formatTimestamp(selected.startTime)}</span></div>
                  <div class="info-row"><span class="info-label">Type</span><span>{selected.type || '-'}</span></div>
                  <div class="info-row"><span class="info-label">Transport</span><span>{selected.transport || '-'}</span></div>
                  <div class="info-row"><span class="info-label">Method</span><span>{selected.method || '-'}</span></div>
                  <div class="info-row"><span class="info-label">Endpoint</span><span class="mono">{selected.path || selected.url || '-'}</span></div>
                  <div class="info-row"><span class="info-label">Status</span><span>{selected.statusCode ?? '-'}</span></div>
                  <div class="info-row"><span class="info-label">Duration</span><span>{selected.duration != null ? `${selected.duration}ms` : '-'}</span></div>
                  <div class="info-row"><span class="info-label">Payload Size</span><span>{selected.payloadSize ?? '-'} bytes</span></div>
                  {selected.queryLanguage && (
                    <div class="info-row"><span class="info-label">Query Language</span><span>{selected.queryLanguage}</span></div>
                  )}
                </div>

                {(selected.method || selected.path || selected.url) && (
                  <div>
                    <h4>REST Request</h4>
                    <pre class="code-block request-line">{[selected.method || 'REQUEST', selected.path || selected.url || ''].filter(Boolean).join(' ')}</pre>
                  </div>
                )}

                {selected.requestBody !== undefined && (
                  <div>
                    <h4>Request Body</h4>
                    {renderStructuredValue(selected.requestBody)}
                  </div>
                )}

                {selected.sparqlQuery && (
                  <div>
                    <h4>SPARQL Query</h4>
                    <pre class="code-block sparql-highlight">{selected.sparqlQuery}</pre>
                  </div>
                )}

                {selected.query && !selected.sparqlQuery && (
                  <div>
                    <h4>Query Text</h4>
                    <pre class="code-block">{selected.query}</pre>
                  </div>
                )}

                {selected.requestHeaders && Object.keys(selected.requestHeaders).length > 0 && (
                  <details>
                    <summary>Request Headers</summary>
                    <JsonViewer data={selected.requestHeaders} />
                  </details>
                )}

                {selected.response !== undefined && (
                  <div>
                    <h4>Response</h4>
                    {renderStructuredValue(selected.response)}
                  </div>
                )}

                {selected.responseHeaders && Object.keys(selected.responseHeaders).length > 0 && (
                  <details>
                    <summary>Response Headers</summary>
                    <JsonViewer data={selected.responseHeaders} />
                  </details>
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
            ) : (
              <div class="empty">Select a request or trace to view details</div>
            )}
          </div>
        </div>
      )}

      {subTab === 'subscriptions' && (
        <div class="subscriptions-panel">
          {subscriptions.length === 0 && <p class="empty">No live subscriptions tracked</p>}
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
