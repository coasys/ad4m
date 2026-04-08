import { useState } from 'preact/hooks';
import { JsonViewer } from './JsonViewer';

interface Props {
  operations: any[];
}

export function QueriesTab({ operations }: Props) {
  const [selected, setSelected] = useState<any>(null);
  const [filter, setFilter] = useState('');

  const filtered = operations
    .filter(op => !filter || op.operationName?.toLowerCase().includes(filter.toLowerCase()))
    .reverse(); // newest first

  return (
    <div class="tab-panel">
      <h2>GraphQL Operations ({operations.length})</h2>
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
            <span class={`op-type op-${op.type}`}>{op.type?.toUpperCase()?.slice(0, 3)}</span>
            <span class="op-name">{op.operationName}</span>
            <span class="op-duration">
              {op.duration != null ? `${op.duration}ms` : '...'}
            </span>
          </div>
        ))}
      </div>
      {selected && (
        <div class="operation-detail">
          <h3>{selected.operationName}</h3>
          <div class="info-grid">
            <div class="info-row"><span class="info-label">Type</span><span>{selected.type}</span></div>
            <div class="info-row"><span class="info-label">Duration</span><span>{selected.duration ?? '-'}ms</span></div>
            <div class="info-row"><span class="info-label">Payload Size</span><span>{selected.payloadSize ?? '-'} bytes</span></div>
          </div>
          {selected.sparqlQuery && (
            <div>
              <h4>SPARQL Query</h4>
              <pre class="code-block">{selected.sparqlQuery}</pre>
            </div>
          )}
          {selected.variables && (
            <div>
              <h4>Variables</h4>
              <JsonViewer data={selected.variables} />
            </div>
          )}
          {selected.response && (
            <div>
              <h4>Response</h4>
              <JsonViewer data={selected.response} />
            </div>
          )}
          {selected.errors?.length > 0 && (
            <div>
              <h4>Errors</h4>
              <JsonViewer data={selected.errors} />
            </div>
          )}
        </div>
      )}
    </div>
  );
}
