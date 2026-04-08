import { useState } from 'preact/hooks';
import { SparqlEditor } from './SparqlEditor';

export function PerspectivesTab() {
  const [perspectives, setPerspectives] = useState<any[]>([]);
  const [selected, setSelected] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  const loadPerspectives = () => {
    setLoading(true);
    const expr = `
      window.__AD4M_DEVTOOLS__?._client?.perspective?.all()
        .then(ps => JSON.stringify(ps.map(p => ({ uuid: p.uuid, name: p.name, neighbourhood: p.neighbourhood }))))
    `;
    if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
      chrome.devtools.inspectedWindow.eval(expr, (result: any, err: any) => {
        setLoading(false);
        if (result) setPerspectives(JSON.parse(result));
      });
    }
  };

  return (
    <div class="tab-panel">
      <h2>Perspectives</h2>
      <button class="btn" onClick={loadPerspectives} disabled={loading}>
        {loading ? 'Loading...' : 'Load Perspectives'}
      </button>
      
      <div class="perspective-list">
        {perspectives.map(p => (
          <div
            key={p.uuid}
            class={`perspective-item ${selected === p.uuid ? 'selected' : ''}`}
            onClick={() => setSelected(p.uuid)}
          >
            <span class="perspective-name">{p.name || 'Unnamed'}</span>
            <span class="perspective-uuid">{p.uuid.slice(0, 8)}...</span>
            {p.neighbourhood && <span class="badge">Shared</span>}
          </div>
        ))}
      </div>

      {selected && (
        <div class="perspective-detail">
          <h3>SPARQL Query Editor</h3>
          <SparqlEditor perspectiveUUID={selected} />
        </div>
      )}
    </div>
  );
}
