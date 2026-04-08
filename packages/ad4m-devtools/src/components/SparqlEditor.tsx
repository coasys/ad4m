import { useState } from 'preact/hooks';
import { JsonViewer } from './JsonViewer';

interface Props {
  perspectiveUUID: string;
}

export function SparqlEditor({ perspectiveUUID }: Props) {
  const [query, setQuery] = useState('SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 20');
  const [result, setResult] = useState<any>(null);
  const [error, setError] = useState<string | null>(null);
  const [running, setRunning] = useState(false);

  const run = () => {
    setRunning(true);
    setError(null);
    const escaped = query.replace(/\\/g, '\\\\').replace(/'/g, "\\'").replace(/\n/g, '\\n');
    const expr = `
      (async () => {
        const client = window.__AD4M_DEVTOOLS__?._client;
        if (!client) return JSON.stringify({ error: 'No client' });
        const proxy = await client.perspective.byUUID('${perspectiveUUID}');
        if (!proxy) return JSON.stringify({ error: 'Perspective not found' });
        try {
          const r = await proxy.infer('${escaped}');
          return JSON.stringify({ data: r });
        } catch(e) {
          return JSON.stringify({ error: e.message });
        }
      })()
    `;
    if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
      chrome.devtools.inspectedWindow.eval(expr, (res: any, err: any) => {
        setRunning(false);
        if (err) { setError(String(err)); return; }
        try {
          const parsed = JSON.parse(res);
          if (parsed.error) setError(parsed.error);
          else setResult(parsed.data);
        } catch { setError('Failed to parse result'); }
      });
    }
  };

  return (
    <div class="sparql-editor">
      <textarea
        class="sparql-input"
        value={query}
        onInput={(e) => setQuery((e.target as HTMLTextAreaElement).value)}
        rows={6}
        spellcheck={false}
      />
      <button class="btn" onClick={run} disabled={running}>
        {running ? 'Running...' : '▶ Execute'}
      </button>
      {error && <div class="error-msg">{error}</div>}
      {result && <JsonViewer data={result} />}
    </div>
  );
}
