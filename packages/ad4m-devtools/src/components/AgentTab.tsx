import { useState, useEffect } from 'preact/hooks';

interface Props {
  languages: any[];
}

export function AgentTab({ languages }: Props) {
  const [agent, setAgent] = useState<any>(null);
  const [loading, setLoading] = useState(false);
  const [liveLanguages, setLiveLanguages] = useState<any[]>([]);
  const [langLoading, setLangLoading] = useState(false);

  const load = () => {
    setLoading(true);
    const expr = `
      window.__AD4M_DEVTOOLS__?._client?.agent?.status()
        .then(s => JSON.stringify(s))
    `;
    if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
      chrome.devtools.inspectedWindow.eval(expr, (res: any, err: any) => {
        setLoading(false);
        if (res) setAgent(JSON.parse(res));
      });
    }
  };

  const loadLanguages = () => {
    setLangLoading(true);
    const expr = `
      (async () => {
        const dt = window.__AD4M_DEVTOOLS__;
        if (!dt?.getLanguages) return null;
        const langs = await dt.getLanguages();
        return JSON.stringify(langs);
      })()
    `;
    if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
      chrome.devtools.inspectedWindow.eval(expr, (res: any, err: any) => {
        setLangLoading(false);
        if (res) {
          try { setLiveLanguages(JSON.parse(res)); } catch {}
        }
      });
    }
  };

  useEffect(() => { load(); }, []);

  // Merge bridge-tracked languages with live-fetched ones
  const allLanguages = liveLanguages.length > 0 ? liveLanguages : languages;

  return (
    <div class="tab-panel">
      <h2>Agent</h2>
      <button class="btn" onClick={load} disabled={loading}>
        {loading ? 'Loading...' : 'Refresh'}
      </button>
      {agent ? (
        <div class="info-grid">
          <div class="info-row">
            <span class="info-label">DID</span>
            <span class="info-value mono">{agent.did || 'Unknown'}</span>
          </div>
          <div class="info-row">
            <span class="info-label">Is Initialized</span>
            <span class="info-value">{agent.isInitialized ? 'Yes' : 'No'}</span>
          </div>
          <div class="info-row">
            <span class="info-label">Is Unlocked</span>
            <span class="info-value">{agent.isUnlocked ? 'Yes' : 'No'}</span>
          </div>
        </div>
      ) : (
        <p class="empty">No agent data available</p>
      )}

      <h2>Languages</h2>
      <button class="btn" onClick={loadLanguages} disabled={langLoading}>
        {langLoading ? 'Loading...' : 'Load Languages'}
      </button>
      {allLanguages.length === 0 ? (
        <p class="empty">No languages loaded. Click "Load Languages" to fetch from runtime.</p>
      ) : (
        <div class="languages-list">
          {allLanguages.map((lang: any, i: number) => (
            <div key={lang.address || i} class={`language-item ${lang.loadStatus === 'error' ? 'has-error' : ''}`}>
              <div class="language-header">
                <span class="language-name">{lang.name || 'Unnamed'}</span>
                <span class={`language-status status-${lang.loadStatus || 'loaded'}`}>
                  {lang.loadStatus || 'loaded'}
                </span>
              </div>
              <div class="info-row">
                <span class="info-label">Address</span>
                <span class="info-value mono">{(lang.address || '').slice(0, 20)}...</span>
              </div>
              {lang.loadTime != null && (
                <div class="info-row">
                  <span class="info-label">Load Time</span>
                  <span class="info-value">{lang.loadTime}ms</span>
                </div>
              )}
              {lang.error && <div class="error-msg">{lang.error}</div>}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
