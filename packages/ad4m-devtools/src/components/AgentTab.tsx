import { useState, useEffect } from 'preact/hooks';

export function AgentTab() {
  const [agent, setAgent] = useState<any>(null);
  const [loading, setLoading] = useState(false);

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

  useEffect(() => { load(); }, []);

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
    </div>
  );
}
