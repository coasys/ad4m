import { useState } from 'preact/hooks';
import { SparqlEditor } from './SparqlEditor';
import { JsonViewer } from './JsonViewer';

type DetailTab = 'sparql' | 'links' | 'subjectClasses';

export function PerspectivesTab() {
  const [perspectives, setPerspectives] = useState<any[]>([]);
  const [selected, setSelected] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [detailTab, setDetailTab] = useState<DetailTab>('sparql');

  // Links state
  const [links, setLinks] = useState<any[]>([]);
  const [linksLoading, setLinksLoading] = useState(false);
  const [linkFilter, setLinkFilter] = useState({ source: '', predicate: '', target: '' });
  const [linksPage, setLinksPage] = useState(0);
  const LINKS_PER_PAGE = 50;

  // Subject classes state
  const [subjectClasses, setSubjectClasses] = useState<any[]>([]);
  const [scLoading, setScLoading] = useState(false);

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

  const loadLinks = () => {
    if (!selected) return;
    setLinksLoading(true);
    const filterObj: any = {};
    if (linkFilter.source) filterObj.source = linkFilter.source;
    if (linkFilter.predicate) filterObj.predicate = linkFilter.predicate;
    if (linkFilter.target) filterObj.target = linkFilter.target;
    const filterStr = JSON.stringify(filterObj).replace(/'/g, "\\'");
    const expr = `
      (async () => {
        const dt = window.__AD4M_DEVTOOLS__;
        if (!dt?.queryLinks) return '[]';
        const links = await dt.queryLinks('${selected}', ${filterStr});
        return JSON.stringify(links || []);
      })()
    `;
    if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
      chrome.devtools.inspectedWindow.eval(expr, (res: any, err: any) => {
        setLinksLoading(false);
        if (res) { try { setLinks(JSON.parse(res)); setLinksPage(0); } catch {} }
      });
    }
  };

  const loadSubjectClasses = () => {
    if (!selected) return;
    setScLoading(true);
    const expr = `
      (async () => {
        const dt = window.__AD4M_DEVTOOLS__;
        if (!dt?.getSubjectClasses) return '[]';
        const sc = await dt.getSubjectClasses('${selected}');
        return JSON.stringify(sc || []);
      })()
    `;
    if (typeof chrome !== 'undefined' && chrome.devtools?.inspectedWindow) {
      chrome.devtools.inspectedWindow.eval(expr, (res: any, err: any) => {
        setScLoading(false);
        if (res) { try { setSubjectClasses(JSON.parse(res)); } catch {} }
      });
    }
  };

  const pagedLinks = links.slice(linksPage * LINKS_PER_PAGE, (linksPage + 1) * LINKS_PER_PAGE);
  const totalPages = Math.ceil(links.length / LINKS_PER_PAGE);

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
            onClick={() => { setSelected(p.uuid); setLinks([]); setSubjectClasses([]); }}
          >
            <span class="perspective-name">{p.name || 'Unnamed'}</span>
            <span class="perspective-uuid">{p.uuid.slice(0, 8)}...</span>
            {p.neighbourhood && <span class="badge">Shared</span>}
          </div>
        ))}
      </div>

      {selected && (
        <div class="perspective-detail">
          <div class="sub-tab-bar">
            <button class={`sub-tab-btn ${detailTab === 'sparql' ? 'active' : ''}`} onClick={() => setDetailTab('sparql')}>SPARQL</button>
            <button class={`sub-tab-btn ${detailTab === 'links' ? 'active' : ''}`} onClick={() => setDetailTab('links')}>Links ({links.length})</button>
            <button class={`sub-tab-btn ${detailTab === 'subjectClasses' ? 'active' : ''}`} onClick={() => setDetailTab('subjectClasses')}>Subject Classes</button>
          </div>

          {detailTab === 'sparql' && <SparqlEditor perspectiveUUID={selected} />}

          {detailTab === 'links' && (
            <div class="links-panel">
              <div class="link-filters">
                <input class="filter-input" placeholder="Source..." value={linkFilter.source}
                  onInput={(e) => setLinkFilter(f => ({ ...f, source: (e.target as HTMLInputElement).value }))} />
                <input class="filter-input" placeholder="Predicate..." value={linkFilter.predicate}
                  onInput={(e) => setLinkFilter(f => ({ ...f, predicate: (e.target as HTMLInputElement).value }))} />
                <input class="filter-input" placeholder="Target..." value={linkFilter.target}
                  onInput={(e) => setLinkFilter(f => ({ ...f, target: (e.target as HTMLInputElement).value }))} />
                <button class="btn" onClick={loadLinks} disabled={linksLoading}>
                  {linksLoading ? 'Loading...' : 'Query Links'}
                </button>
              </div>
              {links.length > 0 && (
                <>
                  <table class="links-table">
                    <thead>
                      <tr><th>Source</th><th>Predicate</th><th>Target</th><th>Author</th><th>Timestamp</th></tr>
                    </thead>
                    <tbody>
                      {pagedLinks.map((link: any, i: number) => (
                        <tr key={i}>
                          <td class="mono" title={link.data?.source}>{(link.data?.source || '').slice(0, 30)}</td>
                          <td class="mono" title={link.data?.predicate}>{(link.data?.predicate || '').slice(0, 30)}</td>
                          <td class="mono" title={link.data?.target}>{(link.data?.target || '').slice(0, 30)}</td>
                          <td class="mono">{(link.author || '').slice(0, 15)}...</td>
                          <td>{link.timestamp ? new Date(link.timestamp).toLocaleTimeString() : '-'}</td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                  {totalPages > 1 && (
                    <div class="pagination">
                      <button class="btn btn-sm" disabled={linksPage === 0} onClick={() => setLinksPage(p => p - 1)}>← Prev</button>
                      <span>Page {linksPage + 1} / {totalPages}</span>
                      <button class="btn btn-sm" disabled={linksPage >= totalPages - 1} onClick={() => setLinksPage(p => p + 1)}>Next →</button>
                    </div>
                  )}
                </>
              )}
              {links.length === 0 && !linksLoading && <p class="empty">Click "Query Links" to browse links</p>}
            </div>
          )}

          {detailTab === 'subjectClasses' && (
            <div class="subject-classes-panel">
              <button class="btn" onClick={loadSubjectClasses} disabled={scLoading}>
                {scLoading ? 'Loading...' : 'Load Subject Classes'}
              </button>
              {subjectClasses.length === 0 && !scLoading && <p class="empty">No subject classes found</p>}
              {subjectClasses.map((sc: any, i: number) => (
                <div key={i} class="subject-class-item">
                  <div class="sc-header">
                    <span class="sc-name">{sc.name || sc.class || JSON.stringify(sc).slice(0, 50)}</span>
                  </div>
                  {(sc.properties || sc.shape) ? (
                    <JsonViewer data={sc.properties || sc.shape || sc} />
                  ) : (
                    <JsonViewer data={sc} />
                  )}
                </div>
              ))}
            </div>
          )}
        </div>
      )}
    </div>
  );
}
