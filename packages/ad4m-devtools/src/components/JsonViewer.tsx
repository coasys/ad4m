import { useState } from 'preact/hooks';

interface Props {
  data: any;
  depth?: number;
}

export function JsonViewer({ data, depth = 0 }: Props) {
  const [collapsed, setCollapsed] = useState(depth > 1);

  if (data === null || data === undefined) return <span class="json-null">null</span>;
  if (typeof data === 'string') return <span class="json-string">"{data}"</span>;
  if (typeof data === 'number') return <span class="json-number">{data}</span>;
  if (typeof data === 'boolean') return <span class="json-bool">{String(data)}</span>;

  if (Array.isArray(data)) {
    if (data.length === 0) return <span class="json-bracket">[]</span>;
    return (
      <div class="json-container">
        <span class="json-toggle" onClick={() => setCollapsed(!collapsed)}>
          {collapsed ? '▶' : '▼'} [{data.length}]
        </span>
        {!collapsed && (
          <div class="json-indent">
            {data.map((item, i) => (
              <div key={i}>
                <JsonViewer data={item} depth={depth + 1} />
                {i < data.length - 1 && ','}
              </div>
            ))}
          </div>
        )}
      </div>
    );
  }

  if (typeof data === 'object') {
    const keys = Object.keys(data);
    if (keys.length === 0) return <span class="json-bracket">{'{}'}</span>;
    return (
      <div class="json-container">
        <span class="json-toggle" onClick={() => setCollapsed(!collapsed)}>
          {collapsed ? '▶' : '▼'} {'{'}…{'}'}
        </span>
        {!collapsed && (
          <div class="json-indent">
            {keys.map((key, i) => (
              <div key={key}>
                <span class="json-key">"{key}"</span>: <JsonViewer data={data[key]} depth={depth + 1} />
                {i < keys.length - 1 && ','}
              </div>
            ))}
          </div>
        )}
      </div>
    );
  }

  return <span>{String(data)}</span>;
}
