import { useContext, useEffect, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import { instance } from "@viz-js/viz";

interface DebugStringEntry {
  languageAddress: string;
  debugString: string;
  operation: string;
  timestamp: string;
}

interface DebugStringsProps {
  languageAddress: string;
  onClose: () => void;
  open: boolean;
}

const DebugStrings = ({ languageAddress, onClose, open }: DebugStringsProps) => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [debugStrings, setDebugStrings] = useState<DebugStringEntry[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const fetchDebugStrings = async () => {
    if (!client) return;
    
    setLoading(true);
    setError(null);
    
    try {
      const strings = await client.runtime.debugStrings(languageAddress);
      // Sort by timestamp, latest first
      const sortedStrings = strings.sort((a, b) => 
        new Date(b.timestamp).getTime() - new Date(a.timestamp).getTime()
      );
      setDebugStrings(sortedStrings);
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to fetch debug strings");
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    if (open) {
      fetchDebugStrings();
    }
  }, [languageAddress, client, open]);

  const formatTimestamp = (timestamp: string) => {
    return new Date(timestamp).toLocaleString();
  };

  const isDotGraph = (debugString: string) => {
    return debugString.trim().startsWith('digraph') && 
           debugString.includes('{') && 
           debugString.includes('}');
  };

  const renderDotGraph = async (dotString: string) => {
    try {
      const viz = await instance();
      const svg = viz.renderSVGElement(dotString);
      return svg;
    } catch (err) {
      console.error("Failed to render DOT graph:", err);
      return null;
    }
  };

  const DotGraphRenderer = ({ dotString }: { dotString: string }) => {
    const [svgElement, setSvgElement] = useState<SVGElement | null>(null);
    const [renderError, setRenderError] = useState(false);

    useEffect(() => {
      renderDotGraph(dotString).then(svg => {
        if (svg) {
          setSvgElement(svg);
        } else {
          setRenderError(true);
        }
      });
    }, [dotString]);

    if (renderError) {
      return (
        <div style={{ padding: "10px", backgroundColor: "#f5f5f5", borderRadius: "4px" }}>
          <j-text size="300" color="danger">Failed to render graph</j-text>
          <pre style={{ fontSize: "12px", marginTop: "8px", overflow: "auto" }}>
            {dotString}
          </pre>
        </div>
      );
    }

    if (!svgElement) {
      return <j-text size="300">Rendering graph...</j-text>;
    }

    return (
      <div 
        style={{ 
          padding: "10px", 
          backgroundColor: "#f9f9f9", 
          borderRadius: "4px",
          overflow: "auto",
          maxHeight: "300px"
        }}
        dangerouslySetInnerHTML={{ __html: svgElement.outerHTML }}
      />
    );
  };

  if (!open) return null;

  return (
    <j-modal size="fullscreen" open={open}>
      <j-box px="400" py="600">
        <j-box pb="400">
          <j-flex j="between" a="center">
            <j-text nomargin size="600" color="black" weight="600">
              Debug Strings for Language
            </j-text>
            <j-button variant="ghost" size="sm" onClick={fetchDebugStrings} loading={loading}>
              <j-icon name="refresh" size="sm"></j-icon>
              Refresh
            </j-button>
          </j-flex>
        </j-box>

        <j-box pb="300">
          <j-text size="400" color="ui-500">
            Language Address: {languageAddress}
          </j-text>
        </j-box>

        {error && (
          <j-box pb="400">
            <j-text color="danger">{error}</j-text>
          </j-box>
        )}

        {debugStrings.length === 0 && !loading && !error && (
          <j-box py="600" style={{ textAlign: "center" }}>
            <j-text size="400" color="ui-500">
              No debug strings found for this language.
            </j-text>
            <j-box pt="200">
              <j-text size="300" color="ui-400">
                Languages may or may not generate debug strings.
                If this Language is a LinkLanguage, try using it in a neighborhood to generate debug information.
              </j-text>
            </j-box>
          </j-box>
        )}

        <div style={{ 
          display: "flex", 
          flexDirection: "column", 
          gap: "16px",
          maxHeight: "70vh",
          overflowY: "auto"
        }}>
          {debugStrings.map((entry, index) => (
            <div
              key={`${entry.timestamp}-${index}`}
              style={{
                border: "1px solid var(--j-color-ui-200)",
                borderRadius: "var(--j-border-radius)",
                backgroundColor: "rgba(10, 9, 29, 0.8)",
                padding: "var(--j-space-400)",
              }}
            >
              <j-box pb="200">
                <j-flex j="between" a="center">
                  <j-badge size="sm" variant="primary">
                    {entry.operation}
                  </j-badge>
                  <j-text size="300" color="ui-500">
                    {formatTimestamp(entry.timestamp)}
                  </j-text>
                </j-flex>
              </j-box>

              <j-box pt="200">
                {isDotGraph(entry.debugString) ? (
                  <div>
                    <j-box pb="200">
                      <j-text size="300" color="ui-300">
                        Graph Visualization:
                      </j-text>
                    </j-box>
                    <DotGraphRenderer dotString={entry.debugString} />
                    <details style={{ marginTop: "12px" }}>
                      <summary style={{ cursor: "pointer", padding: "4px 0" }}>
                        <j-text size="300" color="ui-400">Show raw DOT syntax</j-text>
                      </summary>
                      <pre style={{
                        fontSize: "12px",
                        backgroundColor: "#1a1a1a",
                        color: "#e0e0e0",
                        padding: "12px",
                        borderRadius: "4px",
                        overflow: "auto",
                        marginTop: "8px"
                      }}>
                        {entry.debugString}
                      </pre>
                    </details>
                  </div>
                ) : (
                  <div>
                    <j-box pb="200">
                      <j-text size="300" color="ui-300">
                        Debug String:
                      </j-text>
                    </j-box>
                    <pre style={{
                      fontSize: "12px",
                      backgroundColor: "#1a1a1a",
                      color: "#e0e0e0",
                      padding: "12px",
                      borderRadius: "4px",
                      overflow: "auto",
                      whiteSpace: "pre-wrap",
                      wordBreak: "break-word"
                    }}>
                      {entry.debugString}
                    </pre>
                  </div>
                )}
              </j-box>
            </div>
          ))}
        </div>

        <j-box pt="400">
          <j-button variant="primary" onClick={onClose}>
            Close
          </j-button>
        </j-box>
      </j-box>
    </j-modal>
  );
};

export default DebugStrings; 