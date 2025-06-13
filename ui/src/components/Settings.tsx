import { Agent, ImportResult, ImportStats, Literal } from "@coasys/ad4m";
import { invoke } from "@tauri-apps/api/core";
import { getCurrentWebviewWindow } from "@tauri-apps/api/webviewWindow";
import { writeText } from "@tauri-apps/plugin-clipboard-manager";
import { openUrl, revealItemInDir } from "@tauri-apps/plugin-opener";
import { join } from '@tauri-apps/api/path';
import { save as dialogSave, open as dialogOpen, message as dialogMessage, type MessageDialogOptions } from "@tauri-apps/plugin-dialog";
import { useCallback, useContext, useEffect, useState } from "react";
import QRCode from "react-qr-code";
import { PREDICATE_FIRSTNAME, PREDICATE_LASTNAME, PREDICATE_USERNAME } from "../constants/triples";
import { Ad4minContext } from "../context/Ad4minContext";
import { AgentContext } from "../context/AgentContext";
import { buildAd4mClient, copyTextToClipboard } from "../util";
import ActionButton from "./ActionButton";
import { cardStyle } from "./styles";

const appWindow = getCurrentWebviewWindow();

type Props = {
  did: String;
  opened: boolean;
  setOpened: (val: boolean) => void;
};

export const fetchProfile = async (agent: Agent) => {
  const tempProfile = {
    firstName: "",
    lastName: "",
    username: "",
  };

  for (const {
    data: { source, predicate, target },
  } of agent.perspective?.links!) {
    if (source === agent.did) {
      if (predicate === PREDICATE_FIRSTNAME) {
        tempProfile.firstName = Literal.fromUrl(target).get();
      } else if (predicate === PREDICATE_LASTNAME) {
        tempProfile.lastName = Literal.fromUrl(target).get();
      } else if (predicate === PREDICATE_USERNAME) {
        tempProfile.username = Literal.fromUrl(target).get();
      }
    }
  }

  return tempProfile;
};

const Profile = (props: Props) => {
  const {
    state: { loading },
    methods: { lockAgent },
  } = useContext(AgentContext);

  const {
    state: { url, did, client, expertMode, isInitialized },
    methods: { toggleExpertMode },
  } = useContext(Ad4minContext);

  const [appState, setAppState] = useState({} as any);

  const [trustedAgents, setTrustedAgents] = useState<any[]>([]);

  const [trustedAgentModalOpen, settrustedAgentModalOpen] = useState(false);

  const [clearAgentModalOpen, setClearAgentModalOpen] = useState(false);
  const [showAgentSelection, setShowAgentSelection] = useState(false);
  const [createAgent, setCreateAgent] = useState(false);
  const [newAgentName, setNewAgentName] = useState("");
  const [file, setFile] = useState<File | null>(null);

  const [proxy, setProxy] = useState("");

  const [qrcodeModal, setQRCodeModal] = useState(false);

  const [copied, setCopied] = useState(false);

  const [password, setPassword] = useState("");

  const [showPassword, setShowPassword] = useState(false);

  const [loadingProxy, setLoadingProxy] = useState(false);

  const [showAddHcAgentInfos, setShowAddHcAgentInfos] = useState(false);

  const [addHcAgentInfos, setAddHcAgentInfos] = useState("");

  const [exportStatus, setExportStatus] = useState("");
  const [importStatus, setImportStatus] = useState("");

  const [networkMetrics, setNetworkMetrics] = useState("");
  const [showNetworkMetrics, setShowNetworkMetrics] = useState(false);

  async function openLogs() {
    let dataPath = await invoke("get_data_path") as string;
    revealItemInDir(await join(dataPath, "ad4m.log"))
  }

  const [profile, setProfile] = useState({
    firstName: "",
    lastName: "",
    username: "",
  });

  const onPasswordChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault();
    let { value } = event.target;
    setPassword(value);
  };

  const getAppState = useCallback(async () => {
    const state = await invoke("get_app_agent_list");
    setAppState(JSON.parse(state));
  }, []);

  const getTrustedAgents = useCallback(async () => {
    if (url) {
      const client = await buildAd4mClient(url, false);
      const trustedAgents = await client!.runtime.getTrustedAgents();

      const tempTempAgents = [];

      for (const agent of trustedAgents) {
        const fetchedAgent = await client!.agent.byDID(agent);

        if (fetchedAgent) {
          const profile = await fetchProfile(fetchedAgent);

          tempTempAgents.push({ did: agent, ...profile });
        } else {
          tempTempAgents.push({ did: agent });
        }
      }

      setTrustedAgents(tempTempAgents);
    }
  }, [url]);

  const fetchCurrentAgentProfile = useCallback(async () => {
    if (url) {
      const client = await buildAd4mClient(url, false);
      const agent = await client!.agent.me();

      const profile = await fetchProfile(agent);

      setProfile(profile);
    }
  }, [url]);

  const getAgentInfo = async () => {
    const info = await client?.runtime.hcAgentInfos();
    alert(info);
  };

  const addAgentInfo = async (info: string) => {
    await client?.runtime.hcAddAgentInfos(info);
    setShowAddHcAgentInfos(false);
  };

  useEffect(() => {
    fetchCurrentAgentProfile();
    getTrustedAgents();
    getAppState();
  }, [fetchCurrentAgentProfile, getTrustedAgents, getAppState]);

  useEffect(() => {
    const getProxy = async () => {
      const proxy: string = await invoke("get_proxy");
      console.log(proxy);
      setProxy(formatProxy(proxy));
    };
    getProxy().catch(console.error);
  }, []);

  // @ts-ignore
  const onKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === "Enter") {
      if (isInitialized) {
        clearAgent(password);
      }
    }
  };

  const formatProxy = (proxy: string) => {
    return proxy.replace(/^https(.*)/, "wss$1").replace(/^http(.*)/, "ws$1") + "/graphql";
  };

  const copyText = (text: string) => {
    copyTextToClipboard(text);
  };

  const showProxyQRCode = () => {
    setQRCodeModal(true);
  };

  const clearAgent = async (password: string) => {
    console.log("clearAgent 0", password);
    let agentStatus = await client?.agent.lock(password);
    console.log("clearAgent 1", agentStatus);
    if (!agentStatus?.isUnlocked) {
      await invoke("clear_state");
    }
  };

  const setupProxy = async (event: any) => {
    try {
      setLoadingProxy(true);
      const proxy: string = await invoke("setup_proxy", { subdomain: did });
      console.log("Finish setup proxy, ", proxy);
      setProxy(formatProxy(proxy));
      event.target.checked = true;
    } catch (e) {
      event.target.checked = false;
      setProxy("");
    } finally {
      setLoadingProxy(false);
    }
  };

  const stopProxy = async (event: any) => {
    try {
      await invoke("stop_proxy");
      setProxy("");
      event.target.checked = false;
    } catch (e) {
      event.target.checked = true;
    }
  };

  const creatAgentFunc = async () => {
    await invoke("add_app_agent_state", {
      agent: {
        name: newAgentName,
        path: `.${newAgentName.toLowerCase()}`,
        bootstrap: file,
      },
    });

    setCreateAgent(false);
    getAppState();
  };

  const handleExport = async () => {
    try {
      setExportStatus("Exporting...");
      const filePath = await dialogSave({
        defaultPath: "ad4m_backup.json",
        filters: [{
          name: 'JSON',
          extensions: ['json']
        }]
      });
      
      if (filePath && client) {
        await client.runtime.exportDb(filePath);
        setExportStatus("Export successful!");
      }
      setTimeout(() => setExportStatus(""), 3000);
    } catch (error) {
      console.error("Export failed:", error);
      setExportStatus("Export failed!");
      setTimeout(() => setExportStatus(""), 3000);
    }
  };

  const handleImport = async () => {
    try {
      setImportStatus("Importing...");
      const filePath = await dialogOpen({
        filters: [{
          name: 'JSON',
          extensions: ['json']
        }]
      });
      
      if (filePath && client) {
        const result = await client.runtime.importDb(filePath.toString());
        
        if (result && typeof result === 'object') {
          const importResult = result as ImportResult;
          
          // Create a summary message
          const summary = [
            `Successfully imported:`,
            `- ${importResult.perspectives.imported} of ${importResult.perspectives.total} perspectives`,
            `- ${importResult.links.imported} of ${importResult.links.total} links`,
            `- ${importResult.expressions.imported} of ${importResult.expressions.total} expressions`,
            `- ${importResult.perspectiveDiffs.imported} of ${importResult.perspectiveDiffs.total} perspective diffs`,
            `- ${importResult.notifications.imported} of ${importResult.notifications.total} notifications`,
            `- ${importResult.models.imported} of ${importResult.models.total} models`,
            `- ${importResult.defaultModels.imported} of ${importResult.defaultModels.total} default models`,
            `- ${importResult.tasks.imported} of ${importResult.tasks.total} tasks`,
            `- ${importResult.friends.imported} of ${importResult.friends.total} friends`,
            `- ${importResult.trustedAgents.imported} of ${importResult.trustedAgents.total} trusted agents`,
            `- ${importResult.knownLinkLanguages.imported} of ${importResult.knownLinkLanguages.total} known link languages`,
          ];

          // Add error summary if there are any errors
          const totalErrors = Object.values(importResult).reduce((sum: number, stats: ImportStats) => {
            return sum + (stats.errors?.length || 0);
          }, 0);

          if (totalErrors > 0) {
            summary.push("\nErrors encountered:");
            Object.entries(importResult).forEach(([category, stats]) => {
              const typedStats = stats as ImportStats;
              if (typedStats.errors && typedStats.errors.length > 0) {
                summary.push(`\n${category}:`);
                typedStats.errors.forEach((error: string) => summary.push(`- ${error}`));
              }
            });
          }

          // Show dialog with results
          const dialogOptions: MessageDialogOptions = {
            title: "Import Results"
          };

          await dialogMessage(summary.join('\n'), dialogOptions);

          setImportStatus("Import completed");
        }
      }
      setTimeout(() => setImportStatus(""), 3000);
    } catch (error) {
      console.error("Import failed:", error);
      alert("Import failed: " + error);
      setImportStatus("Import failed!");
      setTimeout(() => setImportStatus(""), 3000);
    }
  };

  return (
    <div>
      <j-box px="500" my="500">
        <j-toggle
          checked={!!proxy}
          onChange={(e) => {
            e.target.checked ? setupProxy(e) : stopProxy(e);
          }}
        >
          Enable remote access via proxy
        </j-toggle>

        {loadingProxy && <j-spinner size="sm"></j-spinner>}

        {proxy && (
          <j-box pb="500">
            <j-flex a="center">
              <ActionButton title="Proxy URL" onClick={() => copyText(proxy)} icon="clipboard" />
              <ActionButton title="QR Code" onClick={showProxyQRCode} icon="qr-code-scan" />
              <ActionButton
                title="Open GraphQL"
                onClick={() => openUrl(proxy.replace("wss", "https").replace("graphql", "playground"))}
                icon="box-arrow-up-right"
              />
            </j-flex>
          </j-box>
        )}
      </j-box>

      <j-box px="500" my="500">
        <j-button onClick={openLogs} full variant="primary">
          <j-icon size="sm" slot="start" name="clipboard"></j-icon>
          Reveal Log File
        </j-button>
      </j-box>

      <j-box px="500" my="500">
        <j-button onClick={() => setShowAgentSelection(true)} full variant="secondary">
          <j-icon size="sm" slot="start" name="server"></j-icon>
          Switch/create agent
        </j-button>
      </j-box>

      <j-box px="500" my="500">
        <j-toggle full="" checked={expertMode} onChange={(e) => toggleExpertMode()}>
          Advanced mode
        </j-toggle>
      </j-box>

      {expertMode && (
        <div>
          <j-box px="500" my="500">
            <j-button onClick={handleExport} full variant="ghost">
              <j-icon size="sm" slot="start" name="download"></j-icon>
              {exportStatus || "Export Database"}
            </j-button>
          </j-box>
          <j-box px="500" my="500">
            <j-button onClick={handleImport} full variant="ghost">
              <j-icon size="sm" slot="start" name="upload"></j-icon>
              {importStatus || "Import Database"}
            </j-button>
          </j-box>
          <j-box px="500" my="500">
            <j-button onClick={() => setClearAgentModalOpen(true)} full variant="ghost">
              <j-icon size="sm" slot="start" name="trash"></j-icon>
              Delete Agent
            </j-button>
          </j-box>
          <j-box px="500" my="500">
            <j-button
              onClick={() => {
                getAgentInfo();
              }}
              full
              variant="ghost"
            >
              <j-icon size="sm" slot="start" name={!copied ? "clipboard" : "clipboard-check"}></j-icon>
              Show Holochain Agent Info(s)
            </j-button>
          </j-box>

          <j-box px="500" my="500">
            <j-button
              onClick={() => {
                setShowAddHcAgentInfos(true);
              }}
              full
              variant="ghost"
            >
              <j-icon size="sm" slot="start" name="shield-check"></j-icon>
              Add Holochain Agent Info(s)
            </j-button>
          </j-box>
          <j-box px="500" my="500">
            <j-button onClick={() => settrustedAgentModalOpen(true)} full variant="ghost">
              <j-icon size="sm" slot="start" name="shield-check"></j-icon>
              Show trusted agents
            </j-button>
          </j-box>
          <j-box px="500" my="500">
            <j-button onClick={async () => {
              if (client) {
                try {
                  const metrics = await (client.runtime as any).getNetworkMetrics();
                  const formattedMetrics = JSON.stringify(JSON.parse(metrics), null, 2);
                  setNetworkMetrics(formattedMetrics);
                  setShowNetworkMetrics(true);
                } catch (error) {
                  console.error('Failed to get network metrics:', error);
                  alert('Failed to get network metrics. Check console for details.');
                }
              }
            }} full variant="ghost">
              <j-icon size="sm" slot="start" name="graph-up"></j-icon>
              Get Network Metrics
            </j-button>
          </j-box>
          <j-box px="500" my="500">
            <j-button onClick={async () => {
              if (client) {
                try {
                  if (confirm('Are you sure you want to restart Holochain? This will temporarily disconnect you from the network and restart the Holochain conductor.')) {
                    await (client.runtime as any).restartHolochain();
                    alert('Holochain has been restarted successfully!');
                  }
                } catch (error) {
                  console.error('Failed to restart Holochain:', error);
                  alert('Failed to restart Holochain. Check console for details.');
                }
              }
            }} full variant="ghost">
              <j-icon size="sm" slot="start" name="arrow-clockwise"></j-icon>
              Restart Holochain
            </j-button>
          </j-box>
        </div>
      )}

      {showAddHcAgentInfos && (
        <j-modal open={showAddHcAgentInfos} onToggle={(e: any) => setAddHcAgentInfos(e.target.open)}>
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Add Holochain Agents Info
              </j-text>
            </j-box>
            <j-box pb="500">
              <j-input
                placeholder="Encoded Holochain AgentInfo string"
                label="Input another agent's info string here.."
                size="lg"
                required
                onInput={(e: any) => setAddHcAgentInfos(e.target.value)}
              ></j-input>
              <j-box p="400"></j-box>
              <j-button onClick={() => addAgentInfo(addHcAgentInfos)} full loading={loading}>
                Add Agent Info
              </j-button>
            </j-box>
          </j-box>
        </j-modal>
      )}

      {trustedAgentModalOpen && (
        <j-modal
          size="fullscreen"
          open={trustedAgentModalOpen}
          onToggle={(e: any) => settrustedAgentModalOpen(e.target.open)}
        >
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Trusted Agents
              </j-text>
            </j-box>
            {trustedAgents.map((e, i) => (
              <div key={`trusted-agent-${e.did}`} style={{ ...cardStyle, width: "100%" }}>
                <j-flex direction="column" style={{ marginTop: 4 }}>
                  <j-text weight="bold">{e?.username || "No username"}</j-text>
                  <j-flex a="center" j="between">
                    <j-text nomargin variant="body" size="xs">
                      {e?.did.length > 25 ? `${e?.did.substring(0, 25)}...` : e?.did}
                    </j-text>
                    <j-box p="100"></j-box>
                    <j-button size="xs" variant="transparent" onClick={() => copyText(e?.did)}>
                      <j-icon size="xs" slot="end" name="clipboard"></j-icon>
                    </j-button>
                  </j-flex>
                </j-flex>
              </div>
            ))}
          </j-box>
        </j-modal>
      )}
      {qrcodeModal && (
        <j-modal
          size="fullscreen"
          open={qrcodeModal}
          onToggle={(e: any) => setQRCodeModal(e.target.open)}
          title="Proxy QR Code"
        >
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Scan this QR on your phone
              </j-text>
            </j-box>
            <j-box bg="ui-900" px="900" py="600">
              <QRCode value={proxy} />
            </j-box>
          </j-box>
        </j-modal>
      )}

      {showAgentSelection && (
        <j-modal
          size="fullscreen"
          open={showAgentSelection}
          onToggle={(e: any) => setShowAgentSelection(e.target.open)}
        >
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Select agent
              </j-text>
            </j-box>
            <j-text>
              Disclaimer: After changing the agent you will have to restart the launcher for it to start using the new
              agent
            </j-text>
            <j-box p="200"></j-box>
            {appState.agent_list.map((agent: any) => (
              <>
                <j-button
                  full
                  variant={agent.path === appState.selected_agent.path ? "primary" : "secondary"}
                  onClick={() => {
                    invoke("set_selected_agent", { agent });
                    getAppState();
                    setShowAgentSelection(false);
                  }}
                >
                  {agent.name}
                </j-button>
                <j-box p="300"></j-box>
              </>
            ))}
            <j-button full onCLick={() => setCreateAgent(true)} variant="secondary">
              <j-icon name="plus"></j-icon>
              Add new agent
            </j-button>
          </j-box>
        </j-modal>
      )}

      {createAgent && (
        <j-modal open={createAgent} onToggle={(e: any) => setCreateAgent(e.target.open)}>
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Create new agent
              </j-text>
            </j-box>
            <j-input
              placeholder="Agent name"
              label="Name"
              size="lg"
              onInput={(e) => setNewAgentName(e.target.value)}
              required
            ></j-input>
            <j-box p="200"></j-box>
            <j-input
              label="Bootstrap file path (absolute path)"
              size="lg"
              placeholder="ex. /path/to/agent-bootstrap.json"
              onInput={(e) => setFile(e.target.value)}
              required
            ></j-input>
            <j-box p="200"></j-box>
            <j-button variant="primary" full onClick={creatAgentFunc}>
              Create Agent
            </j-button>
          </j-box>
        </j-modal>
      )}

      {clearAgentModalOpen && (
        <j-modal
          size="fullscreen"
          open={clearAgentModalOpen}
          onToggle={(e: any) => setClearAgentModalOpen(e.target.open)}
        >
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Clear agent
              </j-text>
            </j-box>
            <j-text>
              Warning: by clearing the agent you will loose all the data and will have to start with a fresh agent
            </j-text>
            <j-box p="200"></j-box>
            <j-input
              placeholder="Password"
              label="Input your passphrase to clear agent"
              type={showPassword ? "text" : "password"}
              size="lg"
              required
              onInput={onPasswordChange}
              onKeyDown={onKeyDown}
              autovalidate
            >
              <j-button onClick={() => setShowPassword(!showPassword)} slot="end" variant="link" square>
                <j-icon name={showPassword ? "eye-slash" : "eye"} size="sm"></j-icon>
              </j-button>
            </j-input>
            <j-box p="200"></j-box>
            <j-flex>
              <j-button variant="primary" onClick={() => clearAgent(password)} loading={loading}>
                Delete Agent
              </j-button>
            </j-flex>
          </j-box>
        </j-modal>
      )}
      
      {showNetworkMetrics && (
        <j-modal
          size="fullscreen"
          open={showNetworkMetrics}
          onToggle={(e: any) => setShowNetworkMetrics(e.target.open)}
        >
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Network Metrics
              </j-text>
            </j-box>
            <j-box pb="500">
              <j-button 
                onClick={() => {
                  navigator.clipboard.writeText(networkMetrics);
                  alert('Network metrics copied to clipboard!');
                }}
                variant="primary"
              >
                <j-icon size="sm" slot="start" name="clipboard"></j-icon>
                Copy to Clipboard
              </j-button>
            </j-box>
            <j-box style={{ height: '70vh', overflow: 'auto', border: '1px solid #ccc', padding: '10px', backgroundColor: '#f5f5f5' }}>
              <pre style={{ margin: 0, fontSize: '12px', fontFamily: 'monospace', whiteSpace: 'pre-wrap' }}>
                {networkMetrics}
              </pre>
            </j-box>
          </j-box>
        </j-modal>
      )}
    </div>
  );
};

export default Profile;
