import { Agent, ImportResult, ImportStats, Literal } from "@coasys/ad4m";
import { invoke } from "@tauri-apps/api/core";
import { getCurrentWebviewWindow } from "@tauri-apps/api/webviewWindow";
import { writeText } from "@tauri-apps/plugin-clipboard-manager";
import { openUrl, revealItemInDir } from "@tauri-apps/plugin-opener";
import { join } from '@tauri-apps/api/path';
import { save as dialogSave, open as dialogOpen, message as dialogMessage, ask as dialogAsk, type MessageDialogOptions } from "@tauri-apps/plugin-dialog";
import { useCallback, useContext, useEffect, useState } from "react";
import QRCode from "react-qr-code";
import { PREDICATE_FIRSTNAME, PREDICATE_LASTNAME, PREDICATE_USERNAME } from "../constants/triples";
import { Ad4minContext } from "../context/Ad4minContext";
import { AgentContext } from "../context/AgentContext";
import { buildAd4mClient, copyTextToClipboard } from "../util";
import ActionButton from "./ActionButton";
import { cardStyle } from "./styles";
import { HoloHash } from '@spartan-hc/holo-hash';

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
    state: { url, did, client, expertMode, isInitialized, multiUserEnabled },
    methods: { toggleExpertMode, setMultiUserEnabled },
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

  const [agentInfos, setAgentInfos] = useState("");
  const [showAgentInfos, setShowAgentInfos] = useState(false);

  const [restartingHolochain, setRestartingHolochain] = useState(false);

  // MCP Configuration state
  const [mcpEnabled, setMcpEnabled] = useState(false);
  const [mcpPort, setMcpPort] = useState(3001);
  const [mcpChanged, setMcpChanged] = useState(false);

  const [logConfig, setLogConfig] = useState<Record<string, string>>({});
  const [newCrateName, setNewCrateName] = useState<string>("");
  const [newCrateLevel, setNewCrateLevel] = useState<string>("info");
  const [crateNameError, setCrateNameError] = useState<string>("");

  const [tlsConfig, setTlsConfig] = useState<{
    enabled: boolean;
    cert_file_path: string;
    key_file_path: string;
    tls_port: number | null;
  } | null>(null);
  const [tlsChanged, setTlsChanged] = useState(false);
  const [certPathError, setCertPathError] = useState<string>("");
  const [keyPathError, setKeyPathError] = useState<string>("");

  // SMTP Configuration state
  const [smtpConfig, setSmtpConfig] = useState<{
    enabled: boolean;
    host: string;
    port: number;
    username: string;
    password: string;
    from_address: string;
  } | null>(null);
  const [smtpChanged, setSmtpChanged] = useState(false);
  const [smtpTesting, setSmtpTesting] = useState(false);
  const [smtpTestStatus, setSmtpTestStatus] = useState<string>("");
  const [smtpTestEmail, setSmtpTestEmail] = useState<string>("");
  const [showSmtpPassword, setShowSmtpPassword] = useState(false);

  // Host Registration state
  type HostSession = { indexUrl: string; hostId: string; authToken: string; email: string } | null;
  const [hostSession, setHostSession] = useState<HostSession>(null);
  const [hostData, setHostData] = useState<any>(null); // fetched host data from API
  const [hostReg, setHostReg] = useState({
    indexUrl: "https://hosting.ad4m.dev",
    email: "",
    password: "",
    name: "",
    description: "",
    location: "",
    rates: '[{"description": "Base rate", "priceInHOT": 0.001}]',
    aiModels: '["llama3"]',
    computeSpecs: "",
    hostUrl: "",
  });
  const [hostRegStatus, setHostRegStatus] = useState<{
    type: "success" | "error" | "info";
    message: string;
  } | null>(null);
  const [hostRegistering, setHostRegistering] = useState(false);
  const [profilePic, setProfilePic] = useState<File | null>(null);

  const handleHostRegChange = (field: string, value: string) => {
    setHostReg((prev) => ({ ...prev, [field]: value }));
    setHostRegStatus(null);
  };

  // Save session to launcher config
  const saveHostSession = async (session: HostSession) => {
    setHostSession(session);
    if (session) {
      await invoke("set_host_registration", {
        registration: {
          index_url: session.indexUrl,
          host_id: session.hostId,
          auth_token: session.authToken,
          email: session.email,
        },
      });
    } else {
      await invoke("set_host_registration", { registration: null });
    }
  };

  // Fetch host data from API using session
  const fetchHostData = async (session: { indexUrl: string; hostId: string; authToken: string }) => {
    try {
      const res = await fetch(`${session.indexUrl}/hosts`, {
        headers: { Authorization: `Bearer ${session.authToken}` },
      });
      if (res.ok) {
        const hosts = await res.json();
        const mine = hosts.find((h: any) => h.id === session.hostId);
        if (mine) {
          setHostData(mine);
          setHostReg((prev) => ({
            ...prev,
            indexUrl: session.indexUrl,
            name: mine.name || "",
            description: mine.description || "",
            location: mine.location || "",
            hostUrl: mine.url || "",
            rates: JSON.stringify(mine.rates || []),
            aiModels: JSON.stringify(mine.aiModels || []),
            computeSpecs: mine.computeSpecs || "",
          }));
        }
      }
    } catch (e) {
      console.log("Failed to fetch host data:", e);
    }
  };

  const handleRegisterHost = async () => {
    if (!hostReg.email || !hostReg.password || !hostReg.name) {
      setHostRegStatus({ type: "error", message: "Email, password, and name are required." });
      return;
    }
    if (!hostReg.hostUrl) {
      setHostRegStatus({ type: "error", message: "Host URL is required. Configure TLS or enter a public URL." });
      return;
    }

    setHostRegistering(true);
    setHostRegStatus(null);

    try {
      const formData = new FormData();
      formData.append("email", hostReg.email);
      formData.append("password", hostReg.password);
      formData.append("name", hostReg.name);
      formData.append("description", hostReg.description);
      formData.append("location", hostReg.location);
      formData.append("url", hostReg.hostUrl);
      formData.append("rates", hostReg.rates);
      formData.append("aiModels", hostReg.aiModels);
      formData.append("computeSpecs", hostReg.computeSpecs);
      if (profilePic) {
        formData.append("profilePic", profilePic);
      }

      const res = await fetch(`${hostReg.indexUrl}/hosts/register`, {
        method: "POST",
        body: formData,
      });

      const data = await res.json();

      if (res.ok) {
        const session = {
          indexUrl: hostReg.indexUrl,
          hostId: data.hostId,
          authToken: data.authToken,
          email: hostReg.email,
        };
        await saveHostSession(session);
        setHostRegStatus({
          type: "success",
          message: `Host registered. Check your email for the verification link.`,
        });
      } else {
        setHostRegStatus({
          type: "error",
          message: data.error || `Registration failed (${res.status})`,
        });
      }
    } catch (err: any) {
      setHostRegStatus({
        type: "error",
        message: `Network error: ${err.message || "Could not reach index API"}`,
      });
    } finally {
      setHostRegistering(false);
    }
  };

  const handleLoginHost = async () => {
    if (!hostReg.email || !hostReg.password) {
      setHostRegStatus({ type: "error", message: "Email and password are required." });
      return;
    }

    setHostRegistering(true);
    setHostRegStatus(null);

    try {
      const res = await fetch(`${hostReg.indexUrl}/hosts/login`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ email: hostReg.email, password: hostReg.password }),
      });

      const data = await res.json();

      if (res.ok) {
        const session = {
          indexUrl: hostReg.indexUrl,
          hostId: data.hostId,
          authToken: data.authToken,
          email: hostReg.email,
        };
        await saveHostSession(session);
        await fetchHostData(session);
        setHostRegStatus({ type: "success", message: "Logged in successfully." });
      } else {
        setHostRegStatus({
          type: "error",
          message: data.error || `Login failed (${res.status})`,
        });
      }
    } catch (err: any) {
      setHostRegStatus({
        type: "error",
        message: `Network error: ${err.message || "Could not reach index API"}`,
      });
    } finally {
      setHostRegistering(false);
    }
  };

  const handleUpdateHost = async () => {
    if (!hostSession) return;

    setHostRegistering(true);
    setHostRegStatus(null);

    try {
      const formData = new FormData();
      formData.append("name", hostReg.name);
      formData.append("description", hostReg.description);
      formData.append("location", hostReg.location);
      formData.append("url", hostReg.hostUrl);
      formData.append("rates", hostReg.rates);
      formData.append("aiModels", hostReg.aiModels);
      formData.append("computeSpecs", hostReg.computeSpecs);
      if (profilePic) {
        formData.append("profilePic", profilePic);
      }

      const res = await fetch(`${hostSession.indexUrl}/hosts/${hostSession.hostId}`, {
        method: "PUT",
        headers: { Authorization: `Bearer ${hostSession.authToken}` },
        body: formData,
      });

      const data = await res.json();

      if (res.ok) {
        setHostData(data);
        setHostRegStatus({ type: "success", message: "Host updated successfully." });
      } else {
        if (res.status === 401) {
          // Token expired
          await saveHostSession(null);
          setHostData(null);
          setHostRegStatus({ type: "error", message: "Session expired. Please log in again." });
        } else {
          setHostRegStatus({
            type: "error",
            message: data.error || `Update failed (${res.status})`,
          });
        }
      }
    } catch (err: any) {
      setHostRegStatus({
        type: "error",
        message: `Network error: ${err.message || "Could not reach index API"}`,
      });
    } finally {
      setHostRegistering(false);
    }
  };

  const handleLogoutHost = async () => {
    await saveHostSession(null);
    setHostData(null);
    setHostRegStatus(null);
  };

  // Load host session from launcher config on mount
  useEffect(() => {
    const loadHostSession = async () => {
      try {
        const reg = await invoke<{ index_url: string; host_id: string; auth_token: string; email: string } | null>("get_host_registration");
        if (reg) {
          const session = {
            indexUrl: reg.index_url,
            hostId: reg.host_id,
            authToken: reg.auth_token,
            email: reg.email,
          };
          setHostSession(session);
          setHostReg((prev) => ({ ...prev, indexUrl: session.indexUrl, email: session.email }));
          await fetchHostData(session);
        }
      } catch (e) {
        console.log("Failed to load host registration:", e);
      }
    };
    loadHostSession();
  }, []);

  // Auto-populate host URL from TLS certificate domain
  useEffect(() => {
    const fetchTlsDomain = async () => {
      if (!client) return;
      try {
        const domain = await client.runtime.tlsDomain();
        console.log("TLS domain from certificate:", domain);
        if (domain) {
          setHostReg((prev) => prev.hostUrl ? prev : ({
            ...prev,
            hostUrl: `wss://${domain}`,
          }));
        }
      } catch (e) {
        console.log("TLS domain query failed (TLS may not be configured):", e);
      }
    };
    fetchTlsDomain();
  }, [client]);

  useEffect(() => {
    const loadMcpConfig = async () => {
      try {
        const [enabled, port] = await invoke<[boolean, number]>("get_mcp_config");
        setMcpEnabled(enabled);
        setMcpPort(port);
      } catch (error) {
        console.error("Failed to load MCP config:", error);
      }
    };
    loadMcpConfig();
  }, []);

  // Load current log config on component mount
  useEffect(() => {
    const loadLogConfig = async () => {
      try {
        const currentConfig = await invoke<Record<string, string>>("get_log_config");
        setLogConfig(currentConfig);
      } catch (error) {
        console.error("Failed to load log config:", error);
      }
    };
    loadLogConfig();
  }, []);

  // Load TLS config on component mount
  useEffect(() => {
    const loadTlsConfig = async () => {
      try {
        const config = await invoke<typeof tlsConfig>("get_tls_config");
        setTlsConfig(config || {
          enabled: false,
          cert_file_path: "",
          key_file_path: "",
          tls_port: 12001, // Default TLS port
        });
      } catch (error) {
        console.error("Failed to load TLS config:", error);
      }
    };
    loadTlsConfig();
  }, []);

  // Load SMTP config on component mount
  useEffect(() => {
    const loadSmtpConfig = async () => {
      try {
        const config = await invoke<typeof smtpConfig>("get_smtp_config");
        setSmtpConfig(config || {
          enabled: true,
          host: "",
          port: 587,
          username: "",
          password: "",
          from_address: "",
        });
      } catch (error) {
        console.error("Failed to load SMTP config:", error);
      }
    };
    loadSmtpConfig();
  }, []);

  const handleLogConfigChange = async (newConfig: Record<string, string>) => {
    try {
      await invoke<void>("set_log_config", { config: newConfig });
      setLogConfig(newConfig);
    } catch (error) {
      console.error("Failed to set log config:", error);
      alert("Failed to set log config. Check console for details.");
    }
  };

  const updateCrateLevel = (crateName: string, level: string) => {
    const newConfig = { ...logConfig, [crateName]: level };
    handleLogConfigChange(newConfig);
  };

  const validateCrateName = (name: string): string | null => {
    const trimmed = name.trim();
    
    // Check if empty
    if (!trimmed) {
      return "Crate name cannot be empty";
    }
    
    // Check if already exists
    if (trimmed in logConfig) {
      return "Crate name already exists";
    }
    
    // Check Rust crate naming conventions
    // Must start with letter or underscore
    if (!/^[a-zA-Z_]/.test(trimmed)) {
      return "Crate name must start with a letter or underscore";
    }
    
    // Only lowercase letters, numbers, hyphens, and underscores allowed
    if (!/^[a-zA-Z0-9_-]+$/.test(trimmed)) {
      return "Crate name can only contain letters, numbers, hyphens, and underscores";
    }
    
    // Cannot end with hyphen
    if (trimmed.endsWith("-")) {
      return "Crate name cannot end with a hyphen";
    }
    
    // No double hyphens
    if (trimmed.includes("--")) {
      return "Crate name cannot contain consecutive hyphens";
    }
    
    // Convert to lowercase for consistency with Rust conventions
    if (trimmed !== trimmed.toLowerCase()) {
      return "Crate name should be lowercase (will be converted automatically)";
    }
    
    return null;
  };

  const addNewCrate = () => {
    const trimmedName = newCrateName.trim().toLowerCase();
    const error = validateCrateName(trimmedName);
    
    if (error) {
      setCrateNameError(error);
      return;
    }
    
    // Clear any previous error
    setCrateNameError("");
    
    // Add the new crate
    const newConfig = { ...logConfig, [trimmedName]: newCrateLevel };
    handleLogConfigChange(newConfig);
    setNewCrateName("");
    setNewCrateLevel("info");
  };

  const handleTlsConfigChange = async (newConfig: typeof tlsConfig) => {
    if (!newConfig) return;

    try {
      await invoke<void>("set_tls_config", { config: newConfig });
      setTlsConfig(newConfig);
      setTlsChanged(true);
      setCertPathError("");
      setKeyPathError("");
    } catch (error) {
      const errorMsg = String(error);
      if (errorMsg.includes("Certificate")) {
        setCertPathError(errorMsg);
      } else if (errorMsg.includes("Key")) {
        setKeyPathError(errorMsg);
      } else {
        alert("Failed to save TLS config: " + errorMsg);
      }
    }
  };

  const handleCertFilePicker = async () => {
    const filePath = await dialogOpen({
      title: "Select TLS Certificate File",
      filters: [{ name: 'Certificate', extensions: ['pem', 'crt', 'cert'] }]
    });

    if (filePath && tlsConfig) {
      setTlsConfig({ ...tlsConfig, cert_file_path: filePath.toString() });
    }
  };

  const handleKeyFilePicker = async () => {
    const filePath = await dialogOpen({
      title: "Select TLS Private Key File",
      filters: [{ name: 'Private Key', extensions: ['pem', 'key'] }]
    });

    if (filePath && tlsConfig) {
      setTlsConfig({ ...tlsConfig, key_file_path: filePath.toString() });
    }
  };

  const handleSmtpConfigChange = async (newConfig: typeof smtpConfig) => {
    if (!newConfig) return;

    try {
      await invoke<void>("set_smtp_config", { config: newConfig });
      setSmtpConfig(newConfig);
      setSmtpChanged(true);
    } catch (error) {
      alert("Failed to save SMTP config: " + error);
    }
  };

  const handleSmtpTest = async () => {
    if (!smtpConfig || !smtpTestEmail) {
      alert("Please configure SMTP settings and provide a test email address");
      return;
    }

    setSmtpTesting(true);
    setSmtpTestStatus("");

    try {
      await invoke<void>("test_smtp_config", {
        config: smtpConfig,
        testEmail: smtpTestEmail
      });
      setSmtpTestStatus("Test email sent successfully! Check your inbox.");
    } catch (error) {
      setSmtpTestStatus("Failed to send test email: " + error);
    } finally {
      setSmtpTesting(false);
    }
  };

  const removeCrate = (crateName: string) => {
    const newConfig = { ...logConfig };
    delete newConfig[crateName];
    handleLogConfigChange(newConfig);
  };

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
    if (info) {
      try {
        const formattedInfo = JSON.stringify(JSON.parse(info), null, 2);
        setAgentInfos(formattedInfo);
        setShowAgentInfos(true);
      } catch (error) {
        console.error("Failed to parse agent info data:", error);
        alert("Failed to display agent info: The data returned from the backend is malformed. Please check the console for details.");
      }
    }
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

  const formatProxy = (proxy: string | null) => {
    if (!proxy) return "";
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

      <j-box px="500" my="500">
        <j-toggle full="" checked={mcpEnabled} onChange={async (e) => {
          try {
            const newEnabled = e.target.checked;
            await invoke("set_mcp_config", { enabled: newEnabled, port: mcpPort });
            setMcpEnabled(newEnabled);
            setMcpChanged(true);
          } catch (error) {
            console.error("Failed to toggle MCP:", error);
            e.target.checked = !e.target.checked;
          }
        }}>
          Enable MCP Server (AI Agent Integration)
        </j-toggle>
      </j-box>

      {mcpEnabled && (
        <>
          <j-box px="500" my="300">
            <j-box mb="200">
              <j-text size="500" weight="500">MCP Port</j-text>
            </j-box>
            <j-input
              type="number"
              value={mcpPort.toString()}
              onChange={(e) => {
                const port = parseInt((e.target as HTMLInputElement).value);
                if (!isNaN(port) && port > 0 && port < 65536) {
                  setMcpPort(port);
                  invoke("set_mcp_config", { enabled: true, port }).catch(console.error);
                  setMcpChanged(true);
                }
              }}
              placeholder="3001"
            />
            <j-box mt="200">
              <j-text size="300" color="ui-500">
                AI agents connect via MCP at http://localhost:{mcpPort}/mcp
              </j-text>
            </j-box>
          </j-box>

          {mcpChanged && (
            <j-box px="500" my="500">
              <j-box p="400" style={{
                backgroundColor: "#fff3cd",
                borderRadius: "8px",
                border: "1px solid #ffc107"
              }}>
                <j-flex a="center" gap="300">
                  <j-icon name="exclamation-triangle" color="warning"></j-icon>
                  <j-text size="500">
                    Restart required: MCP settings will take effect after restarting the launcher.
                  </j-text>
                </j-flex>
              </j-box>
            </j-box>
          )}
        </>
      )}

      <j-box px="500" my="500">
        <j-toggle full="" checked={multiUserEnabled} onChange={async (e) => {
          try {
            await setMultiUserEnabled(e.target.checked);
          } catch (error) {
            console.error("Failed to toggle multi-user mode:", error);
            e.target.checked = !e.target.checked; // Revert on error
          }
        }}>
          Multi-user mode
        </j-toggle>
      </j-box>

      {/* SMTP/Email Configuration Section */}
      {multiUserEnabled && (
        <>
          <j-box px="500" my="500">
            <j-text size="600" weight="600" color="black">
              Email Configuration (SMTP)
            </j-text>
          </j-box>

          <j-box px="500" my="300">
            <j-text size="500" color="ui-500">
              Configure SMTP settings for email verification. Required for email-based user authentication.
            </j-text>
          </j-box>

          {smtpConfig && (
            <>
              {/* SMTP Enable/Disable Toggle */}
              <j-box px="500" my="500">
                <j-toggle
                  checked={smtpConfig.enabled}
                  onChange={(e) => {
                    const newConfig = { ...smtpConfig, enabled: (e.target as HTMLInputElement).checked };
                    setSmtpConfig(newConfig);
                    handleSmtpConfigChange(newConfig);
                  }}
                >
                  Enable Email/SMTP
                </j-toggle>
              </j-box>

              {smtpConfig.enabled && (
                <>
                  {/* SMTP Host */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">SMTP Host</j-text>
                </j-box>
                <j-input
                  value={smtpConfig.host}
                  onChange={(e) => {
                    setSmtpConfig({
                      ...smtpConfig,
                      host: (e.target as HTMLInputElement).value
                    });
                  }}
                  placeholder="smtp.gmail.com"
                />
              </j-box>

              {/* SMTP Port */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">SMTP Port</j-text>
                </j-box>
                <j-input
                  type="number"
                  value={smtpConfig.port.toString()}
                  onChange={(e) => {
                    const port = parseInt((e.target as HTMLInputElement).value);
                    setSmtpConfig({
                      ...smtpConfig,
                      port: isNaN(port) ? 587 : port
                    });
                  }}
                  placeholder="587"
                />
                <j-box mt="200">
                  <j-text size="300" color="ui-500">
                    Common ports: 587 (STARTTLS), 465 (SSL/TLS), 25 (unencrypted)
                  </j-text>
                </j-box>
              </j-box>

              {/* SMTP Username */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Username</j-text>
                </j-box>
                <j-input
                  value={smtpConfig.username}
                  onChange={(e) => {
                    setSmtpConfig({
                      ...smtpConfig,
                      username: (e.target as HTMLInputElement).value
                    });
                  }}
                  placeholder="your-email@example.com"
                />
              </j-box>

              {/* SMTP Password */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Password / App Password</j-text>
                </j-box>
                <j-input
                  type={showSmtpPassword ? "text" : "password"}
                  value={smtpConfig.password}
                  onChange={(e) => {
                    setSmtpConfig({
                      ...smtpConfig,
                      password: (e.target as HTMLInputElement).value
                    });
                  }}
                  placeholder="Enter SMTP password or app password"
                >
                  <j-button onClick={() => setShowSmtpPassword(!showSmtpPassword)} slot="end" variant="link" square>
                    <j-icon name={showSmtpPassword ? "eye-slash" : "eye"} size="sm"></j-icon>
                  </j-button>
                </j-input>
                <j-box mt="200">
                  <j-text size="300" color="ui-500">
                    For Gmail, use an App Password. Regular passwords won't work with 2FA enabled.
                  </j-text>
                </j-box>
              </j-box>

              {/* From Address */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">From Email Address</j-text>
                </j-box>
                <j-input
                  value={smtpConfig.from_address}
                  onChange={(e) => {
                    setSmtpConfig({
                      ...smtpConfig,
                      from_address: (e.target as HTMLInputElement).value
                    });
                  }}
                  placeholder="noreply@yourdomain.com"
                />
                <j-box mt="200">
                  <j-text size="300" color="ui-500">
                    Email address that will appear in the "From" field of verification emails
                  </j-text>
                </j-box>
              </j-box>

              {/* Save Button */}
              <j-box px="500" my="500">
                <j-button onClick={() => handleSmtpConfigChange(smtpConfig)} variant="primary" full>
                  Save SMTP Configuration
                </j-button>
              </j-box>

              {/* Test Email Section */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Test Email Configuration</j-text>
                </j-box>
                <j-flex gap="200" a="center">
                  <j-input
                    value={smtpTestEmail}
                    onChange={(e) => {
                      setSmtpTestEmail((e.target as HTMLInputElement).value);
                    }}
                    placeholder="test@example.com"
                    style={{ flexGrow: "1" }}
                  />
                  <j-button onClick={handleSmtpTest} variant="secondary" loading={smtpTesting} disabled={smtpTesting}>
                    Send Test
                  </j-button>
                </j-flex>
                {smtpTestStatus && (
                  <j-box mt="200">
                    <j-text size="400" color={smtpTestStatus.includes("success") ? "success-500" : "danger-500"}>
                      {smtpTestStatus}
                    </j-text>
                  </j-box>
                )}
              </j-box>

              {/* Restart Required Warning */}
              {smtpChanged && (
                <j-box px="500" my="500">
                  <j-box p="400" style={{
                    backgroundColor: "#fff3cd",
                    borderRadius: "8px",
                    border: "1px solid #ffc107"
                  }}>
                    <j-flex a="center" gap="300">
                      <j-icon name="exclamation-triangle" color="warning"></j-icon>
                      <j-text size="500">
                        Restart required: SMTP settings will take effect after restarting the launcher.
                      </j-text>
                    </j-flex>
                  </j-box>
                </j-box>
              )}

              {/* Info about SMTP providers setup */}
              <j-box px="500" my="500">
                <j-box p="400" style={{
                  backgroundColor: "#e7f3ff",
                  borderRadius: "8px",
                  border: "1px solid #2196f3"
                }}>
                  <j-text size="400">
                    <strong>Common SMTP Provider Settings:</strong>
                    <br/><br/>
                    <strong>Gmail:</strong>
                    <br/>• Host: smtp.gmail.com, Port: 587
                    <br/>• Enable 2FA and generate an App Password (Google Account → Security → App Passwords)
                    <br/><br/>
                    <strong>Mailgun:</strong>
                    <br/>• Host: smtp.mailgun.org (or your region-specific host)
                    <br/>• Port: 587 (STARTTLS) or 465 (SSL/TLS)
                    <br/>• Username: Your Mailgun SMTP username (usually postmaster@your-domain)
                    <br/>• Password: Your Mailgun SMTP password
                    <br/><br/>
                    <strong>Note:</strong> Port 465 uses implicit SSL/TLS, Port 587 uses STARTTLS
                  </j-text>
                </j-box>
              </j-box>
                </>
              )}
            </>
          )}

          {/* Host Registration Section */}
          <j-box px="500" my="500" pt="500" style={{ borderTop: "1px solid var(--j-color-ui-200)" }}>
            <j-flex a="center" j="between">
              <j-text size="600" weight="600" color="black">
                Host Registration
              </j-text>
              {hostSession && (
                <j-text size="400" color="ui-500">
                  Logged in as {hostSession.email}
                </j-text>
              )}
            </j-flex>
          </j-box>

          <j-box px="500" my="300">
            <j-text size="500" color="ui-500">
              Register this executor with the AD4M hosting index so users can discover and connect to it.
            </j-text>
          </j-box>

          {/* Index API URL — always shown */}
          <j-box px="500" my="500">
            <j-box mb="200">
              <j-text size="500" weight="500">Index API URL</j-text>
            </j-box>
            <j-input
              value={hostReg.indexUrl}
              onInput={(e: any) => handleHostRegChange("indexUrl", e.target.value)}
              placeholder="https://hosting.ad4m.dev"
              disabled={!!hostSession}
            />
          </j-box>

          {!hostSession ? (
            <>
              {/* Not logged in: show login/register form */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Email *</j-text>
                </j-box>
                <j-input
                  value={hostReg.email}
                  onInput={(e: any) => handleHostRegChange("email", e.target.value)}
                  placeholder="admin@example.com"
                />
              </j-box>

              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Password *</j-text>
                </j-box>
                <j-input
                  type="password"
                  value={hostReg.password}
                  onInput={(e: any) => handleHostRegChange("password", e.target.value)}
                  placeholder="Password"
                />
              </j-box>

              {/* Host Name — only for registration */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Host Name (for new registration)</j-text>
                </j-box>
                <j-input
                  value={hostReg.name}
                  onInput={(e: any) => handleHostRegChange("name", e.target.value)}
                  placeholder="My AD4M Host"
                />
              </j-box>

              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Location (for new registration)</j-text>
                </j-box>
                <j-input
                  value={hostReg.location}
                  onInput={(e: any) => handleHostRegChange("location", e.target.value)}
                  placeholder="e.g. US-East, EU-West"
                />
              </j-box>

              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Host URL (for new registration)</j-text>
                </j-box>
                <j-input
                  value={hostReg.hostUrl}
                  onInput={(e: any) => handleHostRegChange("hostUrl", e.target.value)}
                  placeholder="wss://your-host-domain.com"
                />
                <j-box mt="100">
                  <j-text size="400" color="ui-400">
                    The public WebSocket URL where agents will connect. Auto-populated from TLS certificate if configured.
                  </j-text>
                </j-box>
              </j-box>

              {/* Status Message */}
              {hostRegStatus && (
                <j-box px="500" my="500">
                  <j-box p="400" style={{
                    backgroundColor: hostRegStatus.type === "success" ? "#e8f5e9" : hostRegStatus.type === "error" ? "#ffebee" : "#e7f3ff",
                    borderRadius: "8px",
                    border: `1px solid ${hostRegStatus.type === "success" ? "#4caf50" : hostRegStatus.type === "error" ? "#f44336" : "#2196f3"}`
                  }}>
                    <j-flex a="center" gap="300">
                      <j-icon
                        name={hostRegStatus.type === "success" ? "check-circle" : hostRegStatus.type === "error" ? "x-circle" : "info-circle"}
                        color={hostRegStatus.type === "success" ? "success" : hostRegStatus.type === "error" ? "danger" : "primary"}
                      ></j-icon>
                      <j-text size="500">{hostRegStatus.message}</j-text>
                    </j-flex>
                  </j-box>
                </j-box>
              )}

              {/* Login / Register buttons */}
              <j-box px="500" my="500">
                <j-flex gap="400">
                  <j-button
                    variant="primary"
                    size="lg"
                    onClick={handleLoginHost}
                    loading={hostRegistering}
                    disabled={hostRegistering}
                  >
                    Login
                  </j-button>
                  <j-button
                    variant="subtle"
                    size="lg"
                    onClick={handleRegisterHost}
                    loading={hostRegistering}
                    disabled={hostRegistering}
                  >
                    Register New Host
                  </j-button>
                </j-flex>
              </j-box>
            </>
          ) : (
            <>
              {/* Logged in: show editable host details */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Host Name</j-text>
                </j-box>
                <j-input
                  value={hostReg.name}
                  onInput={(e: any) => handleHostRegChange("name", e.target.value)}
                  placeholder="My AD4M Host"
                />
              </j-box>

              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Description</j-text>
                </j-box>
                <j-input
                  value={hostReg.description}
                  onInput={(e: any) => handleHostRegChange("description", e.target.value)}
                  placeholder="A brief description of your host"
                />
              </j-box>

              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Location</j-text>
                </j-box>
                <j-input
                  value={hostReg.location}
                  onInput={(e: any) => handleHostRegChange("location", e.target.value)}
                  placeholder="e.g. US-East, EU-West"
                />
              </j-box>

              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Host URL</j-text>
                </j-box>
                <j-input
                  value={hostReg.hostUrl}
                  onInput={(e: any) => handleHostRegChange("hostUrl", e.target.value)}
                  placeholder="wss://your-host-domain.com"
                />
              </j-box>

              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Rates (JSON)</j-text>
                </j-box>
                <j-input
                  value={hostReg.rates}
                  onInput={(e: any) => handleHostRegChange("rates", e.target.value)}
                  placeholder='[{"description": "Base rate", "priceInHOT": 0.001}]'
                />
              </j-box>

              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">AI Models (JSON)</j-text>
                </j-box>
                <j-input
                  value={hostReg.aiModels}
                  onInput={(e: any) => handleHostRegChange("aiModels", e.target.value)}
                  placeholder='["llama3", "mistral"]'
                />
              </j-box>

              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Compute Specs</j-text>
                </j-box>
                <j-input
                  value={hostReg.computeSpecs}
                  onInput={(e: any) => handleHostRegChange("computeSpecs", e.target.value)}
                  placeholder="e.g. 8 CPU, 32GB RAM, RTX 4090"
                />
              </j-box>

              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Profile Picture</j-text>
                </j-box>
                <input
                  type="file"
                  accept="image/*"
                  onChange={(e) => {
                    const f = e.target.files?.[0] || null;
                    if (f && f.size > 2 * 1024 * 1024) {
                      setHostRegStatus({ type: "error", message: "Profile picture must be under 2MB." });
                      return;
                    }
                    setProfilePic(f);
                    setHostRegStatus(null);
                  }}
                  style={{ fontSize: "14px" }}
                />
              </j-box>

              {/* Status Message */}
              {hostRegStatus && (
                <j-box px="500" my="500">
                  <j-box p="400" style={{
                    backgroundColor: hostRegStatus.type === "success" ? "#e8f5e9" : hostRegStatus.type === "error" ? "#ffebee" : "#e7f3ff",
                    borderRadius: "8px",
                    border: `1px solid ${hostRegStatus.type === "success" ? "#4caf50" : hostRegStatus.type === "error" ? "#f44336" : "#2196f3"}`
                  }}>
                    <j-flex a="center" gap="300">
                      <j-icon
                        name={hostRegStatus.type === "success" ? "check-circle" : hostRegStatus.type === "error" ? "x-circle" : "info-circle"}
                        color={hostRegStatus.type === "success" ? "success" : hostRegStatus.type === "error" ? "danger" : "primary"}
                      ></j-icon>
                      <j-text size="500">{hostRegStatus.message}</j-text>
                    </j-flex>
                  </j-box>
                </j-box>
              )}

              {/* Update / Logout buttons */}
              <j-box px="500" my="500">
                <j-flex gap="400">
                  <j-button
                    variant="primary"
                    size="lg"
                    onClick={handleUpdateHost}
                    loading={hostRegistering}
                    disabled={hostRegistering}
                  >
                    Save Changes
                  </j-button>
                  <j-button
                    variant="subtle"
                    size="lg"
                    onClick={handleLogoutHost}
                  >
                    Logout
                  </j-button>
                </j-flex>
              </j-box>
            </>
          )}
        </>
      )}

      {/* TLS Configuration Section */}
      <j-box px="500" my="500">
        <j-text size="600" weight="600" color="black">
          TLS/HTTPS Configuration
        </j-text>
      </j-box>

      <j-box px="500" my="300">
        <j-text size="500" color="ui-500">
          Enable HTTPS for secure remote access. Required for multi-user mode over the web.
        </j-text>
      </j-box>

      {tlsConfig && (
        <>
          <j-box px="500" my="500">
            <j-toggle
              checked={tlsConfig.enabled}
              onChange={(e) => {
                setTlsConfig({ ...tlsConfig, enabled: e.target.checked });
              }}
            >
              Enable TLS/HTTPS
            </j-toggle>
          </j-box>

          {tlsConfig.enabled && (
            <>
              {/* Certificate File Input */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Certificate File</j-text>
                </j-box>
                <j-flex gap="200" a="center">
                  <j-input
                    value={tlsConfig.cert_file_path}
                    onChange={(e) => {
                      setTlsConfig({
                        ...tlsConfig,
                        cert_file_path: (e.target as HTMLInputElement).value
                      });
                    }}
                    placeholder="/path/to/certificate.pem"
                    style={{ flexGrow: "1" }}
                    error={!!certPathError}
                  />
                  <j-button onClick={handleCertFilePicker} variant="secondary" size="sm">
                    Browse
                  </j-button>
                </j-flex>
                {certPathError && (
                  <j-box mt="200">
                    <j-text size="300" color="danger-500">{certPathError}</j-text>
                  </j-box>
                )}
              </j-box>

              {/* Private Key File Input */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">Private Key File</j-text>
                </j-box>
                <j-flex gap="200" a="center">
                  <j-input
                    value={tlsConfig.key_file_path}
                    onChange={(e) => {
                      setTlsConfig({
                        ...tlsConfig,
                        key_file_path: (e.target as HTMLInputElement).value
                      });
                    }}
                    placeholder="/path/to/private-key.pem"
                    style={{ flexGrow: "1" }}
                    error={!!keyPathError}
                  />
                  <j-button onClick={handleKeyFilePicker} variant="secondary" size="sm">
                    Browse
                  </j-button>
                </j-flex>
                {keyPathError && (
                  <j-box mt="200">
                    <j-text size="300" color="danger-500">{keyPathError}</j-text>
                  </j-box>
                )}
              </j-box>

              {/* TLS Port Input */}
              <j-box px="500" my="500">
                <j-box mb="200">
                  <j-text size="500" weight="500">TLS Port (HTTPS/WSS)</j-text>
                </j-box>
                <j-input
                  type="number"
                  value={tlsConfig.tls_port?.toString() || "12001"}
                  onChange={(e) => {
                    const port = parseInt((e.target as HTMLInputElement).value);
                    setTlsConfig({
                      ...tlsConfig,
                      tls_port: isNaN(port) ? null : port
                    });
                  }}
                  placeholder="12001"
                />
                <j-box mt="200">
                  <j-text size="300" color="ui-500">
                    Port for remote HTTPS/WSS access. Local HTTP will use port 12000.
                  </j-text>
                </j-box>
              </j-box>

              {/* Save Button */}
              <j-box px="500" my="500">
                <j-button onClick={() => handleTlsConfigChange(tlsConfig)} variant="primary" full>
                  Save TLS Configuration
                </j-button>
              </j-box>

              {/* Restart Required Warning */}
              {tlsChanged && (
                <j-box px="500" my="500">
                  <j-box p="400" style={{
                    backgroundColor: "#fff3cd",
                    borderRadius: "8px",
                    border: "1px solid #ffc107"
                  }}>
                    <j-flex a="center" gap="300">
                      <j-icon name="exclamation-triangle" color="warning"></j-icon>
                      <j-text size="500">
                        Restart required: TLS settings will take effect after restarting the launcher.
                      </j-text>
                    </j-flex>
                  </j-box>
                </j-box>
              )}

              {/* Info about binding behavior */}
              <j-box px="500" my="500">
                <j-box p="400" style={{
                  backgroundColor: "#e7f3ff",
                  borderRadius: "8px",
                  border: "1px solid #2196f3"
                }}>
                  <j-text size="400">
                    <strong>Note:</strong> When TLS is enabled, AD4M will run two GraphQL servers:
                    <br/>• <strong>HTTP on 127.0.0.1:12000</strong> - for local apps (Flux, launcher, ad4m-connect)
                    <br/>• <strong>HTTPS on 0.0.0.0:[TLS port]</strong> - for remote access with TLS encryption
                    <br/><br/>
                    Local apps will continue using port 12000 for compatibility. Remote users connect
                    via your domain on the configured TLS port.
                  </j-text>
                </j-box>
              </j-box>
            </>
          )}
        </>
      )}

      {expertMode && (
        <div>
          <j-box px="500" my="500">
            <j-text size="600" weight="600" color="black">
              Advanced Settings
            </j-text>
          </j-box>
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
            <j-button onClick={() => settrustedAgentModalOpen(true)} full variant="ghost">
              <j-icon size="sm" slot="start" name="shield-check"></j-icon>
              Show trusted agents
            </j-button>
          </j-box>

          {/* Holochain Section */}
          <j-box px="500" my="500">
            <j-text size="600" weight="600" color="black">
              Holochain
            </j-text>
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
            <j-button onClick={async () => {
              if (client) {
                try {
                  const metrics = await client.runtime.getNetworkMetrics();
                  const parsedMetrics = JSON.parse(metrics);
                  const formattedMetrics = JSON.stringify(parsedMetrics, (key, value) => {
                    // Keep buffer arrays on one line by not formatting them
                    if (Array.isArray(value) && value.every(item => typeof item === 'number' && item >= 0 && item <= 255)) {

                      if(value.length > 0){
                        let array = new Uint8Array(value)
                        try{
                          const holoHash = new HoloHash(array);
                          return holoHash.toString();
                        } catch (error) {
                          return JSON.stringify(value);
                        }
                      }
                    }
                    return value;
                  }, 2);
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
                  const confirmed = await dialogAsk('Are you sure you want to restart Holochain? This will temporarily disconnect you from the network and restart the Holochain conductor.', {
                    title: 'Restart Holochain'
                  });
                  
                  if (confirmed) {
                    setRestartingHolochain(true);
                    await client.runtime.restartHolochain();
                    await dialogMessage('Holochain has been restarted successfully!', {
                      title: 'Success'
                    });
                    setRestartingHolochain(false);
                  }
                } catch (error) {
                  console.error('Failed to restart Holochain:', error);
                  await dialogMessage('Failed to restart Holochain. Check console for details.', {
                    title: 'Error'
                  });
                  setRestartingHolochain(false);
                }
              }
            }} full variant="ghost" loading={restartingHolochain} disabled={restartingHolochain}>
              <j-icon size="sm" slot="start" name="arrow-clockwise"></j-icon>
              Restart Holochain
            </j-button>
          </j-box>

          <j-box px="500" my="500">
            <j-text size="600" weight="600" color="black">
              Logging Configuration
            </j-text>
          </j-box>

          <j-box px="500" my="500">
            <j-text size="500" color="ui-500">
              Configure log levels for different crates. Changes take effect after restart.
            </j-text>
          </j-box>

          {Object.entries(logConfig)
            .sort(([a], [b]) => {
              // Put rust_executor first, then alphabetical
              if (a === "rust_executor") return -1;
              if (b === "rust_executor") return 1;
              return a.localeCompare(b);
            })
            .map(([crateName, level]) => (
              <j-box key={crateName} px="500" my="300">
                <j-box mb="200">
                  <j-flex a="center" j="between">
                    <j-text size="500" weight="500" color="black">
                      {crateName === "rust_executor" ? "ad4m" : crateName}
                    </j-text>
                    <j-button
                      onClick={() => removeCrate(crateName)}
                      variant="ghost"
                      size="sm"
                    >
                      <j-icon name="trash" size="sm"></j-icon>
                    </j-button>
                  </j-flex>
                </j-box>
                <j-flex gap="200">
                  {["error", "warn", "info", "debug", "trace"].map((lvl) => (
                    <j-button
                      key={lvl}
                      onClick={() => updateCrateLevel(crateName, lvl)}
                      variant={level === lvl ? "primary" : "ghost"}
                      size="sm"
                    >
                      {lvl}
                    </j-button>
                  ))}
                </j-flex>
              </j-box>
            ))}

          <j-box px="500" my="500">
            <j-text size="500" weight="600" color="black">
              Add New Crate
            </j-text>
          </j-box>

          {crateNameError && (
            <j-box px="500" my="200">
              <j-text size="300" color="danger-500">
                {crateNameError}
              </j-text>
            </j-box>
          )}

          <j-box px="500" my="300">
            <j-box mb="200">
              <j-flex a="center" j="between">
                <j-input
                  value={newCrateName}
                  onChange={(e) => {
                    setNewCrateName((e.target as HTMLInputElement).value);
                    // Clear error when user starts typing
                    if (crateNameError) {
                      setCrateNameError("");
                    }
                  }}
                  placeholder="Crate name (e.g., tokio)"
                  style={{ flexGrow: "1", marginRight: "10px" }}
                />
                <j-button
                  onClick={addNewCrate}
                  variant="primary"
                  size="sm"
                >
                  Add
                </j-button>
              </j-flex>
            </j-box>
            <j-flex gap="200">
              {["error", "warn", "info", "debug", "trace"].map((lvl) => (
                <j-button
                  key={lvl}
                  onClick={() => setNewCrateLevel(lvl)}
                  variant={newCrateLevel === lvl ? "primary" : "ghost"}
                  size="sm"
                >
                  {lvl}
                </j-button>
              ))}
            </j-flex>
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
          <j-box px="400" py="600" style={{ height: '100vh', display: 'flex', flexDirection: 'column' }}>
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
            <div style={{ 
              flex: 1, 
              overflow: 'auto', 
              border: '1px solid #ccc', 
              padding: '10px', 
              backgroundColor: '#1e1e1e',
              maxHeight: 'calc(100vh - 200px)',
              scrollbarWidth: 'thin',
              scrollbarColor: '#888 #f1f1f1'
            }}>
              <pre style={{ 
                margin: 0, 
                fontSize: '12px', 
                fontFamily: 'monospace', 
                whiteSpace: 'pre-wrap',
                overflowWrap: 'break-word',
                wordBreak: 'break-all',
                color: '#e0e0e0'
              }}>
                {networkMetrics}
              </pre>
            </div>
          </j-box>
        </j-modal>
      )}

      {showAgentInfos && (
        <j-modal
          size="fullscreen"
          open={showAgentInfos}
          onToggle={(e: any) => setShowAgentInfos(e.target.open)}
        >
          <j-box px="400" py="600" style={{ height: '100vh', display: 'flex', flexDirection: 'column' }}>
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Holochain Agent Infos
              </j-text>
            </j-box>
            <j-box pb="500">
              <j-button 
                onClick={() => {
                  navigator.clipboard.writeText(agentInfos);
                  alert('Agent infos copied to clipboard!');
                }}
                variant="primary"
              >
                <j-icon size="sm" slot="start" name="clipboard"></j-icon>
                Copy to Clipboard
              </j-button>
            </j-box>
            <div style={{ 
              flex: 1, 
              overflow: 'auto', 
              border: '1px solid #ccc', 
              padding: '10px', 
              backgroundColor: '#1e1e1e',
              maxHeight: 'calc(100vh - 200px)',
              scrollbarWidth: 'thin',
              scrollbarColor: '#888 #f1f1f1'
            }}>
              <pre style={{ 
                margin: 0, 
                fontSize: '12px', 
                fontFamily: 'monospace', 
                whiteSpace: 'pre-wrap',
                overflowWrap: 'break-word',
                wordBreak: 'break-all',
                color: '#e0e0e0'
              }}>
                {agentInfos}
              </pre>
            </div>
          </j-box>
        </j-modal>
      )}
    </div>
  );
};

export default Profile;
