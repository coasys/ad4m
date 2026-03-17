import { useCallback, useContext, useEffect, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { open as dialogOpen } from "@tauri-apps/plugin-dialog";
import { Ad4minContext } from "../context/Ad4minContext";
import { cardStyle, listStyle } from "./styles";
import Wallet from "./Wallet";
import type { UserStatistics } from "@coasys/ad4m";

type HostSession = {
  indexUrl: string;
  hostId: string;
  authToken: string;
  email: string;
} | null;

type RegStep = "credentials" | "verify" | "logged-in";
type MembraneProofStatus = "none" | "fetching" | "done" | "error";

const Hosting = () => {
  const {
    state: { client, multiUserEnabled },
    methods: { setMultiUserEnabled },
  } = useContext(Ad4minContext);

  // ---- Users state ----
  const [users, setUsers] = useState<UserStatistics[]>([]);
  const [usersLoading, setUsersLoading] = useState(true);
  const [creditAmounts, setCreditAmounts] = useState<Record<string, string>>({});
  const [actionLoading, setActionLoading] = useState<Record<string, boolean>>({});

  // ---- Host Registration state ----
  const [hostSession, setHostSession] = useState<HostSession>(null);
  const [hostData, setHostData] = useState<any>(null);
  const [regStep, setRegStep] = useState<RegStep>("credentials");
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
  const [verificationCode, setVerificationCode] = useState("");
  const [membraneProofStatus, setMembraneProofStatus] = useState<MembraneProofStatus>("none");
  const [membraneProofError, setMembraneProofError] = useState<string | null>(null);

  // ---- SMTP state ----
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
  const [smtpTestStatus, setSmtpTestStatus] = useState("");
  const [smtpTestEmail, setSmtpTestEmail] = useState("");
  const [showSmtpPassword, setShowSmtpPassword] = useState(false);

  // ---- TLS state ----
  const [tlsConfig, setTlsConfig] = useState<{
    enabled: boolean;
    cert_file_path: string;
    key_file_path: string;
    tls_port: number | null;
  } | null>(null);
  const [tlsChanged, setTlsChanged] = useState(false);
  const [certPathError, setCertPathError] = useState("");
  const [keyPathError, setKeyPathError] = useState("");

  // ---- Helpers ----

  const handleHostRegChange = (field: string, value: string) => {
    setHostReg((prev) => ({ ...prev, [field]: value }));
    setHostRegStatus(null);
  };

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

  const fetchHostData = async (session: { indexUrl: string; hostId: string; authToken: string }): Promise<boolean> => {
    try {
      const res = await fetch(`${session.indexUrl}/hosts/me`, {
        headers: { Authorization: `Bearer ${session.authToken}` },
      });
      if (res.ok) {
        const mine = await res.json();
        setHostData(mine);
        setHostReg((prev) => ({
          ...prev,
          indexUrl: session.indexUrl,
          name: mine.name || prev.name,
          description: mine.description || prev.description,
          location: mine.location || prev.location,
          hostUrl: mine.url || prev.hostUrl,
          rates: (mine.rates && mine.rates.length > 0) ? JSON.stringify(mine.rates) : prev.rates,
          aiModels: (mine.aiModels && mine.aiModels.length > 0) ? JSON.stringify(mine.aiModels) : prev.aiModels,
          computeSpecs: mine.computeSpecs || prev.computeSpecs,
        }));
        return !!mine.emailVerified;
      }
    } catch (e) {
      console.log("Failed to fetch host data:", e);
    }
    return false;
  };

  const fetchMembraneProof = async (session: { indexUrl: string; hostId: string; authToken: string }) => {
    if (!client) return;

    // Skip if we already have the proof or DNA is installed
    try {
      const vi = await client.runtime.unytVersionInfo();
      if (vi) {
        const info = JSON.parse(vi);
        if (info.installed) {
          console.log("Unyt DNA already installed, skipping membrane proof fetch");
          setMembraneProofStatus("done");
          return;
        }
      }
    } catch {
      // Not installed yet, proceed
    }

    setMembraneProofStatus("fetching");
    setMembraneProofError(null);
    try {
      // 1. Get or create the Holochain agent key for the Unyt DNA
      const agentKey = await client.runtime.unytAgentKey();
      console.log("Unyt agent key:", agentKey);

      // 2. Request membrane proof from the hosting index API
      const res = await fetch(`${session.indexUrl}/hosts/${session.hostId}/request-membrane-proof`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${session.authToken}`,
        },
        body: JSON.stringify({ agent_key: agentKey }),
      });

      if (!res.ok) {
        const data = await res.json().catch(() => ({ error: `HTTP ${res.status}` }));
        throw new Error(data.error || `Failed to get membrane proof (${res.status})`);
      }

      const data = await res.json();
      const proofs: Record<string, string> = data.membrane_proofs || {};
      const proofKeys = Object.keys(proofs);

      if (proofKeys.length === 0) {
        throw new Error("No membrane proofs returned from joining service");
      }

      // 3. Store the first membrane proof (there's typically one role)
      const proof = proofs[proofKeys[0]];
      console.log("Got membrane proof for role:", proofKeys[0], "length:", proof.length);
      const result = await client.runtime.unytSetMembraneProof(proof);

      if (!result.success) {
        throw new Error(result.message || "Failed to store membrane proof");
      }

      console.log("Membrane proof stored, Unyt DNA will be installed automatically");
      setMembraneProofStatus("done");
    } catch (e: any) {
      console.error("Failed to fetch membrane proof:", e);
      setMembraneProofError(e.message || "Unknown error");
      setMembraneProofStatus("error");
    }
  };

  // ---- Registration flow ----

  const handleRegister = async () => {
    if (!hostReg.email || !hostReg.password) {
      setHostRegStatus({ type: "error", message: "Email and password are required." });
      return;
    }

    setHostRegistering(true);
    setHostRegStatus(null);

    try {
      const res = await fetch(`${hostReg.indexUrl}/hosts/register`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ email: hostReg.email, password: hostReg.password }),
      });

      const data = await res.json();

      if (res.ok) {
        // Save session immediately — register returns authToken+hostId
        const session = {
          indexUrl: hostReg.indexUrl,
          hostId: data.hostId,
          authToken: data.authToken,
          email: hostReg.email,
        };
        await saveHostSession(session);
        setHostRegStatus({
          type: "success",
          message: "Registration successful! Check your email for a verification code.",
        });
        setRegStep("verify");
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

  const handleVerifyEmail = async () => {
    if (!verificationCode) {
      setHostRegStatus({ type: "error", message: "Please enter the verification code." });
      return;
    }

    setHostRegistering(true);
    setHostRegStatus(null);

    try {
      const res = await fetch(`${hostReg.indexUrl}/hosts/verify-email`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ token: verificationCode }),
      });

      const data = await res.json();

      if (res.ok) {
        setHostRegStatus({ type: "success", message: "Email verified! Fetching auth material..." });
        if (hostSession) {
          await fetchHostData(hostSession);
          setRegStep("logged-in");
          // Automatically fetch membrane proof after verification
          fetchMembraneProof(hostSession);
        } else {
          // Fallback: login if session was lost
          await handleLogin();
        }
      } else {
        setHostRegStatus({
          type: "error",
          message: data.error || `Verification failed (${res.status})`,
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

  const handleResendVerification = async () => {
    if (!hostSession) {
      setHostRegStatus({ type: "error", message: "Not logged in. Please log in first." });
      return;
    }
    setHostRegistering(true);
    setHostRegStatus(null);
    try {
      const res = await fetch(`${hostSession.indexUrl}/hosts/resend-verification`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${hostSession.authToken}`,
        },
      });
      const data = await res.json();
      if (res.ok) {
        setHostRegStatus({ type: "success", message: "Verification code resent. Check your email." });
      } else {
        setHostRegStatus({ type: "error", message: data.error || `Resend failed (${res.status})` });
      }
    } catch (err: any) {
      setHostRegStatus({ type: "error", message: `Network error: ${err.message || "Could not reach index API"}` });
    } finally {
      setHostRegistering(false);
    }
  };

  const handleLogin = async () => {
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
        const verified = await fetchHostData(session);
        if (verified) {
          setRegStep("logged-in");
          setHostRegStatus({ type: "success", message: "Logged in successfully." });
          // Automatically fetch membrane proof if not already done
          if (membraneProofStatus !== "done") {
            fetchMembraneProof(session);
          }
        } else {
          setRegStep("verify");
          setHostRegStatus({ type: "info", message: "Email not yet verified. Check your inbox or resend the code." });
        }
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
          await saveHostSession(null);
          setHostData(null);
          setRegStep("credentials");
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

  const handleLogout = async () => {
    await saveHostSession(null);
    setHostData(null);
    setRegStep("credentials");
    setHostRegStatus(null);
  };

  // ---- SMTP handlers ----

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
    if (!smtpConfig || !smtpTestEmail) return;
    setSmtpTesting(true);
    setSmtpTestStatus("");
    try {
      await invoke<void>("test_smtp_config", { config: smtpConfig, testEmail: smtpTestEmail });
      setSmtpTestStatus("Test email sent successfully!");
    } catch (error) {
      setSmtpTestStatus("Failed: " + error);
    } finally {
      setSmtpTesting(false);
    }
  };

  // ---- TLS handlers ----

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
      if (errorMsg.includes("Certificate")) setCertPathError(errorMsg);
      else if (errorMsg.includes("Key")) setKeyPathError(errorMsg);
      else alert("Failed to save TLS config: " + errorMsg);
    }
  };

  const handleCertFilePicker = async () => {
    const filePath = await dialogOpen({
      title: "Select TLS Certificate File",
      filters: [{ name: "Certificate", extensions: ["pem", "crt", "cert"] }],
    });
    if (filePath && tlsConfig) {
      setTlsConfig({ ...tlsConfig, cert_file_path: filePath.toString() });
    }
  };

  const handleKeyFilePicker = async () => {
    const filePath = await dialogOpen({
      title: "Select TLS Private Key File",
      filters: [{ name: "Private Key", extensions: ["pem", "key"] }],
    });
    if (filePath && tlsConfig) {
      setTlsConfig({ ...tlsConfig, key_file_path: filePath.toString() });
    }
  };

  // ---- Users handlers ----

  const getUsers = useCallback(async () => {
    if (!client) return;
    try {
      setUsersLoading(true);
      const userList = await client.runtime.listUsers();
      setUsers(userList);
    } catch (error) {
      console.error("Failed to load users:", error);
    } finally {
      setUsersLoading(false);
    }
  }, [client]);

  const formatLastSeen = (lastSeen: string | null | undefined) => {
    if (!lastSeen) return "Never";
    const date = new Date(lastSeen);
    if (isNaN(date.getTime())) return "Invalid date";
    const diffMs = Date.now() - date.getTime();
    const diffMins = Math.floor(diffMs / 60000);
    const diffHours = Math.floor(diffMins / 60);
    const diffDays = Math.floor(diffHours / 24);
    if (diffMins < 1) return "Just now";
    if (diffMins < 60) return `${diffMins}m ago`;
    if (diffHours < 24) return `${diffHours}h ago`;
    if (diffDays < 7) return `${diffDays}d ago`;
    return date.toLocaleDateString();
  };

  const getStatusBadge = (lastSeen: string | null | undefined) => {
    if (!lastSeen) return <j-badge variant="gray">Inactive</j-badge>;
    const date = new Date(lastSeen);
    if (isNaN(date.getTime())) return <j-badge variant="gray">Invalid</j-badge>;
    const diffMins = Math.floor((Date.now() - date.getTime()) / 60000);
    if (diffMins < 5) return <j-badge variant="success">Online</j-badge>;
    if (diffMins < 30) return <j-badge variant="warning">Away</j-badge>;
    return <j-badge variant="gray">Offline</j-badge>;
  };

  const handleToggleFreeAccess = async (email: string, currentFreeAccess: boolean) => {
    const key = `free-${email}`;
    setActionLoading((prev) => ({ ...prev, [key]: true }));
    try {
      await client!.agent.setUserFreeAccess(email, !currentFreeAccess);
      await getUsers();
    } catch (error) {
      console.error("Failed to toggle free access:", error);
    } finally {
      setActionLoading((prev) => ({ ...prev, [key]: false }));
    }
  };

  const handleSetCredits = async (email: string) => {
    const amount = parseFloat(creditAmounts[email] || "0");
    if (isNaN(amount) || amount < 0) return;
    const key = `credits-${email}`;
    setActionLoading((prev) => ({ ...prev, [key]: true }));
    try {
      await client!.agent.setUserCredits(email, amount);
      setCreditAmounts((prev) => ({ ...prev, [email]: "" }));
      await getUsers();
    } catch (error) {
      console.error("Failed to set credits:", error);
    } finally {
      setActionLoading((prev) => ({ ...prev, [key]: false }));
    }
  };

  // ---- Effects ----

  useEffect(() => {
    if (multiUserEnabled && client) getUsers();
  }, [multiUserEnabled, client, getUsers]);

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
          const verified = await fetchHostData(session);
          setRegStep(verified ? "logged-in" : "verify");
          // If verified but membrane proof might be missing, try to fetch it
          if (verified) {
            fetchMembraneProof(session);
          }
        }
      } catch (e) {
        console.log("Failed to load host registration:", e);
      }
    };
    loadHostSession();
  }, []);

  // Auto-populate host URL from TLS domain
  useEffect(() => {
    if (!client) return;
    const fetchTlsDomain = async () => {
      try {
        const domain = await client.runtime.tlsDomain();
        if (domain) {
          setHostReg((prev) => (prev.hostUrl ? prev : { ...prev, hostUrl: `wss://${domain}` }));
        }
      } catch (e) {
        // TLS not configured
      }
    };
    fetchTlsDomain();
  }, [client]);

  useEffect(() => {
    const loadTlsConfig = async () => {
      try {
        const config = await invoke<typeof tlsConfig>("get_tls_config");
        setTlsConfig(config || { enabled: false, cert_file_path: "", key_file_path: "", tls_port: 12001 });
      } catch (e) {
        console.error("Failed to load TLS config:", e);
      }
    };
    loadTlsConfig();
  }, []);

  useEffect(() => {
    const loadSmtpConfig = async () => {
      try {
        const config = await invoke<typeof smtpConfig>("get_smtp_config");
        setSmtpConfig(config || { enabled: true, host: "", port: 587, username: "", password: "", from_address: "" });
      } catch (e) {
        console.error("Failed to load SMTP config:", e);
      }
    };
    loadSmtpConfig();
  }, []);

  // ---- Status message component ----

  const StatusMessage = () => {
    if (!hostRegStatus) return null;
    const bg = hostRegStatus.type === "success" ? "#e8f5e9" : hostRegStatus.type === "error" ? "#ffebee" : "#e7f3ff";
    const border = hostRegStatus.type === "success" ? "#4caf50" : hostRegStatus.type === "error" ? "#f44336" : "#2196f3";
    const icon = hostRegStatus.type === "success" ? "check-circle" : hostRegStatus.type === "error" ? "x-circle" : "info-circle";
    const color = hostRegStatus.type === "success" ? "success" : hostRegStatus.type === "error" ? "danger" : "primary";
    return (
      <j-box px="500" my="300">
        <j-box p="400" style={{ backgroundColor: bg, borderRadius: "8px", border: `1px solid ${border}` }}>
          <j-flex a="center" gap="300">
            <j-icon name={icon} color={color}></j-icon>
            <j-text size="500">{hostRegStatus.message}</j-text>
          </j-flex>
        </j-box>
      </j-box>
    );
  };

  // ==== RENDER ====

  return (
    <div>
      {/* Multi-user toggle */}
      <j-box px="500" my="500">
        <j-toggle
          full=""
          checked={multiUserEnabled}
          onChange={async (e: any) => {
            try {
              await setMultiUserEnabled(e.target.checked);
            } catch (error) {
              console.error("Failed to toggle multi-user mode:", error);
              e.target.checked = !e.target.checked;
            }
          }}
        >
          Multi-user mode
        </j-toggle>
      </j-box>

      {!multiUserEnabled && (
        <j-box px="500" my="300">
          <j-text size="500" color="ui-500">
            Enable multi-user mode to host this AD4M instance for other users.
          </j-text>
        </j-box>
      )}

      {multiUserEnabled && (
        <>
          {/* ===== USERS SECTION ===== */}
          <j-box px="500" my="500" pt="500" style={{ borderTop: "1px solid var(--j-color-ui-200)" }}>
            <j-flex a="center" j="between">
              <j-text size="600" weight="600" color="black">Users</j-text>
              <j-button size="sm" variant="subtle" onClick={getUsers}>Refresh</j-button>
            </j-flex>
          </j-box>

          {usersLoading ? (
            <j-box px="500" py="300">
              <j-flex gap="300" a="center">
                <j-spinner size="sm"></j-spinner>
                <j-text color="ui-500">Loading users...</j-text>
              </j-flex>
            </j-box>
          ) : users.length === 0 ? (
            <j-box px="500" my="300">
              <j-text size="500" color="ui-400">No users yet. Users will appear here when they connect.</j-text>
            </j-box>
          ) : (
            <div style={{ ...listStyle, padding: "0 20px", marginTop: 0 }}>
              <j-text size="400" color="ui-500" style={{ marginBottom: "8px", display: "block" }}>
                {users.length} user{users.length !== 1 ? "s" : ""} registered
              </j-text>
              {users.map((user, index) => (
                <div key={`user-${index}`} style={{ ...cardStyle, width: "100%" }}>
                  <j-flex gap="500" direction="column">
                    <j-flex gap="400" a="center">
                      <j-avatar size="lg" hash={user.email}></j-avatar>
                      <j-flex direction="column" gap="100">
                        <j-flex gap="300" a="center">
                          <j-text nomargin variant="heading-sm" size="600" weight="600">{user.email}</j-text>
                          {getStatusBadge(user.lastSeen)}
                          {(user as any).freeAccess && <j-badge variant="success">Free Access</j-badge>}
                        </j-flex>
                        <j-text nomargin size="300" color="ui-500" style={{ wordBreak: "break-all", fontFamily: "monospace" }}>
                          {user.did || "DID not set"}
                        </j-text>
                      </j-flex>
                    </j-flex>

                    <j-flex gap="600">
                      <j-flex direction="column" gap="100">
                        <j-text nomargin size="300" color="ui-500" weight="500">LAST SEEN</j-text>
                        <j-text nomargin size="400">{formatLastSeen(user.lastSeen)}</j-text>
                      </j-flex>
                      <j-flex direction="column" gap="100">
                        <j-text nomargin size="300" color="ui-500" weight="500">PERSPECTIVES</j-text>
                        <j-text nomargin size="400">{user.perspectiveCount}</j-text>
                      </j-flex>
                      <j-flex direction="column" gap="100">
                        <j-text nomargin size="300" color="ui-500" weight="500">CREDITS</j-text>
                        <j-text nomargin size="400" weight="600">
                          {(user as any).remainingCredits === "unlimited" ? "Unlimited" : (user as any).remainingCredits || "0"}
                        </j-text>
                      </j-flex>
                    </j-flex>

                    <j-box style={{ borderTop: "1px solid var(--j-color-ui-200)", paddingTop: "var(--j-space-400)" }}>
                      <j-flex gap="500" a="center" wrap="wrap">
                        <j-toggle
                          checked={(user as any).freeAccess}
                          disabled={actionLoading[`free-${user.email}`]}
                          onChange={() => handleToggleFreeAccess(user.email, (user as any).freeAccess)}
                        >
                          Free access
                        </j-toggle>
                        {!(user as any).freeAccess && (
                          <j-flex gap="300" a="center">
                            <j-input
                              size="sm"
                              type="number"
                              placeholder="Amount"
                              value={creditAmounts[user.email] || ""}
                              onInput={(e: any) => setCreditAmounts((prev) => ({ ...prev, [user.email]: e.target.value }))}
                              style={{ width: "120px" }}
                            ></j-input>
                            <j-button
                              size="sm"
                              variant="primary"
                              disabled={actionLoading[`credits-${user.email}`] || !creditAmounts[user.email] || parseFloat(creditAmounts[user.email]) <= 0}
                              onClick={() => handleSetCredits(user.email)}
                            >
                              {actionLoading[`credits-${user.email}`] ? "Setting..." : "Set Credits"}
                            </j-button>
                          </j-flex>
                        )}
                      </j-flex>
                    </j-box>
                  </j-flex>
                </div>
              ))}
            </div>
          )}

          {/* ===== PAID HOSTING REGISTRATION ===== */}
          <j-box px="500" my="500" pt="500" style={{ borderTop: "1px solid var(--j-color-ui-200)" }}>
            <j-flex a="center" j="between">
              <j-text size="600" weight="600" color="black">Paid Hosting</j-text>
              {hostSession && (
                <j-text size="400" color="ui-500">Logged in as {hostSession.email}</j-text>
              )}
            </j-flex>
          </j-box>

          <j-box px="500" my="300">
            <j-text size="500" color="ui-500">
              Get paid in mHOT for hosting this AD4M instance for others. Registration is required both
              for showing up in ad4m-connect (so people can find this instance) and for getting a joining
              code for the mHOT Unyt DHT (to receive payments).
            </j-text>
          </j-box>

          {/* Index API URL */}
          <j-box px="500" my="400">
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

          {/* Step 1: Credentials */}
          {regStep === "credentials" && !hostSession && (
            <>
              <j-box px="500" my="400">
                <j-box mb="200">
                  <j-text size="500" weight="500">Email</j-text>
                </j-box>
                <j-input
                  value={hostReg.email}
                  onInput={(e: any) => handleHostRegChange("email", e.target.value)}
                  placeholder="admin@example.com"
                />
              </j-box>

              <j-box px="500" my="400">
                <j-box mb="200">
                  <j-text size="500" weight="500">Password</j-text>
                </j-box>
                <j-input
                  type="password"
                  value={hostReg.password}
                  onInput={(e: any) => handleHostRegChange("password", e.target.value)}
                  placeholder="Password"
                />
              </j-box>

              <StatusMessage />

              <j-box px="500" my="400">
                <j-flex gap="400">
                  <j-button variant="primary" size="lg" onClick={handleLogin} loading={hostRegistering} disabled={hostRegistering}>
                    Login
                  </j-button>
                  <j-button variant="subtle" size="lg" onClick={handleRegister} loading={hostRegistering} disabled={hostRegistering}>
                    Register
                  </j-button>
                </j-flex>
              </j-box>
            </>
          )}

          {/* Step 2: Email Verification */}
          {regStep === "verify" && (
            <>
              <j-box px="500" my="400">
                <j-box p="400" style={{ backgroundColor: "#e7f3ff", borderRadius: "8px", border: "1px solid #2196f3" }}>
                  <j-text size="500">
                    A verification code has been sent to <strong>{hostReg.email}</strong>. Enter the code below.
                  </j-text>
                </j-box>
              </j-box>

              <j-box px="500" my="400">
                <j-box mb="200">
                  <j-text size="500" weight="500">Verification Code</j-text>
                </j-box>
                <j-input
                  value={verificationCode}
                  onInput={(e: any) => setVerificationCode(e.target.value)}
                  placeholder="Enter 6-digit code"
                />
              </j-box>

              <StatusMessage />

              <j-box px="500" my="400">
                <j-flex gap="400">
                  <j-button variant="primary" size="lg" onClick={handleVerifyEmail} loading={hostRegistering} disabled={hostRegistering}>
                    Verify
                  </j-button>
                  <j-button variant="subtle" size="lg" onClick={handleResendVerification} disabled={hostRegistering}>
                    Resend Code
                  </j-button>
                  <j-button variant="subtle" size="lg" onClick={() => { setRegStep("credentials"); setHostRegStatus(null); }}>
                    Back
                  </j-button>
                </j-flex>
              </j-box>
            </>
          )}

          {/* Step 3: Logged in — host profile fields */}
          {regStep === "logged-in" && hostSession && (
            <>
              {/* Membrane proof / Unyt DNA status */}
              {membraneProofStatus === "fetching" && (
                <j-box px="500" my="400">
                  <j-box p="400" style={{ backgroundColor: "#e7f3ff", borderRadius: "8px", border: "1px solid #2196f3" }}>
                    <j-flex a="center" gap="300">
                      <j-spinner size="sm"></j-spinner>
                      <j-text size="500">Fetching Unyt DHT auth material...</j-text>
                    </j-flex>
                  </j-box>
                </j-box>
              )}
              {membraneProofStatus === "done" && (
                <j-box px="500" my="400">
                  <j-box p="400" style={{ backgroundColor: "#e8f5e9", borderRadius: "8px", border: "1px solid #4caf50" }}>
                    <j-flex a="center" gap="300">
                      <j-icon name="check-circle" color="success"></j-icon>
                      <j-text size="500">Unyt DHT auth material received. DNA will be installed automatically.</j-text>
                    </j-flex>
                  </j-box>
                </j-box>
              )}
              {membraneProofStatus === "error" && (
                <j-box px="500" my="400">
                  <j-box p="400" style={{ backgroundColor: "#ffebee", borderRadius: "8px", border: "1px solid #f44336" }}>
                    <j-flex a="center" gap="300" wrap="wrap">
                      <j-icon name="x-circle" color="danger"></j-icon>
                      <j-text size="500">Failed to fetch auth material: {membraneProofError}</j-text>
                      <j-button size="sm" variant="subtle" onClick={() => fetchMembraneProof(hostSession)}>
                        Retry
                      </j-button>
                    </j-flex>
                  </j-box>
                </j-box>
              )}

              <j-box px="500" my="400">
                <j-box mb="200"><j-text size="500" weight="500">Host Name</j-text></j-box>
                <j-input value={hostReg.name} onInput={(e: any) => handleHostRegChange("name", e.target.value)} placeholder="My AD4M Host" />
              </j-box>

              <j-box px="500" my="400">
                <j-box mb="200"><j-text size="500" weight="500">Description</j-text></j-box>
                <j-input value={hostReg.description} onInput={(e: any) => handleHostRegChange("description", e.target.value)} placeholder="A brief description of your host" />
              </j-box>

              <j-box px="500" my="400">
                <j-box mb="200"><j-text size="500" weight="500">Location</j-text></j-box>
                <j-input value={hostReg.location} onInput={(e: any) => handleHostRegChange("location", e.target.value)} placeholder="e.g. US-East, EU-West" />
              </j-box>

              <j-box px="500" my="400">
                <j-box mb="200"><j-text size="500" weight="500">Host URL</j-text></j-box>
                <j-input value={hostReg.hostUrl} onInput={(e: any) => handleHostRegChange("hostUrl", e.target.value)} placeholder="wss://your-host-domain.com" />
                <j-box mt="100">
                  <j-text size="400" color="ui-400">Auto-populated from TLS certificate if configured.</j-text>
                </j-box>
              </j-box>

              <j-box px="500" my="400">
                <j-box mb="200"><j-text size="500" weight="500">Rates (JSON)</j-text></j-box>
                <j-input value={hostReg.rates} onInput={(e: any) => handleHostRegChange("rates", e.target.value)} placeholder='[{"description": "Base rate", "priceInHOT": 0.001}]' />
              </j-box>

              <j-box px="500" my="400">
                <j-box mb="200"><j-text size="500" weight="500">AI Models (JSON)</j-text></j-box>
                <j-input value={hostReg.aiModels} onInput={(e: any) => handleHostRegChange("aiModels", e.target.value)} placeholder='["llama3", "mistral"]' />
              </j-box>

              <j-box px="500" my="400">
                <j-box mb="200"><j-text size="500" weight="500">Compute Specs</j-text></j-box>
                <j-input value={hostReg.computeSpecs} onInput={(e: any) => handleHostRegChange("computeSpecs", e.target.value)} placeholder="e.g. 8 CPU, 32GB RAM, RTX 4090" />
              </j-box>

              <j-box px="500" my="400">
                <j-box mb="200"><j-text size="500" weight="500">Profile Picture</j-text></j-box>
                <input
                  type="file"
                  accept="image/*"
                  onChange={(e) => {
                    const f = (e.target as HTMLInputElement).files?.[0] || null;
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

              <StatusMessage />

              <j-box px="500" my="400">
                <j-flex gap="400">
                  <j-button variant="primary" size="lg" onClick={handleUpdateHost} loading={hostRegistering} disabled={hostRegistering}>
                    Save Changes
                  </j-button>
                  <j-button variant="subtle" size="lg" onClick={handleLogout}>
                    Logout
                  </j-button>
                </j-flex>
              </j-box>
            </>
          )}

          {/* ===== WALLET ===== */}
          <Wallet />

          {/* ===== SMTP CONFIGURATION ===== */}
          <j-box px="500" my="500" pt="500" style={{ borderTop: "1px solid var(--j-color-ui-200)" }}>
            <j-text size="600" weight="600" color="black">Email Configuration (SMTP)</j-text>
          </j-box>

          <j-box px="500" my="300">
            <j-text size="500" color="ui-500">
              Configure SMTP settings for email verification. Required for email-based user authentication.
            </j-text>
          </j-box>

          {smtpConfig && (
            <>
              <j-box px="500" my="400">
                <j-toggle
                  checked={smtpConfig.enabled}
                  onChange={(e: any) => {
                    const newConfig = { ...smtpConfig, enabled: e.target.checked };
                    setSmtpConfig(newConfig);
                    handleSmtpConfigChange(newConfig);
                  }}
                >
                  Enable Email/SMTP
                </j-toggle>
              </j-box>

              {smtpConfig.enabled && (
                <>
                  <j-box px="500" my="400">
                    <j-box mb="200"><j-text size="500" weight="500">SMTP Host</j-text></j-box>
                    <j-input value={smtpConfig.host} onChange={(e: any) => setSmtpConfig({ ...smtpConfig, host: e.target.value })} placeholder="smtp.gmail.com" />
                  </j-box>
                  <j-box px="500" my="400">
                    <j-box mb="200"><j-text size="500" weight="500">SMTP Port</j-text></j-box>
                    <j-input type="number" value={smtpConfig.port.toString()} onChange={(e: any) => setSmtpConfig({ ...smtpConfig, port: parseInt(e.target.value) || 587 })} placeholder="587" />
                    <j-box mt="200"><j-text size="300" color="ui-500">Common: 587 (STARTTLS), 465 (SSL/TLS), 25 (unencrypted)</j-text></j-box>
                  </j-box>
                  <j-box px="500" my="400">
                    <j-box mb="200"><j-text size="500" weight="500">Username</j-text></j-box>
                    <j-input value={smtpConfig.username} onChange={(e: any) => setSmtpConfig({ ...smtpConfig, username: e.target.value })} placeholder="your-email@example.com" />
                  </j-box>
                  <j-box px="500" my="400">
                    <j-box mb="200"><j-text size="500" weight="500">Password / App Password</j-text></j-box>
                    <j-input
                      type={showSmtpPassword ? "text" : "password"}
                      value={smtpConfig.password}
                      onChange={(e: any) => setSmtpConfig({ ...smtpConfig, password: e.target.value })}
                      placeholder="Enter SMTP password"
                    >
                      <j-button onClick={() => setShowSmtpPassword(!showSmtpPassword)} slot="end" variant="link" square>
                        <j-icon name={showSmtpPassword ? "eye-slash" : "eye"} size="sm"></j-icon>
                      </j-button>
                    </j-input>
                  </j-box>
                  <j-box px="500" my="400">
                    <j-box mb="200"><j-text size="500" weight="500">From Email Address</j-text></j-box>
                    <j-input value={smtpConfig.from_address} onChange={(e: any) => setSmtpConfig({ ...smtpConfig, from_address: e.target.value })} placeholder="noreply@yourdomain.com" />
                  </j-box>
                  <j-box px="500" my="400">
                    <j-button onClick={() => handleSmtpConfigChange(smtpConfig)} variant="primary" full>Save SMTP Configuration</j-button>
                  </j-box>

                  {/* Test Email */}
                  <j-box px="500" my="400">
                    <j-box mb="200"><j-text size="500" weight="500">Test Email</j-text></j-box>
                    <j-flex gap="200" a="center">
                      <j-input value={smtpTestEmail} onChange={(e: any) => setSmtpTestEmail(e.target.value)} placeholder="test@example.com" style={{ flexGrow: "1" }} />
                      <j-button onClick={handleSmtpTest} variant="secondary" loading={smtpTesting} disabled={smtpTesting}>Send Test</j-button>
                    </j-flex>
                    {smtpTestStatus && (
                      <j-box mt="200">
                        <j-text size="400" color={smtpTestStatus.includes("success") ? "success-500" : "danger-500"}>{smtpTestStatus}</j-text>
                      </j-box>
                    )}
                  </j-box>

                  {smtpChanged && (
                    <j-box px="500" my="400">
                      <j-box p="400" style={{ backgroundColor: "#fff3cd", borderRadius: "8px", border: "1px solid #ffc107" }}>
                        <j-flex a="center" gap="300">
                          <j-icon name="exclamation-triangle" color="warning"></j-icon>
                          <j-text size="500">Restart required for SMTP changes to take effect.</j-text>
                        </j-flex>
                      </j-box>
                    </j-box>
                  )}
                </>
              )}
            </>
          )}

          {/* ===== TLS CONFIGURATION ===== */}
          <j-box px="500" my="500" pt="500" style={{ borderTop: "1px solid var(--j-color-ui-200)" }}>
            <j-text size="600" weight="600" color="black">TLS/HTTPS Configuration</j-text>
          </j-box>

          <j-box px="500" my="300">
            <j-text size="500" color="ui-500">
              Enable HTTPS for secure remote access. Required for multi-user mode over the web.
            </j-text>
          </j-box>

          {tlsConfig && (
            <>
              <j-box px="500" my="400">
                <j-toggle checked={tlsConfig.enabled} onChange={(e: any) => setTlsConfig({ ...tlsConfig, enabled: e.target.checked })}>
                  Enable TLS/HTTPS
                </j-toggle>
              </j-box>

              {tlsConfig.enabled && (
                <>
                  <j-box px="500" my="400">
                    <j-box mb="200"><j-text size="500" weight="500">Certificate File</j-text></j-box>
                    <j-flex gap="200" a="center">
                      <j-input value={tlsConfig.cert_file_path} onChange={(e: any) => setTlsConfig({ ...tlsConfig, cert_file_path: e.target.value })} placeholder="/path/to/certificate.pem" style={{ flexGrow: "1" }} error={!!certPathError} />
                      <j-button onClick={handleCertFilePicker} variant="secondary" size="sm">Browse</j-button>
                    </j-flex>
                    {certPathError && <j-box mt="200"><j-text size="300" color="danger-500">{certPathError}</j-text></j-box>}
                  </j-box>

                  <j-box px="500" my="400">
                    <j-box mb="200"><j-text size="500" weight="500">Private Key File</j-text></j-box>
                    <j-flex gap="200" a="center">
                      <j-input value={tlsConfig.key_file_path} onChange={(e: any) => setTlsConfig({ ...tlsConfig, key_file_path: e.target.value })} placeholder="/path/to/private-key.pem" style={{ flexGrow: "1" }} error={!!keyPathError} />
                      <j-button onClick={handleKeyFilePicker} variant="secondary" size="sm">Browse</j-button>
                    </j-flex>
                    {keyPathError && <j-box mt="200"><j-text size="300" color="danger-500">{keyPathError}</j-text></j-box>}
                  </j-box>

                  <j-box px="500" my="400">
                    <j-box mb="200"><j-text size="500" weight="500">TLS Port</j-text></j-box>
                    <j-input type="number" value={tlsConfig.tls_port?.toString() || "12001"} onChange={(e: any) => setTlsConfig({ ...tlsConfig, tls_port: parseInt(e.target.value) || null })} placeholder="12001" />
                    <j-box mt="200"><j-text size="300" color="ui-500">Port for remote HTTPS/WSS access.</j-text></j-box>
                  </j-box>

                  <j-box px="500" my="400">
                    <j-button onClick={() => handleTlsConfigChange(tlsConfig)} variant="primary" full>Save TLS Configuration</j-button>
                  </j-box>

                  {tlsChanged && (
                    <j-box px="500" my="400">
                      <j-box p="400" style={{ backgroundColor: "#fff3cd", borderRadius: "8px", border: "1px solid #ffc107" }}>
                        <j-flex a="center" gap="300">
                          <j-icon name="exclamation-triangle" color="warning"></j-icon>
                          <j-text size="500">Restart required for TLS changes to take effect.</j-text>
                        </j-flex>
                      </j-box>
                    </j-box>
                  )}

                  <j-box px="500" my="400">
                    <j-box p="400" style={{ backgroundColor: "#e7f3ff", borderRadius: "8px", border: "1px solid #2196f3" }}>
                      <j-text size="400">
                        <strong>Note:</strong> When TLS is enabled, AD4M runs two GraphQL servers:
                        <br />- <strong>HTTP on 127.0.0.1:12000</strong> — local apps
                        <br />- <strong>HTTPS on 0.0.0.0:{tlsConfig.tls_port || 12001}</strong> — remote access
                      </j-text>
                    </j-box>
                  </j-box>
                </>
              )}
            </>
          )}
        </>
      )}
    </div>
  );
};

export default Hosting;
