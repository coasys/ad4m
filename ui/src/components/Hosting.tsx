import React, { useCallback, useContext, useEffect, useRef, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { open as dialogOpen } from "@tauri-apps/plugin-dialog";
import { Ad4minContext } from "../context/Ad4minContext";
import { cardStyle, listStyle } from "./styles";
import Wallet, { HotLogo } from "./Wallet";
import type { UserStatistics } from "@coasys/ad4m";

type HostSession = {
  indexUrl: string;
  hostId: string;
  authToken: string;
  email: string;
} | null;

type RegStep = "credentials" | "verify" | "logged-in";
type MembraneProofStatus = "none" | "fetching" | "done" | "error";

// Uncontrolled price input that only commits value on blur/Enter
const PriceInput = ({ initialValue, placeholder, style, onCommit }: {
  initialValue: number;
  placeholder: string;
  style: React.CSSProperties;
  onCommit: (val: string) => void;
}) => {
  const [local, setLocal] = React.useState(initialValue != null ? String(initialValue) : "");
  const isEditing = React.useRef(false);
  // Update local when initialValue changes externally (but not while user is editing)
  React.useEffect(() => {
    if (!isEditing.current && initialValue != null) {
      setLocal(String(initialValue));
    }
  }, [initialValue]);
  return (
    <input
      type="text"
      inputMode="decimal"
      value={local}
      onFocus={() => { isEditing.current = true; }}
      onChange={(e) => setLocal(e.target.value)}
      onBlur={() => { isEditing.current = false; onCommit(local); }}
      onKeyDown={(e) => { if (e.key === "Enter") { (e.target as HTMLInputElement).blur(); } }}
      placeholder={placeholder}
      style={style}
    />
  );
};

// Extracted outside the component to avoid remounting on every parent re-render
const ExpandableSection = ({
  title,
  expanded,
  onToggle,
  children,
  badge,
}: {
  title: string;
  expanded: boolean;
  onToggle: () => void;
  children: React.ReactNode;
  badge?: React.ReactNode;
}) => (
  <div style={{ margin: "16px 0" }}>
    <div
      onClick={onToggle}
      style={{
        display: "flex",
        alignItems: "center",
        gap: "8px",
        cursor: "pointer",
        padding: "8px 0",
        userSelect: "none",
      }}
    >
      <j-icon
        name={expanded ? "chevron-down" : "chevron-right"}
        size="sm"
      ></j-icon>
      <span style={{ fontSize: "16px", fontWeight: 600, flex: 1 }}>
        {title}
      </span>
      {badge}
    </div>
    {expanded && (
      <div style={{ padding: "12px 0 0 24px" }}>{children}</div>
    )}
  </div>
);

const Hosting = () => {
  const {
    state: { client, multiUserEnabled, freeHostingEnabled },
    methods: { setMultiUserEnabled, setFreeHostingEnabled },
  } = useContext(Ad4minContext);

  // ---- Users state ----
  const [users, setUsers] = useState<UserStatistics[]>([]);
  const [usersLoading, setUsersLoading] = useState(true);
  const [creditAmounts, setCreditAmounts] = useState<Record<string, string>>(
    {},
  );
  const [actionLoading, setActionLoading] = useState<Record<string, boolean>>(
    {},
  );

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
    rates: '[{"description": "link write", "priceInHOT": 0.25}]',
    aiModels: "[]",
    computeSpecs: "",
    hostUrl: "",
  });
  const [hostRegStatus, setHostRegStatus] = useState<{
    type: "success" | "error" | "info";
    message: string;
  } | null>(null);
  const [hostRegistering, setHostRegistering] = useState(false);

  // Auto-clear status messages after 5 seconds
  useEffect(() => {
    if (!hostRegStatus) return;
    const timer = setTimeout(() => setHostRegStatus(null), 5000);
    return () => clearTimeout(timer);
  }, [hostRegStatus]);
  const [profilePic, setProfilePic] = useState<File | null>(null);
  const [profilePicUrl, setProfilePicUrl] = useState<string | null>(null);
  React.useEffect(() => {
    if (!profilePic) { setProfilePicUrl(null); return; }
    const url = URL.createObjectURL(profilePic);
    setProfilePicUrl(url);
    return () => URL.revokeObjectURL(url);
  }, [profilePic]);
  const [verificationCode, setVerificationCode] = useState("");
  const [membraneProofStatus, setMembraneProofStatus] =
    useState<MembraneProofStatus>("none");
  const [membraneProofError, setMembraneProofError] = useState<string | null>(
    null,
  );

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
  const [smtpTestVisible, setSmtpTestVisible] = useState(false);
  const [smtpTestLoading, setSmtpTestLoading] = useState(false);
  const [smtpTestStatus, setSmtpTestStatus] = useState("");
  const [smtpTestEmail, setSmtpTestEmail] = useState("");
  const [showSmtpPassword, setShowSmtpPassword] = useState(false);

  // ---- AI Models state ----
  const [aiModels, setAiModels] = useState<any[]>([]);

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
  const [tlsValidationErrors, setTlsValidationErrors] = useState<string[]>([]);

  // ---- UI state ----
  const [activeTab, setActiveTab] = useState<"users" | "settings">("settings");
  const [unytExpanded, setUnytExpanded] = useState(false);
  const [hostingProfileExpanded, setHostingProfileExpanded] = useState(true);
  const [tlsExpanded, setTlsExpanded] = useState(false);
  const [smtpExpanded, setSmtpExpanded] = useState(false);
  const [setupInfoExpanded, setSetupInfoExpanded] = useState(false);
  const prevMultiUserEnabled = useRef(multiUserEnabled);

  // Auto-expand setup info when multi-user is first enabled, then auto-collapse
  useEffect(() => {
    if (multiUserEnabled && !prevMultiUserEnabled.current) {
      setSetupInfoExpanded(true);
      const timer = setTimeout(() => setSetupInfoExpanded(false), 10000);
      return () => clearTimeout(timer);
    }
    prevMultiUserEnabled.current = multiUserEnabled;
  }, [multiUserEnabled]);

  // ---- Helpers ----

  const handleHostRegChange = (field: string, value: string) => {
    setHostReg((prev) => ({ ...prev, [field]: value }));
    setHostRegStatus(null);
  };

  const saveHostSession = async (session: HostSession) => {
    setHostSession(session);
    if (session) {
      try {
        await invoke("set_host_registration", {
          registration: {
            index_url: session.indexUrl,
            host_id: session.hostId,
            auth_token: session.authToken,
            email: session.email,
          },
        });
        console.log("Host session saved successfully");
      } catch (e) {
        console.error("Failed to save host session:", e);
      }
    } else {
      try {
        await invoke("set_host_registration", { registration: null });
        console.log("Host session cleared");
      } catch (e) {
        console.error("Failed to clear host session:", e);
      }
    }
  };

  // Helper to get full profile pic URL (handle relative paths)
  const getProfilePicUrl = (url: string | undefined) => {
    if (!url) return null;
    if (url.startsWith("http")) return url;
    // Prepend the hosting index URL for relative paths like /uploads/...
    return `${hostSession?.indexUrl || hostReg.indexUrl}${url}`;
  };

  // Returns "verified", "unverified", or "expired" (token invalid)
  const fetchHostData = async (session: {
    indexUrl: string;
    hostId: string;
    authToken: string;
  }): Promise<"verified" | "unverified" | "expired" | "unavailable"> => {
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
          name: mine.name !== undefined ? mine.name : prev.name,
          description: mine.description !== undefined ? mine.description : prev.description,
          location: mine.location !== undefined ? mine.location : prev.location,
          hostUrl: mine.url !== undefined ? mine.url : prev.hostUrl,
          rates: Array.isArray(mine.rates) ? JSON.stringify(mine.rates) : prev.rates,
          aiModels: Array.isArray(mine.aiModels) ? JSON.stringify(mine.aiModels) : prev.aiModels,
          computeSpecs: mine.computeSpecs !== undefined ? mine.computeSpecs : prev.computeSpecs,
        }));
        return mine.emailVerified ? "verified" : "unverified";
      }
      if (res.status === 401) {
        console.warn("Host session token expired or invalid, clearing session");
        return "expired";
      }
    } catch (e) {
      console.log("Failed to fetch host data:", e);
      return "unavailable";
    }
    return "unavailable";
  };

  // Track whether we've already attempted a membrane proof fetch this mount
  const membraneProofAttempted = React.useRef(false);

  const fetchMembraneProof = async (session: {
    indexUrl: string;
    hostId: string;
    authToken: string;
  }) => {
    if (!client) return;
    membraneProofAttempted.current = true;

    // Skip if we already have the proof or DNA is installed
    try {
      const vi = await client.runtime.unytVersionInfo();
      if (vi) {
        const info = JSON.parse(vi);
        if (info.installed) {
          console.log(
            "Unyt DNA already installed, skipping membrane proof fetch",
          );
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
      const res = await fetch(
        `${session.indexUrl}/hosts/${session.hostId}/request-membrane-proof`,
        {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            Authorization: `Bearer ${session.authToken}`,
          },
          body: JSON.stringify({ agent_key: agentKey }),
        },
      );

      if (!res.ok) {
        const data = await res
          .json()
          .catch(() => ({ error: `HTTP ${res.status}` }));

        // If the API says we need to re-verify email, switch to verify step
        if (data.code === "reverify_required") {
          console.log("Email verification expired, prompting re-verification");
          setRegStep("verify");
          setHostRegStatus({
            type: "info",
            message: "Email verification has expired. Please re-verify and click Resend to get a new code.",
          });
          setMembraneProofStatus("none");
          membraneProofAttempted.current = false;
          return;
        }

        const detail = data.detail ? ` [${data.detail}]` : "";
        throw new Error(
          (data.error || `Failed to get membrane proof (${res.status})`) + detail,
        );
      }

      const data = await res.json();
      const proofs: Record<string, string> = data.membrane_proofs || {};
      const proofKeys = Object.keys(proofs);

      if (proofKeys.length === 0) {
        throw new Error("No membrane proofs returned from joining service");
      }

      // 3. Store the first membrane proof (there's typically one role)
      const proof = proofs[proofKeys[0]];
      console.log(
        "Got membrane proof for role:",
        proofKeys[0],
        "length:",
        proof.length,
      );
      const result = await client.runtime.unytSetMembraneProof(proof);

      if (!result.success) {
        throw new Error(result.message || "Failed to store membrane proof");
      }

      console.log(
        "Membrane proof stored, Unyt DNA will be installed automatically",
      );
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
      setHostRegStatus({
        type: "error",
        message: "Email and password are required.",
      });
      return;
    }

    setHostRegistering(true);
    setHostRegStatus(null);

    try {
      const res = await fetch(`${hostReg.indexUrl}/hosts/register`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          email: hostReg.email,
          password: hostReg.password,
        }),
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
          message:
            "Registration successful! Check your email for a verification code.",
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
      setHostRegStatus({
        type: "error",
        message: "Please enter the verification code.",
      });
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
        setHostRegStatus({
          type: "success",
          message: "Email verified! Fetching auth material...",
        });
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
      setHostRegStatus({
        type: "error",
        message: "Not logged in. Please log in first.",
      });
      return;
    }
    setHostRegistering(true);
    setHostRegStatus(null);
    try {
      const res = await fetch(
        `${hostSession.indexUrl}/hosts/resend-verification`,
        {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
            Authorization: `Bearer ${hostSession.authToken}`,
          },
        },
      );
      const data = await res.json();
      if (res.ok) {
        setHostRegStatus({
          type: "success",
          message: "Verification code resent. Check your email.",
        });
      } else {
        setHostRegStatus({
          type: "error",
          message: data.error || `Resend failed (${res.status})`,
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

  const handleLogin = async () => {
    if (!hostReg.email || !hostReg.password) {
      setHostRegStatus({
        type: "error",
        message: "Email and password are required.",
      });
      return;
    }

    setHostRegistering(true);
    setHostRegStatus(null);

    try {
      const res = await fetch(`${hostReg.indexUrl}/hosts/login`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          email: hostReg.email,
          password: hostReg.password,
        }),
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
        const result = await fetchHostData(session);
        if (result === "verified") {
          setRegStep("logged-in");
          setHostRegStatus({
            type: "success",
            message: "Logged in successfully.",
          });
          if (membraneProofStatus !== "done") {
            fetchMembraneProof(session);
          }
        } else if (result === "unverified") {
          setRegStep("verify");
          setHostRegStatus({
            type: "info",
            message:
              "Email not yet verified. Check your inbox or resend the code.",
          });
        } else if (result === "unavailable") {
          setHostRegStatus({
            type: "error",
            message: "Could not reach the hosting service. Please try again later.",
          });
        } else {
          await saveHostSession(null);
          setHostRegStatus({
            type: "error",
            message: "Session expired. Please log in again.",
          });
          setRegStep("credentials");
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

      const res = await fetch(
        `${hostSession.indexUrl}/hosts/${hostSession.hostId}`,
        {
          method: "PUT",
          headers: { Authorization: `Bearer ${hostSession.authToken}` },
          body: formData,
        },
      );

      const data = await res.json();

      if (res.ok) {
        setHostData(data);
        // Also persist rates to executor DB for credit deduction
        try {
          if (client) await client.runtime.setHostRates(hostReg.rates);
          setHostRegStatus({
            type: "success",
            message: "Host updated successfully.",
          });
        } catch (e: any) {
          console.warn("Failed to sync rates to executor DB:", e);
          setHostRegStatus({
            type: "error",
            message: `Host updated but failed to sync rates to executor: ${e.message || e}`,
          });
        }
      } else {
        if (res.status === 401) {
          await saveHostSession(null);
          setHostData(null);
          setRegStep("credentials");
          setHostRegStatus({
            type: "error",
            message: "Session expired. Please log in again.",
          });
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
    setHostSession(null);
    setRegStep("credentials");
    setHostRegStatus(null);
    setHostReg((prev) => ({ ...prev, password: "" }));
    setMembraneProofStatus("none");
    membraneProofAttempted.current = false;
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
    setSmtpTestLoading(true);
    setSmtpTestStatus("");
    try {
      await invoke<void>("test_smtp_config", {
        config: smtpConfig,
        testEmail: smtpTestEmail,
      });
      setSmtpTestStatus("Test email sent successfully!");
    } catch (error) {
      setSmtpTestStatus("Failed: " + error);
    } finally {
      setSmtpTestLoading(false);
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
      // Re-validate after saving
      try {
        const errors = await invoke<string[]>("validate_tls_config");
        setTlsValidationErrors(errors);
      } catch (_) {
        // validation not available
      }
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
      const newConfig = { ...tlsConfig, cert_file_path: filePath.toString() };
      setTlsConfig(newConfig);
      await handleTlsConfigChange(newConfig);
    }
  };

  const handleKeyFilePicker = async () => {
    const filePath = await dialogOpen({
      title: "Select TLS Private Key File",
      filters: [{ name: "Private Key", extensions: ["pem", "key"] }],
    });
    if (filePath && tlsConfig) {
      const newConfig = { ...tlsConfig, key_file_path: filePath.toString() };
      setTlsConfig(newConfig);
      await handleTlsConfigChange(newConfig);
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

  const handleToggleFreeAccess = async (
    email: string,
    currentFreeAccess: boolean,
  ) => {
    const key = `free-${email}`;
    setActionLoading((prev) => ({ ...prev, [key]: true }));
    try {
      await client!.runtime.setUserFreeAccess(email, !currentFreeAccess);
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
      await client!.runtime.setUserCredits(email, amount);
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
    if (freeHostingEnabled) return;
    const loadHostSession = async () => {
      try {
        const reg = await invoke<{
          index_url: string;
          host_id: string;
          auth_token: string;
          email: string;
        } | null>("get_host_registration");
        if (reg) {
          const session = {
            indexUrl: reg.index_url,
            hostId: reg.host_id,
            authToken: reg.auth_token,
            email: reg.email,
          };
          setHostReg((prev) => ({
            ...prev,
            indexUrl: session.indexUrl,
            email: session.email,
          }));
          const result = await fetchHostData(session);
          if (result === "verified") {
            setHostSession(session);
            setRegStep("logged-in");
            // membrane proof fetch is handled by the useEffect below
          } else if (result === "unverified") {
            setHostSession(session);
            setRegStep("verify");
          } else if (result === "unavailable") {
            // Network/transient failure — keep session, let user retry
            setHostSession(session);
            setRegStep("logged-in");
          } else {
            // Token expired/invalid — clear stored session, go to login
            setHostSession(null);
            saveHostSession(null);
            setRegStep("credentials");
          }
        }
      } catch (e) {
        console.log("Failed to load host registration:", e);
      }
    };
    loadHostSession();
  }, [freeHostingEnabled]);

  // Auto-fetch membrane proof once when we have a session + client but no proof yet
  useEffect(() => {
    if (freeHostingEnabled) return;
    if (!client || !hostSession || membraneProofStatus === "done" || membraneProofStatus === "fetching" || membraneProofAttempted.current) return;
    fetchMembraneProof(hostSession);
  }, [client, hostSession, freeHostingEnabled]);

  // Auto-populate host URL from TLS domain + port
  useEffect(() => {
    if (!client) return;
    const fetchTlsDomain = async () => {
      try {
        const domain = await client.runtime.tlsDomain();
        if (domain) {
          const port = tlsConfig?.tls_port || 12001;
          const url = `wss://${domain}:${port}/graphql`;
          setHostReg((prev) =>
            prev.hostUrl ? prev : { ...prev, hostUrl: url },
          );
        }
      } catch (e) {
        // TLS not configured
      }
    };
    fetchTlsDomain();
  }, [client, tlsConfig]);

  useEffect(() => {
    if (!client) return;
    const fetchAiModels = async () => {
      try {
        const models = await client.ai.getModels();
        setAiModels(models);
        const modelNames = models.map((m: any) => m.name);
        setHostReg((prev) => ({
          ...prev,
          aiModels: JSON.stringify(modelNames),
        }));
      } catch (e) {
        console.error("Failed to fetch AI models:", e);
      }
    };
    fetchAiModels();

    // Load saved rates from executor DB
    const fetchHostRates = async () => {
      try {
        const rates = await client.runtime.getHostRates();
        if (rates.length > 0) {
          setHostReg((prev) => ({
            ...prev,
            rates: JSON.stringify(rates),
          }));
        }
      } catch (e) {
        console.error("Failed to fetch host rates from DB:", e);
      }
    };
    fetchHostRates();
  }, [client]);

  useEffect(() => {
    const loadTlsConfig = async () => {
      try {
        const config = await invoke<typeof tlsConfig>("get_tls_config");
        setTlsConfig(
          config || {
            enabled: false,
            cert_file_path: "",
            key_file_path: "",
            tls_port: 12001,
          },
        );
        // Validate TLS files after loading config
        try {
          const errors = await invoke<string[]>("validate_tls_config");
          setTlsValidationErrors(errors);
        } catch (_) {
          // validation command not available
        }
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
        setSmtpConfig(
          config || {
            enabled: true,
            host: "",
            port: 587,
            username: "",
            password: "",
            from_address: "",
          },
        );
      } catch (e) {
        console.error("Failed to load SMTP config:", e);
      }
    };
    loadSmtpConfig();
  }, []);

  // ==== RENDER ====

  // Status message as a render function (not a component) to avoid remounting
  const renderStatusMessage = () => {
    if (!hostRegStatus) return null;
    const bg =
      hostRegStatus.type === "success"
        ? "#e8f5e9"
        : hostRegStatus.type === "error"
          ? "#ffebee"
          : "#e7f3ff";
    const border =
      hostRegStatus.type === "success"
        ? "#4caf50"
        : hostRegStatus.type === "error"
          ? "#f44336"
          : "#2196f3";
    const icon =
      hostRegStatus.type === "success"
        ? "check-circle"
        : hostRegStatus.type === "error"
          ? "x-circle"
          : "info-circle";
    const color =
      hostRegStatus.type === "success"
        ? "success"
        : hostRegStatus.type === "error"
          ? "danger"
          : "primary";
    return (
      <div style={{ display: "flex", alignItems: "center", gap: "8px", margin: "12px 0" }}>
        <j-icon name={icon} color={color}></j-icon>
        <span style={{ fontSize: "14px", color: border }}>{hostRegStatus.message}</span>
      </div>
    );
  };

  // ExpandableSection moved outside component to prevent remounting

  return (
    <div>
      <style>{`
        .hosting-input::placeholder {
          color: var(--j-color-ui-300) !important;
          font-weight: 400 !important;
          opacity: 1;
        }
        textarea.hosting-input::placeholder {
          color: var(--j-color-ui-300) !important;
          font-weight: 400 !important;
          opacity: 1;
        }
      `}</style>
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
          <j-flex a="center" gap="300">
            <span>Enable multi-user hosting</span>
            {multiUserEnabled && (
              <button
                type="button"
                aria-expanded={setupInfoExpanded}
                aria-controls="setup-info-panel"
                style={{
                  display: "inline-flex",
                  alignItems: "center",
                  gap: "4px",
                  cursor: "pointer",
                  background: "none",
                  border: "none",
                  padding: 0,
                  font: "inherit",
                  color: "inherit",
                }}
                onClick={(e: React.MouseEvent) => {
                  e.preventDefault();
                  e.stopPropagation();
                  setSetupInfoExpanded(!setupInfoExpanded);
                }}
              >
                <j-icon
                  name={setupInfoExpanded ? "info-circle-fill" : "info-circle"}
                  color="ui-500"
                  size="sm"
                ></j-icon>
                <j-text size="300" color="ui-500" weight="500" nomargin>
                  Setup instructions
                </j-text>
                <j-icon
                  name={setupInfoExpanded ? "chevron-up" : "chevron-down"}
                  color="ui-500"
                  size="xs"
                ></j-icon>
              </button>
            )}
          </j-flex>
        </j-toggle>
      </j-box>

      {!multiUserEnabled && (
        <j-box px="500" my="300">
          <j-text size="500" color="ui-500">
            Enable this to host your AD4M node for other users.
          </j-text>
        </j-box>
      )}

      {multiUserEnabled && (
        <j-box px="500">
          <div
            id="setup-info-panel"
            style={{
              overflow: "hidden",
              maxHeight: setupInfoExpanded ? "600px" : "0",
              opacity: setupInfoExpanded ? 1 : 0,
              visibility: setupInfoExpanded ? "visible" as const : "hidden" as const,
              pointerEvents: setupInfoExpanded ? "auto" as const : "none" as const,
              transition: "max-height 0.4s ease, opacity 0.3s ease, visibility 0.3s ease",
              marginTop: setupInfoExpanded ? "8px" : "0",
            }}
          >
            <div
              style={{
                padding: "12px 16px",
                background: "var(--j-color-ui-50)",
                borderRadius: "8px",
                border: "1px solid var(--j-color-ui-200)",
              }}
            >
              <j-text size="400" color="ui-500">
                Allows remote users to sign up and log in via email and password.
              </j-text>
              <j-box mt="300">
                <j-text size="400" color="ui-500">
                  <strong>Email verification (optional):</strong> Configure an SMTP email service
                  in the Settings tab below to enable verification codes.
                  Without SMTP, users will sign in with just email and password.
                </j-text>
              </j-box>
              <j-box mt="300">
                <j-text size="400" color="ui-500">
                  <strong>TLS certificate (required for security):</strong> You should obtain a TLS
                  certificate for your domain, e.g. from{" "}
                  <a
                    href="https://letsencrypt.org"
                    target="_blank"
                    rel="noopener noreferrer"
                    style={{ color: "var(--j-color-primary-500)" }}
                  >
                    Let's Encrypt
                  </a>
                  . Then point to your certificate and key files in the{" "}
                  <strong>TLS Configuration</strong> section in the Settings tab.
                  TLS runs on a separate port which you can configure there.
                </j-text>
              </j-box>
              <j-box mt="300">
                <j-text size="400" color="ui-500">
                  <strong>Network access:</strong> Make sure the AD4M executor's TLS port is
                  publicly accessible — you may need to configure port forwarding on your
                  router or firewall so that remote users can connect.
                </j-text>
              </j-box>
            </div>
          </div>
        </j-box>
      )}

      {multiUserEnabled && (
        <j-box px="500" my="300">
          <j-toggle
            full=""
            checked={!freeHostingEnabled}
            onChange={async (e: any) => {
              try {
                await setFreeHostingEnabled(!e.target.checked);
              } catch (error) {
                console.error("Failed to toggle hosting mode:", error);
                e.target.checked = !e.target.checked;
              }
            }}
          >
            <span style={{ display: "inline-flex", alignItems: "center", gap: "6px" }}>
              Paid hosting via <HotLogo size={20} /> wHOT
            </span>
          </j-toggle>
          <j-box mt="200">
            <j-text size="400" color="ui-500">
              {freeHostingEnabled
                ? "All users have free access. No payment or wallet setup required."
                : "Users pay with wHOT credits to use this node. Configure your wallet below."}
            </j-text>
          </j-box>
        </j-box>
      )}

      {multiUserEnabled && (
        <>
          {/* ===== WALLET (Simple Framed) - only shown for paid hosting ===== */}
          {!freeHostingEnabled && (
            <j-box px="500" my="100">
              <div
                style={{
                  border: "1px solid var(--j-color-ui-200)",
                  borderRadius: "12px",
                  padding: "10px",
                  background: "var(--j-color-ui-50)",
                }}
              >
                <Wallet key="wallet" />
              </div>
            </j-box>
          )}

          {/* ===== UNYT DNA (Expandable) ===== */}
          {false && (
            <ExpandableSection
              title="Unyt DNA"
              expanded={unytExpanded}
              onToggle={() => setUnytExpanded(!unytExpanded)}
              badge={
                membraneProofStatus === "done" ? (
                  <j-badge variant="success">Connected</j-badge>
                ) : membraneProofStatus === "error" ? (
                  <j-badge variant="danger">Error</j-badge>
                ) : undefined
              }
            >
              <j-box mb="300">
                <j-text size="400" color="ui-500">
                  Required for receiving wHOT payments. The Unyt DNA enables
                  your AD4M instance to join the payment network.
                </j-text>
              </j-box>
              {membraneProofStatus === "fetching" && (
                <j-flex a="center" gap="300">
                  <j-spinner size="sm"></j-spinner>
                  <j-text size="400">Fetching auth material...</j-text>
                </j-flex>
              )}
              {membraneProofStatus === "done" && (
                <j-flex a="center" gap="300">
                  <j-icon name="check-circle" color="success"></j-icon>
                  <j-text size="400">Connected to Unyt DHT</j-text>
                  <j-button
                    size="sm"
                    variant="subtle"
                    onClick={() => fetchMembraneProof(hostSession!)}
                  >
                    Re-fetch
                  </j-button>
                </j-flex>
              )}
              {membraneProofStatus === "error" && (
                <j-flex a="center" gap="300" wrap="wrap">
                  <j-icon name="x-circle" color="danger"></j-icon>
                  <j-text size="400" color="danger">
                    {membraneProofError || "Failed to connect"}
                  </j-text>
                  <j-button
                    size="sm"
                    variant="primary"
                    onClick={() =>
                      hostSession && fetchMembraneProof(hostSession)
                    }
                  >
                    Retry
                  </j-button>
                </j-flex>
              )}
              {membraneProofStatus === "none" && hostSession && (
                <j-button
                  variant="primary"
                  onClick={() => fetchMembraneProof(hostSession)}
                >
                  Connect
                </j-button>
              )}
              {!hostSession && (
                <j-text size="400" color="ui-400">
                  Log in to hosting profile to connect
                </j-text>
              )}
            </ExpandableSection>
          )}

          {/* ===== TABS ===== */}
          <j-box px="500" mt="500">
            <j-tabs
              value={activeTab}
              onChange={(e: any) => setActiveTab(e.target.value)}
              variant="segment"
            >
              <j-tab-item value="settings">Settings</j-tab-item>
              <j-tab-item value="users">Users</j-tab-item>
            </j-tabs>
          </j-box>

          {/* ===== USERS TAB ===== */}
          {activeTab === "users" && (
            <div style={{ padding: "0 var(--j-space-500)", margin: "var(--j-space-400) 0" }}>
              {usersLoading ? (
                <j-flex gap="300" a="center">
                  <j-spinner size="sm"></j-spinner>
                  <j-text color="ui-500">Loading users...</j-text>
                </j-flex>
              ) : users.length === 0 ? (
                <j-text size="500" color="ui-400">
                  No users yet. Users will appear here when they connect.
                </j-text>
              ) : (
                <div style={{ ...listStyle, padding: "0 20px", marginTop: 0 }}>
                  <j-text
                    size="400"
                    color="ui-500"
                    style={{ marginBottom: "8px", display: "block" }}
                  >
                    {users.length} user{users.length !== 1 ? "s" : ""}{" "}
                    registered
                  </j-text>
                  {users.map((user, index) => (
                    <div
                      key={`user-${index}`}
                      style={{ ...cardStyle, width: "100%" }}
                    >
                      <j-flex gap="500" direction="column">
                        <j-flex gap="400" a="center">
                          <j-avatar size="lg" hash={user.email}></j-avatar>
                          <j-flex direction="column" gap="100">
                            <j-flex gap="300" a="center">
                              <j-text
                                nomargin
                                variant="heading-sm"
                                size="600"
                                weight="600"
                              >
                                {user.email}
                              </j-text>
                              {getStatusBadge(user.lastSeen)}
                              {(freeHostingEnabled || (user as any).freeAccess) && (
                                <j-badge variant="success">Free Access</j-badge>
                              )}
                            </j-flex>
                            <j-text
                              nomargin
                              size="300"
                              color="ui-500"
                              style={{
                                wordBreak: "break-all",
                                fontFamily: "monospace",
                              }}
                            >
                              {user.did || "DID not set"}
                            </j-text>
                          </j-flex>
                        </j-flex>

                        <j-flex gap="600">
                          <j-flex direction="column" gap="100">
                            <j-text
                              nomargin
                              size="300"
                              color="ui-500"
                              weight="500"
                            >
                              LAST SEEN
                            </j-text>
                            <j-text nomargin size="400">
                              {formatLastSeen(user.lastSeen)}
                            </j-text>
                          </j-flex>
                          <j-flex direction="column" gap="100">
                            <j-text
                              nomargin
                              size="300"
                              color="ui-500"
                              weight="500"
                            >
                              PERSPECTIVES
                            </j-text>
                            <j-text nomargin size="400">
                              {user.perspectiveCount}
                            </j-text>
                          </j-flex>
                          {!freeHostingEnabled && (
                            <j-flex direction="column" gap="100">
                              <j-text
                                nomargin
                                size="300"
                                color="ui-500"
                                weight="500"
                              >
                                CREDITS
                              </j-text>
                              <j-text nomargin size="400" weight="600">
                                {(user as any).remainingCredits === "unlimited"
                                  ? "Unlimited"
                                  : (user as any).remainingCredits || "0"}
                              </j-text>
                            </j-flex>
                          )}
                        </j-flex>

                        {!freeHostingEnabled && (
                        <j-box
                          style={{
                            borderTop: "1px solid var(--j-color-ui-200)",
                            paddingTop: "var(--j-space-400)",
                          }}
                        >
                          <j-flex gap="500" a="center" wrap="wrap">
                            <j-toggle
                              checked={(user as any).freeAccess}
                              disabled={actionLoading[`free-${user.email}`]}
                              onChange={() =>
                                handleToggleFreeAccess(
                                  user.email,
                                  (user as any).freeAccess,
                                )
                              }
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
                                  onInput={(e: any) =>
                                    setCreditAmounts((prev) => ({
                                      ...prev,
                                      [user.email]: e.target.value,
                                    }))
                                  }
                                ></j-input>
                                <j-button
                                  size="sm"
                                  variant="primary"
                                  disabled={
                                    actionLoading[`credits-${user.email}`] ||
                                    !creditAmounts[user.email] ||
                                    parseFloat(creditAmounts[user.email]) <= 0
                                  }
                                  onClick={() => handleSetCredits(user.email)}
                                >
                                  {actionLoading[`credits-${user.email}`]
                                    ? "Setting..."
                                    : "Set Credits"}
                                </j-button>
                              </j-flex>
                            )}
                          </j-flex>
                        </j-box>
                        )}
                      </j-flex>
                    </div>
                  ))}
                </div>
              )}
            </div>
          )}

          {/* ===== SETTINGS TAB ===== */}
          {activeTab === "settings" && (
            <div style={{ padding: "0 var(--j-space-500)", margin: "var(--j-space-400) 0" }}>
              {/* Hosting Profile - only shown for paid hosting */}
              {!freeHostingEnabled && (
              <ExpandableSection
                title="Hosting Profile"
                expanded={hostingProfileExpanded}
                onToggle={() =>
                  setHostingProfileExpanded(!hostingProfileExpanded)
                }
                badge={
                  hostSession ? (
                    <j-badge variant="success">{hostSession.email}</j-badge>
                  ) : (
                    <j-badge variant="gray">Not logged in</j-badge>
                  )
                }
              >
                {!hostSession ? (
                  // Login/Register form - nice card style
                  <div
                    style={{
                      padding: "20px",
                      background: "var(--j-color-ui-50)",
                      borderRadius: "12px",
                      border: "1px solid var(--j-color-ui-200)",
                    }}
                  >
                    <j-box mb="400">
                      <j-text size="400" color="ui-500">
                        Register to appear in ad4m-connect and receive wHOT
                        payments.
                      </j-text>
                    </j-box>

                    <j-box mb="300">
                      <j-text size="400" weight="500" mb="200">
                        Index API URL
                      </j-text>
                      <j-input
                        value={hostReg.indexUrl}
                        onInput={(e: any) =>
                          handleHostRegChange("indexUrl", e.target.value)
                        }
                        placeholder="https://hosting.ad4m.dev"
                      />
                    </j-box>

                    <j-box mb="300">
                      <j-text size="400" weight="500" mb="200">
                        Email
                      </j-text>
                      <j-input
                        value={hostReg.email}
                        onInput={(e: any) =>
                          handleHostRegChange("email", e.target.value)
                        }
                        placeholder="admin@example.com"
                      />
                    </j-box>

                    <j-box mb="400">
                      <j-text size="400" weight="500" mb="200">
                        Password
                      </j-text>
                      <j-input
                        type="password"
                        value={hostReg.password}
                        onInput={(e: any) =>
                          handleHostRegChange("password", e.target.value)
                        }
                        placeholder="Password"
                      />
                    </j-box>

                    {renderStatusMessage()}

                    <j-flex gap="300">
                      <j-button
                        variant="primary"
                        onClick={handleLogin}
                        loading={hostRegistering}
                        disabled={hostRegistering}
                      >
                        Login
                      </j-button>
                      <j-button
                        variant="subtle"
                        onClick={handleRegister}
                        loading={hostRegistering}
                        disabled={hostRegistering}
                      >
                        Register
                      </j-button>
                    </j-flex>
                  </div>
                ) : regStep === "verify" ? (
                  // Email verification
                  <j-box>
                    <j-box
                      mb="400"
                      p="300"
                      style={{ background: "#e7f3ff", borderRadius: "8px" }}
                    >
                      <j-text size="400">
                        Verification code sent to{" "}
                        <strong>{hostReg.email}</strong>
                      </j-text>
                    </j-box>
                    <j-box mb="300">
                      <j-text size="400" weight="500" mb="200">
                        Code
                      </j-text>
                      <j-input
                        value={verificationCode}
                        onInput={(e: any) =>
                          setVerificationCode(e.target.value)
                        }
                        placeholder="123456"
                      />
                    </j-box>
                    {renderStatusMessage()}
                    <j-flex gap="300">
                      <j-button
                        variant="primary"
                        onClick={handleVerifyEmail}
                        loading={hostRegistering}
                      >
                        Verify
                      </j-button>
                      <j-button
                        variant="subtle"
                        onClick={handleResendVerification}
                        disabled={hostRegistering}
                      >
                        Resend
                      </j-button>
                    </j-flex>
                  </j-box>
                ) : (
                  // Logged in - show profile in a nice card
                  <div
                    style={{
                      display: "flex",
                      flexDirection: "column",
                      alignItems: "center",
                      gap: "16px",
                      padding: "24px",
                      background: "var(--j-color-ui-50)",
                      borderRadius: "12px",
                      border: "1px solid var(--j-color-ui-200)",
                      position: "relative" as const,
                    }}
                  >
                    {/* Info tooltip */}
                    <div style={{ position: "absolute", top: "12px", right: "12px", zIndex: 1 }}>
                      <j-tooltip
                        title="This is your public hosting profile. Other users will see this information in AD4M Connect when choosing a host."
                        placement="left"
                      >
                        <j-icon
                          name="info-circle"
                          color="ui-400"
                          size="sm"
                          style={{ cursor: "help" }}
                        ></j-icon>
                      </j-tooltip>
                    </div>
                    {/* Profile picture */}
                    <label
                      style={{
                        cursor: "pointer",
                        position: "relative" as const,
                      }}
                    >
                      {hostData?.profilePicUrl && !profilePic ? (
                        <div style={{ position: "relative" as const }}>
                          <img
                            src={getProfilePicUrl(hostData.profilePicUrl)!}
                            alt=""
                            style={{
                              width: "80px",
                              height: "80px",
                              borderRadius: "50%",
                              objectFit: "cover",
                              background: "var(--j-color-ui-200)",
                            }}
                          />
                          <div
                            style={{
                              position: "absolute",
                              bottom: 0,
                              right: 0,
                              width: "24px",
                              height: "24px",
                              borderRadius: "50%",
                              background: "var(--j-color-primary-500)",
                              display: "flex",
                              alignItems: "center",
                              justifyContent: "center",
                            }}
                          >
                            <j-icon
                              name="pencil"
                              size="xs"
                              color="white"
                            ></j-icon>
                          </div>
                        </div>
                      ) : profilePicUrl ? (
                        <img
                          src={profilePicUrl}
                          alt=""
                          style={{
                            width: "80px",
                            height: "80px",
                            borderRadius: "50%",
                            objectFit: "cover",
                            background: "var(--j-color-ui-200)",
                          }}
                        />
                      ) : (
                        <div
                          style={{
                            width: "80px",
                            height: "80px",
                            borderRadius: "50%",
                            display: "flex",
                            alignItems: "center",
                            justifyContent: "center",
                            background: "var(--j-color-ui-200)",
                            border: "2px dashed var(--j-color-ui-400)",
                          }}
                        >
                          <j-icon
                            name="camera"
                            size="md"
                            color="ui-500"
                          ></j-icon>
                        </div>
                      )}
                      <input
                        type="file"
                        accept="image/*"
                        style={{ display: "none" }}
                        onChange={(e) => {
                          const f =
                            (e.target as HTMLInputElement).files?.[0] || null;
                          if (f && f.size > 2 * 1024 * 1024) {
                            setHostRegStatus({
                              type: "error",
                              message: "Max 2MB",
                            });
                            return;
                          }
                          setProfilePic(f);
                        }}
                      />
                    </label>

                    {/* Name */}
                    <input
                      className="hosting-input"
                      value={hostReg.name}
                      onChange={(e) =>
                        handleHostRegChange("name", e.target.value)
                      }
                      placeholder="Host Name"
                      style={{
                        fontSize: "22px",
                        fontWeight: 700,
                        textAlign: "center",
                        background: "transparent",
                        border: "none",
                        borderBottom: "1px dashed var(--j-color-ui-300)",
                        color: "var(--j-color-black)",
                        width: "100%",
                        padding: "4px 0",
                        outline: "none",
                      }}
                    />

                    {/* Location */}
                    <input
                      className="hosting-input"
                      value={hostReg.location}
                      onChange={(e) =>
                        handleHostRegChange("location", e.target.value)
                      }
                      placeholder="Location (e.g. Frankfurt, DE)"
                      style={{
                        fontSize: "16px",
                        textAlign: "center",
                        background: "transparent",
                        border: "none",
                        borderBottom: "1px dashed var(--j-color-ui-300)",
                        color: "var(--j-color-black)",
                        width: "100%",
                        padding: "2px 0",
                        outline: "none",
                      }}
                    />

                    {/* Description */}
                    <textarea
                      className="hosting-input"
                      value={hostReg.description}
                      onChange={(e) =>
                        handleHostRegChange("description", e.target.value)
                      }
                      placeholder="A brief description of your host"
                      rows={2}
                      style={{
                        fontSize: "16px",
                        textAlign: "center",
                        lineHeight: "1.4",
                        background: "transparent",
                        border: "none",
                        borderBottom: "1px dashed var(--j-color-ui-300)",
                        color: "var(--j-color-black)",
                        width: "100%",
                        padding: "2px 0",
                        outline: "none",
                        resize: "vertical",
                        fontFamily: "inherit",
                      }}
                    />

                    {/* Endpoint URL */}
                    <div
                      style={{
                        width: "100%",
                        padding: "12px 16px",
                        background: "var(--j-color-primary-50, rgba(33,150,243,0.05))",
                        borderRadius: "8px",
                        border: "1px solid var(--j-color-primary-200, rgba(33,150,243,0.2))",
                      }}
                    >
                      <j-text
                        size="400"
                        weight="600"
                        color="primary-500"
                        style={{
                          textTransform: "uppercase",
                          letterSpacing: "0.5px",
                          display: "block",
                          marginBottom: "4px",
                        }}
                      >
                        Endpoint URL
                      </j-text>
                      <j-text
                        size="400"
                        color="ui-500"
                        style={{ display: "block", marginBottom: "8px" }}
                      >
                        This is how remote users reach your node. It is auto-populated
                        from your TLS domain and port configuration. Make sure this
                        address is publicly reachable (check your router / firewall settings).
                      </j-text>
                      <input
                        className="hosting-input"
                        value={hostReg.hostUrl}
                        onChange={(e) =>
                          handleHostRegChange("hostUrl", e.target.value)
                        }
                        placeholder="wss://your-domain.com:12001/graphql"
                        style={{
                          fontSize: "15px",
                          width: "100%",
                          padding: "8px 12px",
                          background: "var(--j-color-ui-100)",
                          border: "1px solid var(--j-color-ui-200)",
                          borderRadius: "6px",
                          color: "var(--j-color-black)",
                          fontWeight: 500,
                          outline: "none",
                          fontFamily: "monospace",
                        }}
                      />
                    </div>

                    {/* Compute Specs */}
                    <div style={{ width: "100%" }}>
                      <j-text
                        size="400"
                        weight="600"
                        color="ui-500"
                        style={{
                          textTransform: "uppercase",
                          letterSpacing: "0.5px",
                          display: "block",
                          marginBottom: "6px",
                        }}
                      >
                        Compute
                      </j-text>
                      <input
                        className="hosting-input"
                        value={hostReg.computeSpecs}
                        onChange={(e) =>
                          handleHostRegChange("computeSpecs", e.target.value)
                        }
                        placeholder="e.g. 8 CPU, 32GB RAM, RTX 4090"
                        style={{
                          fontSize: "16px",
                          width: "100%",
                          padding: "8px 12px",
                          background: "var(--j-color-ui-100)",
                          border: "1px solid var(--j-color-ui-200)",
                          borderRadius: "6px",
                          color: "var(--j-color-black)",
                          fontWeight: 500,
                          outline: "none",
                          fontFamily: "inherit",
                        }}
                      />
                    </div>

                    {/* AI Models */}
                    <div style={{ width: "100%" }}>
                      <j-text
                        size="400"
                        weight="600"
                        color="ui-500"
                        style={{
                          textTransform: "uppercase",
                          letterSpacing: "0.5px",
                          display: "block",
                          marginBottom: "6px",
                        }}
                      >
                        AI Models
                      </j-text>
                      {aiModels.length > 0 ? (
                        <div
                          style={{
                            display: "flex",
                            flexWrap: "wrap",
                            justifyContent: "center",
                            gap: "6px",
                          }}
                        >
                          {aiModels.map((m: any, i: number) => (
                            <span
                              key={i}
                              style={{
                                fontSize: "15px",
                                padding: "4px 12px",
                                borderRadius: "12px",
                                background: "rgba(33, 150, 243, 0.1)",
                                color: "var(--j-color-primary-500)",
                              }}
                            >
                              {m.name}
                            </span>
                          ))}
                        </div>
                      ) : (
                        <j-text
                          size="400"
                          color="ui-400"
                          style={{ fontStyle: "italic" }}
                        >
                          No models configured
                        </j-text>
                      )}
                      <j-text
                        size="300"
                        color="ui-400"
                        style={{ display: "block", marginTop: "6px" }}
                      >
                        AI models can be added in the AI section.
                      </j-text>
                    </div>

                    {/* Pricing */}
                    <div style={{ width: "100%" }}>
                      <j-text
                        size="400"
                        weight="600"
                        color="ui-500"
                        style={{
                          textTransform: "uppercase",
                          letterSpacing: "0.5px",
                          display: "block",
                          marginBottom: "6px",
                        }}
                      >
                        Pricing
                      </j-text>
                      {(() => {
                        let parsed: { description: string; priceInHOT: number }[] = [];
                        try { parsed = JSON.parse(hostReg.rates); } catch {}
                        if (!Array.isArray(parsed)) parsed = [];

                        // Defaults in wHOT (at ~$0.0004/wHOT)
                        const DEFAULT_LINK_PRICE = 0.25;    // ~$0.0001 per link
                        const DEFAULT_TOKEN_PRICE = 12.5;   // ~$0.005 per token, avg API pricing

                        // Sync rates with current model list: add missing, remove stale
                        const modelNames: string[] = aiModels.map((m: any) => m.name);
                        const validKeys = new Set(["link write", ...modelNames]);
                        const has = (desc: string) => parsed.some((r) => r.description === desc);

                        // Migrate legacy "ModelName (per token)" entries to plain model names
                        const legacyPattern = /^(.+)\s*\(per token\)$/;
                        for (let i = parsed.length - 1; i >= 0; i--) {
                          const m = legacyPattern.exec(parsed[i].description);
                          if (m) {
                            const plainName = m[1].trim();
                            if (validKeys.has(plainName) && !has(plainName)) {
                              parsed[i] = { description: plainName, priceInHOT: parsed[i].priceInHOT };
                            } else {
                              parsed.splice(i, 1);
                            }
                          }
                        }

                        // Remove entries for deleted models
                        const before = parsed.length;
                        parsed = parsed.filter((r) => validKeys.has(r.description));

                        // Add defaults for new models
                        let changed = parsed.length !== before;
                        if (!has("link write")) { parsed.push({ description: "link write", priceInHOT: DEFAULT_LINK_PRICE }); changed = true; }
                        for (const name of modelNames) {
                          if (!parsed.some((r) => r.description === name)) { parsed.push({ description: name, priceInHOT: DEFAULT_TOKEN_PRICE }); changed = true; }
                        }
                        if (changed) {
                          // Schedule state update for next tick to avoid updating during render
                          setTimeout(() => handleHostRegChange("rates", JSON.stringify(parsed)), 0);
                        }

                        const getPrice = (desc: string) =>
                          parsed.find((r) => r.description === desc)?.priceInHOT ?? 0;

                        const getDefault = (desc: string) =>
                          desc === "link write" ? DEFAULT_LINK_PRICE : DEFAULT_TOKEN_PRICE;

                        const commitPrice = (desc: string, val: string) => {
                          const num = parseFloat(val);
                          const updated = parsed.filter((r) => r.description !== desc);
                          if (!isNaN(num) && num > 0) {
                            updated.push({ description: desc, priceInHOT: num });
                          } else {
                            // Reset to default if invalid
                            updated.push({ description: desc, priceInHOT: getDefault(desc) });
                          }
                          handleHostRegChange("rates", JSON.stringify(updated));
                        };

                        const inputStyle: React.CSSProperties = {
                          fontSize: "15px",
                          width: "110px",
                          padding: "6px 10px",
                          background: "var(--j-color-ui-100)",
                          border: "1px solid var(--j-color-ui-200)",
                          borderRadius: "6px",
                          color: "var(--j-color-ui-700)",
                          outline: "none",
                          fontFamily: "inherit",
                          textAlign: "right",
                        };

                        const rowStyle: React.CSSProperties = {
                          display: "flex",
                          alignItems: "center",
                          justifyContent: "space-between",
                          padding: "8px 12px",
                          borderBottom: "1px solid var(--j-color-ui-100)",
                        };

                        const labelStyle: React.CSSProperties = {
                          fontSize: "15px",
                          color: "var(--j-color-ui-600)",
                        };

                        return (
                          <div
                            style={{
                              border: "1px solid var(--j-color-ui-200)",
                              borderRadius: "8px",
                              overflow: "hidden",
                            }}
                          >
                            {/* Header */}
                            <div
                              style={{
                                ...rowStyle,
                                background: "var(--j-color-ui-50)",
                                fontWeight: 600,
                                fontSize: "14px",
                                textTransform: "uppercase",
                                letterSpacing: "0.5px",
                                color: "var(--j-color-ui-500)",
                              }}
                            >
                              <span>Item</span>
                              <span>Price (wHOT)</span>
                            </div>

                            {/* Base link price */}
                            <div style={rowStyle}>
                              <span style={labelStyle}>Link (base rate per link write)</span>
                              <PriceInput
                                initialValue={getPrice("link write")}
                                placeholder={String(getDefault("link write"))}
                                style={inputStyle}
                                onCommit={(val) => commitPrice("link write", val)}
                              />
                            </div>

                            {/* Per-model token prices */}
                            {modelNames.map((name) => (
                              <div key={name} style={rowStyle}>
                                <span style={labelStyle}>{name} <span style={{ color: "var(--j-color-ui-400)", fontSize: "14px" }}>(per token)</span></span>
                                <PriceInput
                                  initialValue={getPrice(name)}
                                  placeholder={String(getDefault(name))}
                                  style={inputStyle}
                                  onCommit={(val) => commitPrice(name, val)}
                                />
                              </div>
                            ))}

                            {modelNames.length === 0 && (
                              <div style={{ ...rowStyle, borderBottom: "none" }}>
                                <span style={{ ...labelStyle, fontStyle: "italic", color: "var(--j-color-ui-400)" }}>
                                  Add AI models above to set per-token prices
                                </span>
                              </div>
                            )}
                          </div>
                        );
                      })()}
                    </div>

                    {renderStatusMessage()}

                    <j-flex gap="300" mt="200">
                      <j-button
                        variant="primary"
                        onClick={handleUpdateHost}
                        loading={hostRegistering}
                      >
                        Save Changes
                      </j-button>
                      <j-button variant="subtle" onClick={handleLogout}>
                        Logout
                      </j-button>
                    </j-flex>
                  </div>
                )}
              </ExpandableSection>
              )}

              {/* TLS Settings */}
              <ExpandableSection
                title="TLS Configuration"
                expanded={tlsExpanded}
                onToggle={() => setTlsExpanded(!tlsExpanded)}
                badge={
                  tlsConfig?.enabled ? (
                    tlsValidationErrors.length > 0 ? (
                      <j-badge variant="danger">Error</j-badge>
                    ) : (
                      <j-badge variant="success">Enabled</j-badge>
                    )
                  ) : (
                    <j-badge variant="gray">Disabled</j-badge>
                  )
                }
              >
                {tlsConfig && (
                  <>
                    <j-toggle
                      checked={tlsConfig.enabled}
                      onChange={(e: any) => {
                        const newConfig = {
                          ...tlsConfig,
                          enabled: e.target.checked,
                        };
                        setTlsConfig(newConfig);
                        handleTlsConfigChange(newConfig);
                      }}
                    >
                      Enable TLS
                    </j-toggle>

                    {tlsConfig.enabled && (
                      <div style={{ marginTop: "16px" }}>
                        <div style={{ marginBottom: "12px" }}>
                          <j-text size="400" weight="500" mb="200">
                            Certificate File
                          </j-text>
                          <j-flex gap="200">
                            <j-input
                              value={tlsConfig.cert_file_path}
                              readonly
                            />
                            <j-button
                              variant="subtle"
                              onClick={handleCertFilePicker}
                            >
                              Browse
                            </j-button>
                          </j-flex>
                          {certPathError && (
                            <j-text size="300" color="danger">
                              {certPathError}
                            </j-text>
                          )}
                        </div>
                        <div style={{ marginBottom: "12px" }}>
                          <j-text size="400" weight="500" mb="200">
                            Private Key File
                          </j-text>
                          <j-flex gap="200">
                            <j-input value={tlsConfig.key_file_path} readonly />
                            <j-button
                              variant="subtle"
                              onClick={handleKeyFilePicker}
                            >
                              Browse
                            </j-button>
                          </j-flex>
                          {keyPathError && (
                            <j-text size="300" color="danger">
                              {keyPathError}
                            </j-text>
                          )}
                        </div>
                        <div style={{ marginBottom: "12px" }}>
                          <j-text size="400" weight="500" mb="200">
                            TLS Port
                          </j-text>
                          <j-input
                            type="number"
                            value={tlsConfig.tls_port?.toString() || "12001"}
                            onInput={(e: any) => {
                              const newConfig = {
                                ...tlsConfig,
                                tls_port: parseInt(e.target.value) || 12001,
                              };
                              setTlsConfig(newConfig);
                              handleTlsConfigChange(newConfig);
                            }}
                          />
                        </div>
                        {tlsValidationErrors.length > 0 && (
                          <div
                            style={{
                              marginTop: "12px",
                              padding: "12px",
                              backgroundColor: "var(--j-color-danger-50, #fef2f2)",
                              border: "1px solid var(--j-color-danger-200, #fecaca)",
                              borderRadius: "8px",
                            }}
                          >
                            <j-text size="400" weight="600" color="danger-500">
                              TLS Configuration Issues
                            </j-text>
                            {tlsValidationErrors.map((err, i) => (
                              <j-text key={i} size="300" color="danger-500" style={{ display: "block", marginTop: "4px" }}>
                                {err}
                              </j-text>
                            ))}
                          </div>
                        )}
                        {tlsValidationErrors.length === 0 && tlsConfig.cert_file_path && tlsConfig.key_file_path && (
                          <div
                            style={{
                              marginTop: "12px",
                              padding: "12px",
                              backgroundColor: "var(--j-color-success-50, #f0fdf4)",
                              border: "1px solid var(--j-color-success-200, #bbf7d0)",
                              borderRadius: "8px",
                            }}
                          >
                            <j-text size="400" color="success-500">
                              TLS files are valid and readable.
                            </j-text>
                          </div>
                        )}
                      </div>
                    )}
                  </>
                )}
              </ExpandableSection>

              {/* SMTP Settings */}
              <ExpandableSection
                title="Email Configuration (SMTP)"
                expanded={smtpExpanded}
                onToggle={() => setSmtpExpanded(!smtpExpanded)}
                badge={
                  smtpConfig?.enabled ? (
                    <j-badge variant="success">Enabled</j-badge>
                  ) : (
                    <j-badge variant="gray">Disabled</j-badge>
                  )
                }
              >
                {smtpConfig && (
                  <>
                    <j-toggle
                      checked={smtpConfig.enabled}
                      onChange={(e: any) => {
                        const newConfig = {
                          ...smtpConfig,
                          enabled: e.target.checked,
                        };
                        setSmtpConfig(newConfig);
                        handleSmtpConfigChange(newConfig);
                      }}
                    >
                      Enable Email/SMTP
                    </j-toggle>

                    {smtpConfig.enabled && (
                      <div style={{ marginTop: "16px" }}>
                        <div style={{ marginBottom: "12px" }}>
                          <j-text size="400" weight="500" mb="200">
                            SMTP Host
                          </j-text>
                          <j-input
                            value={smtpConfig.host}
                            onChange={(e: any) =>
                              setSmtpConfig({
                                ...smtpConfig,
                                host: e.target.value,
                              })
                            }
                            placeholder="smtp.gmail.com"
                          />
                        </div>
                        <div style={{ marginBottom: "12px" }}>
                          <j-text size="400" weight="500" mb="200">
                            Port
                          </j-text>
                          <j-input
                            type="number"
                            value={smtpConfig.port.toString()}
                            onChange={(e: any) =>
                              setSmtpConfig({
                                ...smtpConfig,
                                port: parseInt(e.target.value) || 587,
                              })
                            }
                          />
                        </div>
                        <div style={{ marginBottom: "12px" }}>
                          <j-text size="400" weight="500" mb="200">
                            Username
                          </j-text>
                          <j-input
                            value={smtpConfig.username}
                            onChange={(e: any) =>
                              setSmtpConfig({
                                ...smtpConfig,
                                username: e.target.value,
                              })
                            }
                          />
                        </div>
                        <div style={{ marginBottom: "12px" }}>
                          <j-text size="400" weight="500" mb="200">
                            Password
                          </j-text>
                          <j-input
                            type={showSmtpPassword ? "text" : "password"}
                            value={smtpConfig.password}
                            onChange={(e: any) =>
                              setSmtpConfig({
                                ...smtpConfig,
                                password: e.target.value,
                              })
                            }
                          >
                            <j-button
                              onClick={() =>
                                setShowSmtpPassword(!showSmtpPassword)
                              }
                              slot="end"
                              variant="link"
                              square
                            >
                              <j-icon
                                name={showSmtpPassword ? "eye-slash" : "eye"}
                                size="sm"
                              ></j-icon>
                            </j-button>
                          </j-input>
                        </div>
                        <div style={{ marginBottom: "12px" }}>
                          <j-text size="400" weight="500" mb="200">
                            From Address
                          </j-text>
                          <j-input
                            value={smtpConfig.from_address}
                            onChange={(e: any) =>
                              setSmtpConfig({
                                ...smtpConfig,
                                from_address: e.target.value,
                              })
                            }
                            placeholder="noreply@example.com"
                          />
                        </div>

                        <j-flex gap="300" mb="400">
                          <j-button
                            variant="primary"
                            onClick={() => handleSmtpConfigChange(smtpConfig)}
                          >
                            Save
                          </j-button>
                          <j-button
                            variant="subtle"
                            onClick={() => {
                              setSmtpTestEmail(hostSession?.email || "");
                              setSmtpTestVisible(true);
                            }}
                          >
                            Send Test
                          </j-button>
                        </j-flex>

                        {smtpTestVisible && (
                          <div style={{ marginBottom: "12px" }}>
                            <j-flex gap="200">
                              <j-input
                                value={smtpTestEmail}
                                onInput={(e: any) =>
                                  setSmtpTestEmail(e.target.value)
                                }
                                placeholder="test@example.com"
                              />
                              <j-button
                                variant="primary"
                                onClick={handleSmtpTest}
                                loading={smtpTestLoading}
                              >
                                Send
                              </j-button>
                            </j-flex>
                            {smtpTestStatus && (
                              <j-text size="300" mt="200">
                                {smtpTestStatus}
                              </j-text>
                            )}
                          </div>
                        )}
                      </div>
                    )}
                  </>
                )}
              </ExpandableSection>
            </div>
          )}
        </>
      )}
    </div>
  );
};

export default Hosting;
