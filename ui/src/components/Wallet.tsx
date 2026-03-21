import React, { useContext, useEffect, useState, useCallback } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import { cardStyle } from "./styles";

export const HotLogo = ({ size = 14 }: { size?: number }) => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    viewBox="0 0 1175.48 847.82"
    width={size}
    height={size * (847.82 / 1175.48)}
    style={{ display: "inline-block", verticalAlign: "middle" }}
  >
    <path
      fill="#007f88"
      d="M30.73.93C12.93,2.33-.77,5.13,0,7.23c7.1,16.9,51,116.4,51.6,117s10.8,1.3,22.8,1.8c53.2,2,95.3,12.4,140.8,34.9,32.7,16.1,56.9,33.4,82.1,58.5,37.1,37,61.3,78.1,75.5,128.4,1.6,5.8,2.9,11,2.9,11.8,0,1.1-8.1,1.3-43.9,1.3-24.2,0-45,.4-46.3.9-1.9.7-6.9,11.5-27.7,59.2-14,32.1-25.6,59.1-25.8,60-.7,2.6,7.7,4.9,28.7,8,4.3.7,30.1,1.4,60.6,1.8l53.1.6-3.2,11c-35.8,121.4-146.9,206.3-283,216.5-7.7.5-17.8,1-22.5,1h-8.5L31,780.23c-22.5,51.5-26,60.3-24.5,60.8,6.8,2.5,22,3.4,57.2,3.4,45,0,66.9-2,104-9.6,151.5-31.1,277.4-127.5,334.4-256.3,11-24.8,23.9-65.6,26.2-82.7l.7-4.9h113.7l.4,2.2c8.2,40.2,17.1,67.3,32.5,99.3,46.3,96.4,133.1,175.1,240.6,218.3a517.15,517.15,0,0,0,205.5,36.9c29.4-.9,52.8-4.2,51.6-7.3-1-2.6-50.8-116.9-51.1-117.2s-10.6-.7-23.1-1.2c-35.1-1.4-61.5-5.5-91.3-14-45.9-13.2-91.3-37.1-126.1-66.3-43.8-36.9-75.5-82.8-92.3-133.7-1.9-5.8-3.8-11.9-4.1-13.7l-.7-3.2,42.8-.3c23.5-.2,43.5-.7,44.3-1.1,1.3-.7,52.5-116.8,52.5-119.1,0-1.5-12.9-5.3-24.6-7.2-8.6-1.4-19.5-1.8-63-2.1l-52.7-.5,2-7.1c29.2-103.4,118.8-185.5,234.3-214.6,25.5-6.4,50.3-9.7,82.5-11,19.2-.8,21.6-1.1,22.8-2.7.8-1.1,12.6-27.6,26.2-58.9,16.7-38.3,24.4-57.2,23.7-57.7-2.2-1.2-16.2-3.7-27.2-4.8-16-1.7-62.7-1.4-83.3.4-117.8,10.5-224,58-303.6,135.6-48.6,47.4-82.7,100.6-103.7,162-5.4,15.5-11.9,39.9-13.9,51.7l-1.2,7.3H530.73l-.4-2.3c-.3-1.2-1.4-6.9-2.6-12.7-16.5-82-63.1-161.7-128.9-220.4C323.93,58.63,230.53,16.73,127.73,4,96.83.23,57-1.07,30.73.93Z"
    />
  </svg>
);

const CopyIcon = ({ size = 14 }: { size?: number }) => (
  <svg
    width={size}
    height={size}
    viewBox="0 0 24 24"
    fill="none"
    stroke="currentColor"
    strokeWidth="2"
    strokeLinecap="round"
    strokeLinejoin="round"
    style={{ display: "inline-block", verticalAlign: "middle" }}
  >
    <rect x="9" y="9" width="13" height="13" rx="2" ry="2" />
    <path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1" />
  </svg>
);

const Wallet = () => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [balance, setBalance] = useState<Record<string, string>>({});
  const [agentPubkey, setAgentPubkey] = useState<string>("");
  const [history, setHistory] = useState<any[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // Version info
  const [versionInfo, setVersionInfo] = useState<{
    installed: string | null;
    bundled: string;
    needsUpdate: boolean;
  } | null>(null);
  const [reinstalling, setReinstalling] = useState(false);

  // Send form
  const [sendRecipient, setSendRecipient] = useState("");
  const [sendAmount, setSendAmount] = useState("");
  const [sendLoading, setSendLoading] = useState(false);
  const [sendResult, setSendResult] = useState<string | null>(null);
  const [withdrawOpen, setWithdrawOpen] = useState(false);
  const [historyOpen, setHistoryOpen] = useState(false);
  const [confirmSend, setConfirmSend] = useState(false);

  // Contacts
  type Contact = { name: string; address: string };
  const CONTACTS_KEY = "wallet_contacts";
  const loadContacts = (): Contact[] => {
    try { return JSON.parse(localStorage.getItem(CONTACTS_KEY) || "[]"); } catch { return []; }
  };
  const [contacts, setContacts] = useState<Contact[]>(loadContacts);
  const [showContacts, setShowContacts] = useState(false);
  const [contactFilter, setContactFilter] = useState("");
  const [newContactName, setNewContactName] = useState("");
  const [newContactAddr, setNewContactAddr] = useState("");
  const [editingContact, setEditingContact] = useState<number | null>(null);

  const saveContacts = (c: Contact[]) => {
    setContacts(c);
    localStorage.setItem(CONTACTS_KEY, JSON.stringify(c));
  };
  const addContact = () => {
    if (!newContactName.trim() || !newContactAddr.trim()) return;
    saveContacts([...contacts, { name: newContactName.trim(), address: newContactAddr.trim() }]);
    setNewContactName("");
    setNewContactAddr("");
  };
  const removeContact = (idx: number) => {
    saveContacts(contacts.filter((_, i) => i !== idx));
  };
  const selectContact = (c: Contact) => {
    setSendRecipient(c.address);
    setShowContacts(false);
  };
  const saveAsContact = () => {
    if (!sendRecipient) return;
    setNewContactAddr(sendRecipient);
    setShowContacts(true);
  };

  // Registered users
  const [registeredUsers, setRegisteredUsers] = useState<{ email: string; did: string; walletAddress?: string }[]>([]);
  useEffect(() => {
    if (!client) return;
    client.runtime.listUsers().then((users: any[]) => {
      console.log("listUsers raw:", JSON.stringify(users));
      setRegisteredUsers(
        (users || []).map((u: any) => ({
          email: u.email,
          did: u.did,
          walletAddress: u.hotWalletAddress || undefined,
        }))
      );
    }).catch((e: any) => console.warn("Failed to fetch users:", e));
  }, [client]);

  const initialLoadDone = React.useRef(false);
  const fetchWalletData = useCallback(async () => {
    if (!client) return;
    try {
      if (!initialLoadDone.current) setLoading(true);
      setError(null);

      const errors: string[] = [];

      // Fetch balance
      try {
        const balStr = await client.runtime.unytWalletBalance();
        if (balStr) {
          try {
            setBalance(JSON.parse(balStr));
          } catch {
            setBalance({ HOT: balStr });
          }
        }
      } catch (e: any) {
        console.error("Failed to fetch balance:", e);
        errors.push(`Balance: ${e.message}`);
      }

      // Fetch pubkey
      try {
        const pk = await client.runtime.unytHotAgentPubkey();
        setAgentPubkey(pk || "");
      } catch (e: any) {
        console.error("Failed to fetch agent pubkey:", e);
        errors.push(`Pubkey: ${e.message}`);
      }

      // Fetch history
      try {
        const histStr = await client.runtime.unytWalletHistory(undefined, 50);
        console.log("Wallet history raw:", histStr);
        if (histStr) {
          try {
            const parsed = JSON.parse(histStr);
            console.log("Wallet history parsed:", parsed);
            setHistory(parsed);
          } catch {
            setHistory([]);
          }
        }
      } catch (e: any) {
        console.error("Failed to fetch history:", e);
        errors.push(`History: ${e.message}`);
      }

      // Fetch version info
      try {
        const viStr = await client.runtime.unytVersionInfo();
        if (viStr) {
          try {
            setVersionInfo(JSON.parse(viStr));
          } catch {}
        }
      } catch (e: any) {
        console.warn("Failed to fetch version info:", e.message);
      }

      if (errors.length > 0) {
        setError(errors.join("; "));
      }
    } catch (e: any) {
      setError(e.message || "Failed to load wallet data");
    } finally {
      setLoading(false);
      initialLoadDone.current = true;
    }
  }, [client]);

  useEffect(() => {
    fetchWalletData();
    const interval = setInterval(fetchWalletData, 30000);
    return () => clearInterval(interval);
  }, [fetchWalletData]);

  const handleSendClick = () => {
    if (!sendRecipient || !sendAmount) return;
    setConfirmSend(true);
  };

  // Resolve email to wallet address
  const resolveRecipient = (input: string): string => {
    if (input.includes("@")) {
      const user = registeredUsers.find(u => u.email.toLowerCase() === input.toLowerCase());
      if (user?.walletAddress) return user.walletAddress;
    }
    return input;
  };

  const handleSendConfirm = async () => {
    if (!client || !sendRecipient || !sendAmount) return;
    setConfirmSend(false);
    setSendLoading(true);
    setSendResult(null);
    try {
      const resolved = resolveRecipient(sendRecipient);
      if (sendRecipient.includes("@") && resolved === sendRecipient) {
        setSendResult("Error: No wallet address found for that email");
        setSendLoading(false);
        return;
      }
      const result = await client.runtime.unytSendHot(
        resolved,
        sendAmount,
      );
      setSendResult(result?.message || "Unknown result");
      if (result?.success) {
        setSendRecipient("");
        setSendAmount("");
        fetchWalletData();
      }
    } catch (e: any) {
      setSendResult(`Error: ${e.message}`);
    } finally {
      setSendLoading(false);
    }
  };

  const handleReinstall = async () => {
    if (!client) return;
    setReinstalling(true);
    try {
      const result = await client.runtime.unytReinstallDna();
      if (result?.success) {
        fetchWalletData();
      } else {
        setError(result?.message || "Reinstall failed");
      }
    } catch (e: any) {
      setError(`Reinstall error: ${e.message}`);
    } finally {
      setReinstalling(false);
    }
  };

  const copyToClipboard = (text: string) => {
    navigator.clipboard.writeText(text).catch(() => {});
  };

  // Always render the structure, just show loading state inline
  return (
    <div>
      {error && (
        <div style={{ padding: "0 20px", margin: "12px 0" }}>
          <j-text size="400" color="danger-500">
            {error}
          </j-text>
        </div>
      )}

      {/* Header + Address */}
      <div style={{ padding: "4px 20px", margin: "12px 0" }}>
        <j-flex a="center" j="between">
          <div style={{ display: "flex", alignItems: "baseline", gap: "4px" }}>
            <j-text size="800" weight="600" color="black">
              Earnings
            </j-text>
            {loading && <j-spinner size="sm"></j-spinner>}
            {agentPubkey && (
              <span
                onClick={() => copyToClipboard(agentPubkey)}
                title={agentPubkey}
                style={{
                  display: "inline-flex",
                  alignItems: "center",
                  gap: "4px",
                  cursor: "pointer",
                  fontFamily: "monospace",
                  fontSize: "12px",
                  color: "var(--j-color-ui-400)",
                  padding: "2px 4px",
                  borderRadius: "4px",
                  transition: "background 0.15s",
                }}
                onMouseEnter={(e) => (e.currentTarget.style.background = "var(--j-color-ui-100)")}
                onMouseLeave={(e) => (e.currentTarget.style.background = "transparent")}
              >
                {agentPubkey.substring(0, 8)}...{agentPubkey.substring(agentPubkey.length - 6)}
                <CopyIcon size={12} />
              </span>
            )}
          </div>
          <span
            onClick={fetchWalletData}
            style={{
              cursor: loading ? "default" : "pointer",
              opacity: loading ? 0.4 : 0.6,
              display: "inline-flex",
              alignItems: "center",
              padding: "4px",
              borderRadius: "4px",
              transition: "opacity 0.15s",
            }}
            onMouseEnter={(e) => { if (!loading) e.currentTarget.style.opacity = "1"; }}
            onMouseLeave={(e) => { if (!loading) e.currentTarget.style.opacity = "0.6"; }}
            title="Refresh"
          >
            <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
              <polyline points="23 4 23 10 17 10" />
              <polyline points="1 20 1 14 7 14" />
              <path d="M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15" />
            </svg>
          </span>
        </j-flex>
      </div>

      {/* Balance + Withdraw on same line */}
      <div style={{ padding: "0 20px", margin: "8px 0" }}>
        <div style={{ display: "flex", alignItems: "flex-end", justifyContent: "space-between" }}>
          <div style={{ display: "flex", alignItems: "baseline", gap: "8px" }}>
            <j-text size="400" weight="500" color="ui-500">
              Balance
            </j-text>
            {loading ? (
              <j-spinner size="sm"></j-spinner>
            ) : Object.entries(balance).length > 0 ? (
              Object.entries(balance).map(([unit, amount]) => (
                <span key={unit} style={{ display: "inline-flex", alignItems: "baseline", gap: "8px" }}>
                  <j-text size="700" weight="700" color="black">
                    {amount}
                  </j-text>
                  <span style={{ display: "inline-flex", alignItems: "baseline", gap: "3px", fontSize: "14px" }}>
                    <span style={{ opacity: 0.6 }}>mirrored</span> <HotLogo size={22} />
                  </span>
                </span>
              ))
            ) : (
              <j-text size="500" color="ui-400">
                No balance data
              </j-text>
            )}
          </div>
          <j-button
            size="sm"
            variant="subtle"
            onClick={() => setWithdrawOpen(!withdrawOpen)}
            style={{ display: "flex", alignItems: "center", gap: "6px" }}
          >
            <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" style={{ marginRight: "4px" }}>
              <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4" />
              <polyline points="7 10 12 15 17 10" />
              <line x1="12" y1="15" x2="12" y2="3" />
            </svg>
            Withdraw
            <svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.5" strokeLinecap="round" strokeLinejoin="round" style={{ transform: withdrawOpen ? "rotate(180deg)" : "rotate(0)", transition: "transform 0.2s" }}>
              <polyline points="6 9 12 15 18 9" />
            </svg>
          </j-button>
        </div>

        {withdrawOpen && (
          <div style={{ marginTop: "12px" }}>
            <div style={{ marginBottom: "8px" }}>
              <j-text size="400" weight="500">
                Amount (HOT)
              </j-text>
            </div>
            <j-flex a="center" gap="200">
              <j-input
                value={sendAmount}
                onInput={(e: any) => setSendAmount(e.target.value)}
                placeholder="100"
                type="number"
              />
              <j-button
                size="sm"
                variant="subtle"
                onClick={() => {
                  const bal = balance["0"] || Object.values(balance)[0];
                  if (bal) setSendAmount(bal);
                }}
              >
                Max
              </j-button>
            </j-flex>
            <div style={{ margin: "8px 0" }}>
              <j-text size="400" weight="500">
                To
              </j-text>
            </div>
            <div style={{ position: "relative" }}>
              <j-flex a="center" gap="200">
                <j-input
                  value={sendRecipient}
                  onInput={(e: any) => {
                    setSendRecipient(e.target.value);
                    setContactFilter(e.target.value);
                    if (e.target.value.length > 0) setShowContacts(true);
                  }}
                  placeholder="email or uhCAk..."
                  style={{ flex: 1 }}
                />
                <j-button
                  size="sm"
                  variant="subtle"
                  onClick={() => { setContactFilter(""); setShowContacts(!showContacts); }}
                  title="Contacts"
                >
                  <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round">
                    <path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" />
                    <circle cx="9" cy="7" r="4" />
                    <path d="M23 21v-2a4 4 0 0 0-3-3.87" />
                    <path d="M16 3.13a4 4 0 0 1 0 7.75" />
                  </svg>
                </j-button>
                <j-button
                  variant="primary"
                  onClick={handleSendClick}
                  loading={sendLoading}
                  disabled={!sendRecipient || !sendAmount || sendLoading}
                >
                  Send
                </j-button>
              </j-flex>

              {showContacts && (
                <div style={{
                  position: "absolute", top: "100%", left: 0, right: 0, zIndex: 9999,
                  marginTop: "4px", background: "var(--j-color-white)", border: "1px solid var(--j-color-ui-200)",
                  borderRadius: "8px", boxShadow: "0 4px 12px rgba(0,0,0,0.1)", maxHeight: "280px", overflow: "auto",
                }}>
                  {/* Registered users (filtered by input) */}
                  {(() => {
                    const q = contactFilter.toLowerCase();
                    const filtered = registeredUsers.filter(u =>
                      u.walletAddress && (!q || u.email.toLowerCase().includes(q) || u.walletAddress.toLowerCase().includes(q))
                    );
                    if (filtered.length === 0) return null;
                    return <>
                      <div style={{ padding: "6px 12px", borderBottom: "1px solid var(--j-color-ui-100)" }}>
                        <j-text size="300" color="ui-400" weight="600">Users</j-text>
                      </div>
                      {filtered.map((u, i) => (
                        <div
                          key={`user-${i}`}
                          onClick={async () => {
                            if (u.walletAddress) {
                              setSendRecipient(u.walletAddress);
                            } else {
                              // Resolve wallet address from email
                              setSendRecipient("resolving...");
                              try {
                                const addr = await client!.runtime.userWalletAddress(u.email);
                                setSendRecipient(addr || `no wallet for ${u.email}`);
                              } catch {
                                setSendRecipient(u.email);
                              }
                            }
                            setContactFilter("");
                            setShowContacts(false);
                          }}
                          style={{
                            padding: "8px 12px", cursor: "pointer",
                            display: "flex", justifyContent: "space-between", alignItems: "center",
                            borderBottom: "1px solid var(--j-color-ui-50)",
                          }}
                          onMouseEnter={(e) => (e.currentTarget.style.background = "var(--j-color-ui-50)")}
                          onMouseLeave={(e) => (e.currentTarget.style.background = "transparent")}
                        >
                          <j-text size="400">{u.email}</j-text>
                          <j-text size="300" color="ui-400" style={{ fontFamily: "monospace" }}>
                            {u.walletAddress ? u.walletAddress.substring(0, 8) + "..." : "no wallet"}
                          </j-text>
                        </div>
                      ))}
                    </>;
                  })()}

                  {/* Saved contacts (filtered) */}
                  {(() => {
                    const q = contactFilter.toLowerCase();
                    const filtered = contacts.filter(c =>
                      !q || c.name.toLowerCase().includes(q) || c.address.toLowerCase().includes(q)
                    );
                    if (filtered.length === 0) return null;
                    return <div style={{ padding: "6px 12px", borderBottom: "1px solid var(--j-color-ui-100)", borderTop: "1px solid var(--j-color-ui-200)" }}>
                      <j-text size="300" color="ui-400" weight="600">Contacts</j-text>
                    </div>;
                  })()}
                  {contacts.filter(c => {
                    const q = contactFilter.toLowerCase();
                    return !q || c.name.toLowerCase().includes(q) || c.address.toLowerCase().includes(q);
                  }).map((c, i) => (
                    <div
                      key={`contact-${i}`}
                      style={{
                        padding: "8px 12px", cursor: "pointer", display: "flex", justifyContent: "space-between", alignItems: "center",
                        borderBottom: "1px solid var(--j-color-ui-50)",
                      }}
                      onMouseEnter={(e) => (e.currentTarget.style.background = "var(--j-color-ui-50)")}
                      onMouseLeave={(e) => (e.currentTarget.style.background = "transparent")}
                    >
                      <div onClick={() => selectContact(c)} style={{ flex: 1 }}>
                        <j-text size="400" weight="500">{c.name}</j-text>
                        <j-text size="300" color="ui-400" style={{ fontFamily: "monospace" }}>
                          {c.address.substring(0, 12)}...
                        </j-text>
                      </div>
                      <span onClick={() => removeContact(i)} style={{ cursor: "pointer", opacity: 0.4, padding: "4px" }} title="Remove">✕</span>
                    </div>
                  ))}

                  {/* Add new contact */}
                  <div style={{ padding: "8px 12px", borderTop: "1px solid var(--j-color-ui-200)" }}>
                    <j-text size="300" color="ui-400" weight="600" style={{ marginBottom: "4px" }}>Add contact</j-text>
                    <div style={{ display: "flex", gap: "4px", marginTop: "4px" }}>
                      <j-input size="sm" value={newContactName} onInput={(e: any) => setNewContactName(e.target.value)} placeholder="Name" style={{ flex: 1 }} />
                      <j-input size="sm" value={newContactAddr} onInput={(e: any) => setNewContactAddr(e.target.value)} placeholder="Address" style={{ flex: 2 }} />
                      <j-button size="sm" variant="subtle" onClick={addContact} disabled={!newContactName.trim() || !newContactAddr.trim()}>+</j-button>
                    </div>
                    {sendRecipient && !contacts.some(c => c.address === sendRecipient) && (
                      <j-button size="sm" variant="subtle" onClick={saveAsContact} style={{ marginTop: "4px", width: "100%" }}>
                        Save current address as contact
                      </j-button>
                    )}
                  </div>
                </div>
              )}
            </div>

            {confirmSend && (
              <div style={{ marginTop: "12px", padding: "12px 16px", background: "var(--j-color-ui-50)", borderRadius: "8px", border: "1px solid var(--j-color-warning-300)" }}>
                <j-text size="400" weight="500">
                  Confirm: Send {sendAmount} <span style={{ display: "inline-flex", alignItems: "center", gap: "2px" }}><span style={{ fontSize: "0.75em", opacity: 0.6 }}>mirrored</span> <HotLogo size={16} /></span> to {sendRecipient.includes("@") ? sendRecipient : sendRecipient.substring(0, 12) + "..."}?
                </j-text>
                <j-flex gap="200" mt="200">
                  <j-button
                    variant="primary"
                    size="sm"
                    onClick={handleSendConfirm}
                  >
                    Confirm
                  </j-button>
                  <j-button
                    variant="subtle"
                    size="sm"
                    onClick={() => setConfirmSend(false)}
                  >
                    Cancel
                  </j-button>
                </j-flex>
              </div>
            )}

            {sendResult && (
              <div style={{ marginTop: "8px" }}>
                <j-text
                  size="400"
                  color={
                    sendResult.startsWith("Error") ? "danger-500" : "success-500"
                  }
                >
                  {sendResult}
                </j-text>
              </div>
            )}
          </div>
        )}
      </div>

      {/* Transaction History */}
      <div style={{ padding: "0 20px", margin: "16px 0" }}>
        <j-button
          size="sm"
          variant="subtle"
          onClick={() => setHistoryOpen(!historyOpen)}
          style={{ display: "flex", alignItems: "center", gap: "6px" }}
        >
          <svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" style={{ marginRight: "4px" }}>
            <circle cx="12" cy="12" r="10" />
            <polyline points="12 6 12 12 16 14" />
          </svg>
          Transaction History
          <svg width="12" height="12" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2.5" strokeLinecap="round" strokeLinejoin="round" style={{ transform: historyOpen ? "rotate(180deg)" : "rotate(0)", transition: "transform 0.2s" }}>
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </j-button>

        {historyOpen && (
          <div style={{ marginTop: "12px" }}>
            {(() => {
              if (!Array.isArray(history) || history.length === 0) {
                return (
                  <j-text size="400" color="ui-400">
                    No transactions yet
                  </j-text>
                );
              }

              // Collapse proposal flows: group by root proposal ID, show only final status
              // Each tx may have a nested "history" array tracing back to the root proposal
              const getRootId = (tx: any): string => {
                // Recursively walk history to find the deepest Proposal
                const findDeepest = (t: any): string | null => {
                  if (t.history && Array.isArray(t.history) && t.history.length > 0) {
                    for (const h of t.history) {
                      const deeper = findDeepest(h);
                      if (deeper) return deeper;
                    }
                    // Return the deepest item's id
                    return t.history[t.history.length - 1]?.id || null;
                  }
                  return null;
                };
                return findDeepest(tx) || tx.id || `unknown-${tx.timestamp}`;
              };

              // Priority: Reject > Accept/Receipt > Commitment > Proposal
              // Reject must be highest so it overwrites a completed send
              const typePriority: Record<string, number> = {
                Reject: 5, Accept: 4, Receipt: 4, Commitment: 2, Proposal: 1,
              };

              const grouped = new Map<string, any>();
              for (const tx of history) {
                const rootId = getRootId(tx);
                const existing = grouped.get(rootId);
                const txPrio = typePriority[tx.tx_type] ?? 0;
                const existPrio = existing ? (typePriority[existing.tx_type] ?? 0) : -1;
                if (!existing || txPrio > existPrio) {
                  grouped.set(rootId, tx);
                }
              }

              const collapsed = Array.from(grouped.values())
                .sort((a, b) => {
                  const ta = a.timestamp || new Date(a.created_at).getTime() / 1000;
                  const tb = b.timestamp || new Date(b.created_at).getTime() / 1000;
                  return (tb || 0) - (ta || 0);
                });

              return collapsed.map((tx: any, i: number) => {
                const isRejected = tx.status === "rejected" || tx.direction === "rejected" || tx.tx_type === "Reject";
                const isCompleted = tx.tx_type === "Accept" || tx.tx_type === "Receipt";
                const isPending = tx.status === "pending" || tx.tx_type === "Proposal" || tx.tx_type === "Commitment";

                // Determine direction from amount sign (negative = sent, positive = received)
                const amountObj = tx.amount;
                let rawAmount = "";
                if (typeof amountObj === "object" && amountObj !== null && !Array.isArray(amountObj)) {
                  rawAmount = amountObj["0"] || Object.values(amountObj)[0] as string || "";
                } else if (typeof amountObj === "string") {
                  rawAmount = amountObj;
                }
                const numAmount = parseFloat(rawAmount) || 0;
                const isSend = numAmount < 0;
                const isIncoming = numAmount > 0;

                const counterpartyKey = tx.counterparty
                  ? (Array.isArray(tx.counterparty) ? tx.counterparty[0] : tx.counterparty)
                  : null;
                const counterparty = tx.counterparty_email || counterpartyKey;
                const absAmount = Math.abs(numAmount);
                const amountStr = absAmount ? String(absAmount) : "";

                const timestamp = tx.timestamp || tx.created_at;
                // Zome timestamps are microseconds
                const dateStr = timestamp
                  ? new Date(typeof timestamp === "number"
                    ? (timestamp > 1e15 ? timestamp / 1000 : timestamp > 1e12 ? timestamp : timestamp * 1000)
                    : timestamp
                  ).toLocaleString()
                  : null;

                let label: string;
                let statusColor: string;
                let amountColor: string;
                if (isRejected) {
                  label = "✕ Rejected";
                  statusColor = "danger-500";
                  amountColor = "danger-500";
                } else if (isCompleted && isSend) {
                  label = "↑ Sent";
                  statusColor = "black";
                  amountColor = "ui-500";
                } else if (isCompleted && isIncoming) {
                  label = "↓ Received";
                  statusColor = "success-500";
                  amountColor = "success-500";
                } else if (isPending && isSend) {
                  label = "↑ Sending";
                  statusColor = "warning-500";
                  amountColor = "warning-500";
                } else if (isPending && isIncoming) {
                  label = "↓ Pending approval";
                  statusColor = "warning-500";
                  amountColor = "warning-500";
                } else if (isIncoming) {
                  label = "↓ Received";
                  statusColor = "success-500";
                  amountColor = "success-500";
                } else {
                  label = "↑ Sent";
                  statusColor = "black";
                  amountColor = "ui-500";
                }

                return (
                  <div
                    key={i}
                    style={{ padding: "8px 0", borderBottom: "1px solid var(--j-color-ui-100)", opacity: isRejected ? 0.6 : 1 }}
                  >
                    <j-flex j="between" a="center">
                      <j-flex a="center" gap="200">
                        <j-text size="400" weight="500" color={statusColor}>
                          {label}
                        </j-text>
                        {dateStr && (
                          <j-text size="300" color="ui-400">
                            {dateStr}
                          </j-text>
                        )}
                      </j-flex>
                      {amountStr && (
                        <j-text size="400" weight="500" color={amountColor}>
                          {isIncoming && !isRejected ? "+" : isSend ? "-" : ""}{amountStr} <span style={{ display: "inline-flex", alignItems: "center", gap: "2px" }}><span style={{ fontSize: "0.75em", opacity: 0.6 }}>m</span><HotLogo size={14} /></span>
                        </j-text>
                      )}
                    </j-flex>
                    {counterparty && (
                      <j-text
                        size="300"
                        color="ui-400"
                        style={{ fontFamily: "monospace" }}
                      >
                        {isIncoming ? "From: " : "To: "}{counterparty.length > 24 ? counterparty.substring(0, 24) + "..." : counterparty}
                      </j-text>
                    )}
                  </div>
                );
              });
            })()}
          </div>
        )}
      </div>
    </div>
  );
};

export default Wallet;
