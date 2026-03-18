import { useContext, useEffect, useState, useCallback } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import { cardStyle } from "./styles";

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

  const fetchWalletData = useCallback(async () => {
    if (!client) return;
    try {
      setLoading(true);
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

  const handleSendConfirm = async () => {
    if (!client || !sendRecipient || !sendAmount) return;
    setConfirmSend(false);
    setSendLoading(true);
    setSendResult(null);
    try {
      const result = await client.runtime.unytSendHot(
        sendRecipient,
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
        <j-box px="500" py="300">
          <j-text size="400" color="danger-500">
            {error}
          </j-text>
        </j-box>
      )}

      {/* Header */}
      <j-box
        px="500"
        my="300"
        pt="100"
        style={{ borderTop: "1px solid var(--j-color-ui-200)" }}
      >
        <j-flex a="center" j="between">
          <j-flex a="center" gap="300">
            <j-text size="800" weight="600" color="black">
              Earnings
            </j-text>
            {loading && <j-spinner size="sm"></j-spinner>}
          </j-flex>
          <j-button
            size="sm"
            variant="subtle"
            onClick={fetchWalletData}
            disabled={loading}
          >
            Refresh
          </j-button>
        </j-flex>
      </j-box>

      {/* Agent Pubkey */}
      {agentPubkey && (
        <j-box px="500" my="300">
          <j-flex a="center" gap="200">
            <j-text size="400" weight="500" color="ui-500">
              Your address:
            </j-text>
            <j-text
              size="400"
              style={{ fontFamily: "monospace", wordBreak: "break-all" }}
            >
              {agentPubkey}
            </j-text>
            <j-button
              size="xs"
              variant="subtle"
              onClick={() => copyToClipboard(agentPubkey)}
            >
              Copy
            </j-button>
          </j-flex>
        </j-box>
      )}
      {/* Version Info & Reinstall */}
      {false && versionInfo && (
        <j-box px="500" my="300">
          <j-flex a="center" gap="300">
            <j-text size="400" color="ui-500">
              DNA: v{versionInfo.installed || "unknown"}{" "}
              {versionInfo.needsUpdate
                ? `→ v${versionInfo.bundled} available`
                : "(up to date)"}
            </j-text>
            {versionInfo.needsUpdate && (
              <j-button
                size="xs"
                variant="primary"
                onClick={handleReinstall}
                loading={reinstalling}
                disabled={reinstalling}
              >
                Reinstall
              </j-button>
            )}
            {!versionInfo.needsUpdate && (
              <j-button
                size="xs"
                variant="subtle"
                onClick={handleReinstall}
                loading={reinstalling}
                disabled={reinstalling}
              >
                Reinstall
              </j-button>
            )}
          </j-flex>
        </j-box>
      )}

      {/* Balance + Withdraw on same line */}
      <j-box px="500" my="200">
        <j-flex a="center" j="between">
          <j-flex a="center" gap="200">
            <j-text size="400" weight="500" color="ui-500">
              Balance
            </j-text>
            {loading ? (
              <j-spinner size="sm"></j-spinner>
            ) : Object.entries(balance).length > 0 ? (
              Object.entries(balance).map(([unit, amount]) => (
                <j-text key={unit} size="700" weight="700" color="black">
                  {amount} {unit === "0" ? "mHOT" : `Unit ${unit}`}
                </j-text>
              ))
            ) : (
              <j-text size="500" color="ui-400">
                No balance data
              </j-text>
            )}
          </j-flex>
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
        </j-flex>

        {withdrawOpen && (
          <j-box mt="300">
            <j-box mb="200">
              <j-text size="400" weight="500">
                Amount (HOT)
              </j-text>
            </j-box>
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
            <j-box mt="200" mb="200">
              <j-text size="400" weight="500">
                To
              </j-text>
            </j-box>
            <j-flex a="center" gap="200">
              <j-input
                value={sendRecipient}
                onInput={(e: any) => setSendRecipient(e.target.value)}
                placeholder="uhCAk..."
                style={{ flex: 1 }}
              />
              <j-button
                variant="primary"
                onClick={handleSendClick}
                loading={sendLoading}
                disabled={!sendRecipient || !sendAmount || sendLoading}
              >
                Send
              </j-button>
            </j-flex>

            {confirmSend && (
              <j-box mt="300" py="300" px="400" style={{ background: "var(--j-color-ui-50)", borderRadius: "8px", border: "1px solid var(--j-color-warning-300)" }}>
                <j-text size="400" weight="500">
                  Confirm: Send {sendAmount} mHOT to {sendRecipient.substring(0, 12)}...?
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
              </j-box>
            )}

            {sendResult && (
              <j-box mt="200">
                <j-text
                  size="400"
                  color={
                    sendResult.startsWith("Error") ? "danger-500" : "success-500"
                  }
                >
                  {sendResult}
                </j-text>
              </j-box>
            )}
          </j-box>
        )}
      </j-box>

      {/* Transaction History */}
      <j-box px="500" my="400">
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
          <j-box mt="300">
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
                  <j-box
                    key={i}
                    py="200"
                    style={{ borderBottom: "1px solid var(--j-color-ui-100)", opacity: isRejected ? 0.6 : 1 }}
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
                          {isIncoming && !isRejected ? "+" : isSend ? "-" : ""}{amountStr} mHOT
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
                  </j-box>
                );
              });
            })()}
          </j-box>
        )}
      </j-box>
    </div>
  );
};

export default Wallet;
