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
  const [versionInfo, setVersionInfo] = useState<{ installed: string | null; bundled: string; needsUpdate: boolean } | null>(null);
  const [reinstalling, setReinstalling] = useState(false);

  // Send form
  const [sendRecipient, setSendRecipient] = useState("");
  const [sendAmount, setSendAmount] = useState("");
  const [sendLoading, setSendLoading] = useState(false);
  const [sendResult, setSendResult] = useState<string | null>(null);

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
        const histStr = await client.runtime.unytWalletHistory(undefined, 20);
        if (histStr) {
          try {
            setHistory(JSON.parse(histStr));
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

  const handleSend = async () => {
    if (!client || !sendRecipient || !sendAmount) return;
    setSendLoading(true);
    setSendResult(null);
    try {
      const result = await client.runtime.unytSendHot(sendRecipient, sendAmount);
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

  if (loading) {
    return (
      <j-box px="500" py="500">
        <j-text size="500" color="ui-500">Loading wallet...</j-text>
      </j-box>
    );
  }

  return (
    <div>
      {error && (
        <j-box px="500" py="300">
          <j-text size="400" color="danger-500">{error}</j-text>
        </j-box>
      )}

      {/* Header */}
      <j-box px="500" my="500" pt="500" style={{ borderTop: "1px solid var(--j-color-ui-200)" }}>
        <j-flex a="center" j="between">
          <j-text size="600" weight="600" color="black">
            mHOT Wallet
          </j-text>
          <j-button size="sm" variant="subtle" onClick={fetchWalletData}>
            Refresh
          </j-button>
        </j-flex>
      </j-box>

      {/* Version Info & Reinstall */}
      {versionInfo && (
        <j-box px="500" my="300">
          <j-flex a="center" gap="300">
            <j-text size="400" color="ui-500">
              DNA: v{versionInfo.installed || "unknown"} {versionInfo.needsUpdate ? `→ v${versionInfo.bundled} available` : "(up to date)"}
            </j-text>
            {versionInfo.needsUpdate && (
              <j-button size="xs" variant="primary" onClick={handleReinstall} loading={reinstalling} disabled={reinstalling}>
                Reinstall
              </j-button>
            )}
            {!versionInfo.needsUpdate && (
              <j-button size="xs" variant="subtle" onClick={handleReinstall} loading={reinstalling} disabled={reinstalling}>
                Reinstall
              </j-button>
            )}
          </j-flex>
        </j-box>
      )}

      {/* Agent Pubkey */}
      {agentPubkey && (
        <j-box px="500" my="300">
          <j-text size="400" weight="500" color="ui-500">Your mHOT Agent Key</j-text>
          <j-flex a="center" gap="200">
            <j-text size="400" style={{ fontFamily: "monospace", wordBreak: "break-all" }}>
              {agentPubkey}
            </j-text>
            <j-button size="xs" variant="subtle" onClick={() => copyToClipboard(agentPubkey)}>
              Copy
            </j-button>
          </j-flex>
        </j-box>
      )}

      {/* Balance */}
      <j-box px="500" my="400">
        <j-text size="400" weight="500" color="ui-500">Balance</j-text>
        <j-box mt="200">
          {Object.entries(balance).length > 0 ? (
            Object.entries(balance).map(([unit, amount]) => (
              <j-text key={unit} size="700" weight="700" color="black">
                {amount} {unit}
              </j-text>
            ))
          ) : (
            <j-text size="500" color="ui-400">No balance data</j-text>
          )}
        </j-box>
      </j-box>

      {/* Send Form */}
      <j-box px="500" my="400" style={cardStyle}>
        <j-text size="500" weight="600" color="black">Send mHOT</j-text>
        <j-box mt="300">
          <j-box mb="200">
            <j-text size="400" weight="500">Recipient Agent Key</j-text>
          </j-box>
          <j-input
            value={sendRecipient}
            onInput={(e: any) => setSendRecipient(e.target.value)}
            placeholder="uhCAk..."
          />
        </j-box>
        <j-box mt="300">
          <j-box mb="200">
            <j-text size="400" weight="500">Amount (HOT)</j-text>
          </j-box>
          <j-input
            value={sendAmount}
            onInput={(e: any) => setSendAmount(e.target.value)}
            placeholder="100"
            type="number"
          />
        </j-box>
        <j-box mt="300">
          <j-button
            variant="primary"
            onClick={handleSend}
            loading={sendLoading}
            disabled={!sendRecipient || !sendAmount || sendLoading}
          >
            Send
          </j-button>
        </j-box>
        {sendResult && (
          <j-box mt="200">
            <j-text size="400" color={sendResult.startsWith("Error") ? "danger-500" : "success-500"}>
              {sendResult}
            </j-text>
          </j-box>
        )}
      </j-box>

      {/* Transaction History */}
      <j-box px="500" my="400">
        <j-text size="500" weight="600" color="black">Transaction History</j-text>
        <j-box mt="300">
          {Array.isArray(history) && history.length > 0 ? (
            history.map((tx: any, i: number) => (
              <j-box key={i} py="200" style={{ borderBottom: "1px solid var(--j-color-ui-100)" }}>
                <j-flex j="between" a="center">
                  <j-text size="400" weight="500">
                    {tx.tx_type || "Transaction"}
                  </j-text>
                  <j-text size="400" color="ui-500">
                    {tx.amount ? JSON.stringify(tx.amount) : ""}
                  </j-text>
                </j-flex>
                {tx.counterparty && (
                  <j-text size="300" color="ui-400" style={{ fontFamily: "monospace" }}>
                    {Array.isArray(tx.counterparty) ? tx.counterparty[0]?.substring(0, 20) + "..." : ""}
                  </j-text>
                )}
              </j-box>
            ))
          ) : (
            <j-text size="400" color="ui-400">No transactions yet</j-text>
          )}
        </j-box>
      </j-box>
    </div>
  );
};

export default Wallet;
