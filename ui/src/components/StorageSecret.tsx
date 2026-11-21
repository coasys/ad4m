import { invoke } from "@tauri-apps/api/core";
import { open as dialogOpen, ask as dialogAsk } from "@tauri-apps/plugin-dialog";
import { useContext, useEffect, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";

const SECRET_RE = /^[0-9a-fA-F]{64}$/;

const StorageSecret = () => {
  const {
    state: { client },
    methods: { unlockStorage, finishStorageUnlock },
  } = useContext(Ad4minContext);

  const [secret, setSecret] = useState("");
  const [error, setError] = useState("");
  const [busy, setBusy] = useState(false);
  const [waiting, setWaiting] = useState(false);

  const fail = (e: any) => {
    setError(typeof e === "string" ? e : e?.message || String(e));
    setBusy(false);
    setWaiting(false);
  };

  // The screen comes up for every gated state. `sealed` asks for the secret below;
  // `unlocking`/`migrating` mean the unlock already ran (the app restarted mid-move,
  // say), so there is nothing to enter — just wait for the stores to open.
  useEffect(() => {
    if (!client) return;
    let live = true;
    client.runtime
      .storageStatus()
      .then((status) => {
        if (!live || status.state === "sealed") return;
        setWaiting(true);
        setBusy(true);
        finishStorageUnlock().catch((e) => {
          if (live) fail(e);
        });
      })
      .catch(() => {});
    return () => {
      live = false;
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [client]);

  const unlock = async (value: string) => {
    const trimmed = value.trim();
    if (!SECRET_RE.test(trimmed)) {
      setError("The secret is 64 hexadecimal characters.");
      return;
    }
    setBusy(true);
    setError("");
    try {
      // Keep it in the OS keyring so the next start finds it; a backend that cannot store
      // it (e.g. Linux without a secret service) still unlocks for this start.
      try {
        await invoke("storage_secret_set", { secret: trimmed });
      } catch (e) {
        console.warn("Could not keep the secret in the OS keyring:", e);
      }
      await unlockStorage(trimmed);
    } catch (e) {
      fail(e);
    }
  };

  const restoreFromFile = async () => {
    const path = await dialogOpen({ multiple: false, directory: false });
    if (!path) return;
    setBusy(true);
    setError("");
    try {
      const restored = await invoke<string>("storage_secret_restore_file", {
        path,
      });
      await unlockStorage(restored);
    } catch (e) {
      fail(e);
    }
  };

  const startOver = async () => {
    const yes = await dialogAsk(
      "This deletes the launcher's data folder — the sealed data included — and restarts with a new agent. The sealed data cannot be recovered without the storage secret. Continue?",
      { title: "Reset storage", kind: "warning" }
    );
    if (yes) await invoke("clear_state");
  };

  if (waiting)
    return (
      <j-box px="800" py="800">
        <j-flex direction="column" gap="500" a="center">
          <j-text variant="heading" nomargin>
            Opening storage
          </j-text>
          <j-text>
            The sealed data is being opened. This can take a while on a first start.
          </j-text>
          <j-spinner size="lg"></j-spinner>
          {error && (
            <j-text color="danger-500" nomargin>
              {error}
            </j-text>
          )}
        </j-flex>
      </j-box>
    );

  return (
    <j-box px="800" py="800">
      <j-flex direction="column" gap="500">
        <j-text variant="heading" nomargin>
          Storage secret missing
        </j-text>
        <j-text>
          This device's data is sealed with a storage secret, and the system's keyring no
          longer holds it. Enter the secret to unlock the data, or restore it from a backup
          file.
        </j-text>
        <j-input
          label="Storage secret (64 hex characters)"
          value={secret}
          disabled={busy}
          onInput={(e: any) => setSecret(e.target.value)}
        ></j-input>
        {error && (
          <j-text color="danger-500" nomargin>
            {error}
          </j-text>
        )}
        <j-flex gap="300">
          <j-button
            variant="primary"
            loading={busy}
            disabled={busy || secret.trim().length === 0}
            onClick={() => unlock(secret)}
          >
            Enter secret
          </j-button>
          <j-button disabled={busy} onClick={restoreFromFile}>
            Restore from file
          </j-button>
        </j-flex>
        <j-box py="300"></j-box>
        <j-text variant="label" nomargin>
          Last resort
        </j-text>
        <j-text size="400" nomargin>
          If the secret is lost for good, the sealed data cannot be recovered. Reset and
          start with a new agent.
        </j-text>
        <j-flex>
          <j-button variant="ghost" disabled={busy} onClick={startOver}>
            Reset storage
          </j-button>
        </j-flex>
      </j-flex>
    </j-box>
  );
};

export default StorageSecret;
