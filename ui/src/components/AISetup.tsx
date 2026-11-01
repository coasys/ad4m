import {
  AI_RECOMMENDED_MODELS,
  AI_SERVER_INSTALL_LINKS,
  type Ad4mClient,
  type DetectedEndpoint,
  type DiscoveredModel,
  type EndpointKind,
} from "@coasys/ad4m";
import { open } from "@tauri-apps/plugin-shell";
import { useCallback, useEffect, useState } from "react";
import "../index.css";
import { endpointFor, kindLabel, registerModel, serves } from "./aiSetup";

type Mode = "Detected" | "Remote" | "Skip";

const REMOTE_KINDS: { kind: EndpointKind; label: string; baseUrl: string }[] = [
  { kind: "OPEN_AI", label: "OpenAI", baseUrl: "https://api.openai.com/v1" },
  { kind: "ANTHROPIC", label: "Anthropic", baseUrl: "https://api.anthropic.com" },
  { kind: "OPEN_AI_COMPATIBLE", label: "Other", baseUrl: "https://" },
];

/**
 * Onboarding's AI page: use a model server detected on this machine, a remote API, or no
 * AI. The executor runs no models itself; nothing here downloads one except an Ollama
 * pull the person asks for.
 */
export default function AISetup(props: {
  client: Ad4mClient;
  /** Whether the page has what it needs to go on (a chat model, or skip). */
  onReady: (ready: boolean) => void;
}) {
  const { client, onReady } = props;
  const [mode, setMode] = useState<Mode>("Detected");
  const [detected, setDetected] = useState<DetectedEndpoint[] | null>(null);
  const [detecting, setDetecting] = useState(false);
  const [busy, setBusy] = useState<string | null>(null);
  const [error, setError] = useState("");
  const [registered, setRegistered] = useState<string[]>([]);

  const [remoteKind, setRemoteKind] = useState(REMOTE_KINDS[0]);
  const [remoteUrl, setRemoteUrl] = useState(REMOTE_KINDS[0].baseUrl);
  const [remoteKey, setRemoteKey] = useState("");
  const [remoteModels, setRemoteModels] = useState<DiscoveredModel[]>([]);
  const [remoteEndpointId, setRemoteEndpointId] = useState<string | null>(null);

  const detect = useCallback(async () => {
    setDetecting(true);
    setError("");
    try {
      setDetected(await client.ai.detectEndpoints());
    } catch (e) {
      setError(`${e}`);
      setDetected([]);
    }
    setDetecting(false);
  }, [client]);

  useEffect(() => {
    detect();
  }, [detect]);

  useEffect(() => {
    onReady(mode === "Skip" || registered.some((r) => r.startsWith("LLM:")));
  }, [mode, registered, onReady]);

  /** Save the detected server as an endpoint and register `remoteModel` on it. */
  async function adopt(server: DetectedEndpoint, modelType: "LLM" | "EMBEDDING" | "TRANSCRIPTION", remoteModel: string) {
    setBusy(`${modelType}:${remoteModel}`);
    setError("");
    try {
      const endpoint = await endpointFor(client, server.kind, server.baseUrl);
      if (server.kind === "OLLAMA" && !serves(server.servedModels, remoteModel)) {
        await client.ai.pullModel(endpoint.id, remoteModel);
      }
      await registerModel(client, endpoint, modelType, remoteModel);
      setRegistered((r) => [...r, `${modelType}:${remoteModel}`]);
      await detect();
    } catch (e) {
      setError(`${e}`);
    }
    setBusy(null);
  }

  async function connectRemote() {
    setBusy("remote");
    setError("");
    setRemoteModels([]);
    try {
      const endpoint = await endpointFor(client, remoteKind.kind, remoteUrl, remoteKey);
      if (endpoint.health.state !== "REACHABLE") {
        throw new Error(endpoint.health.detail || `The API answers ${endpoint.health.state}`);
      }
      setRemoteEndpointId(endpoint.id);
      const models = await client.ai.discoverModels({ endpointId: endpoint.id });
      setRemoteModels(models.filter((m) => m.capabilities.includes("chat")));
    } catch (e) {
      setError(`${e}`);
    }
    setBusy(null);
  }

  async function chooseRemoteModel(remoteModel: string) {
    const endpoint = (await client.ai.endpoints()).find((e) => e.id === remoteEndpointId);
    if (!endpoint) return;
    setBusy(`LLM:${remoteModel}`);
    try {
      await registerModel(client, endpoint, "LLM", remoteModel);
      setRegistered((r) => [...r, `LLM:${remoteModel}`]);
    } catch (e) {
      setError(`${e}`);
    }
    setBusy(null);
  }

  const offers = (server: DetectedEndpoint) =>
    AI_RECOMMENDED_MODELS.filter((m) =>
      server.kind === "OLLAMA" ? !!m.ollama : server.kind === "OPEN_AI_AUDIO" && !!m.openAiAudio
    ).map((m) => ({
      ...m,
      remote: (server.kind === "OLLAMA" ? m.ollama : m.openAiAudio) as string,
    }));

  return (
    <j-flex direction="column" a="center" gap="500" style={{ width: "100%", maxWidth: 640 }}>
      <j-flex gap="400" style={{ padding: "0 10px" }}>
        {(
          [
            ["Detected", "house-fill", "A model server on this computer: Ollama, llama-server, speaches."],
            ["Remote", "broadcast-pin", "An API such as OpenAI or Anthropic, with your key."],
            ["Skip", "x-lg", "No AI for now. Add a model later in Settings, AI."],
          ] as [Mode, string, string][]
        ).map(([m, icon, text]) => (
          <button key={m} className={`card ${mode === m && "selected"}`} onClick={() => setMode(m)}>
            <j-icon name={icon} size="lg" color="ui-500" />
            <j-text size="600" color="ui-0" nomargin style={{ marginBottom: 5 }}>
              {m === "Detected" ? "This computer" : m === "Remote" ? "Remote API" : "Skip"}
            </j-text>
            <j-text size="500" nomargin color="ui-800">
              {text}
            </j-text>
          </button>
        ))}
      </j-flex>

      {mode === "Detected" && (
        <j-flex direction="column" gap="400" style={{ width: "100%" }}>
          {detected === null || detecting ? (
            <j-text>Looking for model servers on this computer...</j-text>
          ) : detected.length === 0 ? (
            <j-flex direction="column" gap="300">
              <j-text>
                No model server answers on this computer. Install one, start it, then check
                again:
              </j-text>
              <j-text>
                <a style={{ cursor: "pointer" }} onClick={() => open(AI_SERVER_INSTALL_LINKS.ollama)}>
                  Ollama
                </a>{" "}
                for chat and embeddings,{" "}
                <a style={{ cursor: "pointer" }} onClick={() => open(AI_SERVER_INSTALL_LINKS.speaches)}>
                  speaches
                </a>{" "}
                for transcription.
              </j-text>
            </j-flex>
          ) : (
            detected.map((server) => (
              <j-box key={server.baseUrl} className="box">
                <j-flex direction="column" gap="300">
                  <j-text nomargin color="ui-0">
                    {kindLabel(server.kind)} at {server.baseUrl}
                  </j-text>
                  {offers(server).map((m) => {
                    const done = registered.includes(`${m.modelType}:${m.remote}`);
                    const has = serves(server.servedModels, m.remote);
                    return (
                      <j-flex key={m.remote} a="center" j="between" gap="300">
                        <j-text nomargin size="400">
                          {m.description}
                        </j-text>
                        <j-button
                          size="sm"
                          variant={done ? "subtle" : "primary"}
                          disabled={done || !!busy}
                          loading={busy === `${m.modelType}:${m.remote}`}
                          onClick={() => adopt(server, m.modelType as any, m.remote)}
                        >
                          {done ? "Added" : has ? "Use" : server.kind === "OLLAMA" ? "Download and use" : "Use"}
                        </j-button>
                      </j-flex>
                    );
                  })}
                </j-flex>
              </j-box>
            ))
          )}
          <j-button variant="subtle" onClick={detect} loading={detecting}>
            Check again
          </j-button>
        </j-flex>
      )}

      {mode === "Remote" && (
        <j-flex direction="column" gap="400" style={{ width: "100%", maxWidth: 420 }}>
          <j-flex gap="300">
            {REMOTE_KINDS.map((k) => (
              <j-button
                key={k.kind}
                variant={remoteKind.kind === k.kind ? "primary" : "subtle"}
                onClick={() => {
                  setRemoteKind(k);
                  setRemoteUrl(k.baseUrl);
                  setRemoteKey("");
                  setRemoteModels([]);
                }}
              >
                {k.label}
              </j-button>
            ))}
          </j-flex>
          <j-input
            size="md"
            label="API URL"
            value={remoteUrl}
            onInput={(e: any) => setRemoteUrl(e.target.value)}
          />
          <j-input
            size="md"
            type="password"
            label="API key"
            value={remoteKey}
            onInput={(e: any) => setRemoteKey(e.target.value)}
          />
          <j-button variant="primary" onClick={connectRemote} loading={busy === "remote"}>
            Connect
          </j-button>
          {remoteModels.length > 0 && (
            <j-flex direction="column" gap="200">
              <j-text nomargin>Pick the chat model:</j-text>
              {remoteModels.map((m) => (
                <j-button
                  key={m.id}
                  size="sm"
                  variant={registered.includes(`LLM:${m.id}`) ? "primary" : "subtle"}
                  loading={busy === `LLM:${m.id}`}
                  onClick={() => chooseRemoteModel(m.id)}
                >
                  {m.id}
                </j-button>
              ))}
            </j-flex>
          )}
        </j-flex>
      )}

      {mode === "Skip" && (
        <j-text style={{ maxWidth: 420 }}>
          Without a chat model, AI features (interpretation, summaries, agents) stay off until
          you add one in Settings, AI.
        </j-text>
      )}

      {error && (
        <j-text color="danger-500" style={{ maxWidth: 520 }}>
          {error}
        </j-text>
      )}
    </j-flex>
  );
}
