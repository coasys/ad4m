import {
  AI_RECOMMENDED_MODELS,
  AI_SERVER_INSTALL_LINKS,
  type Ad4mClient,
  type DetectedEndpoint,
  type DiscoveredModel,
  type EndpointKind,
  type LocalAiHardware,
  type LocalAiPlan,
  type LocalAiProgress,
  type LocalAiStatus,
} from "@coasys/ad4m";
import { open } from "@tauri-apps/plugin-shell";
import { useCallback, useEffect, useState } from "react";
import "../index.css";
import {
  chatModels,
  endpointFor,
  kindLabel,
  preselectedChat,
  registerModel,
  serves,
} from "./aiSetup";

type Mode = "Local" | "Server" | "Skip";
type ServerMode = "Detected" | "Remote";

/** The chat model chosen on a detected server: registered when the person goes on. */
type ChatChoice = { server: DetectedEndpoint; remoteModel: string };

const REMOTE_KINDS: { kind: EndpointKind; label: string; baseUrl: string }[] = [
  { kind: "OPEN_AI", label: "OpenAI", baseUrl: "https://api.openai.com/v1" },
  { kind: "ANTHROPIC", label: "Anthropic", baseUrl: "https://api.anthropic.com" },
  { kind: "OPEN_AI_COMPATIBLE", label: "Other", baseUrl: "https://" },
];

const SERVICE_LABELS: Record<string, string> = {
  llm: "Chat",
  embeddings: "Embeddings",
  stt: "Transcription",
  tts: "Speech",
};

function gb(bytes: number): string {
  return bytes >= 1e9 ? `${(bytes / 1e9).toFixed(1)} GB` : `${Math.max(1, Math.round(bytes / 1e6))} MB`;
}

/** One line for the detected device: CPU, RAM, the GPU a runtime would use. */
function describeDevice(hardware: LocalAiHardware): string {
  const gpu =
    hardware.gpus.find((g) => g.type === "discrete") ?? hardware.gpus[0];
  const gpuText = gpu
    ? `${gpu.name}${gpu.type === "discrete" && gpu.vramBytes ? ` (${gb(gpu.vramBytes)})` : gpu.type === "unified" ? " (unified memory)" : " (integrated)"}`
    : "no GPU";
  const backends = hardware.backends.filter((b) => b !== "cpu");
  return `${hardware.cpu.brand || hardware.arch}, ${gb(hardware.ramBytes)} RAM, ${gpuText}${backends.length ? `, ${backends.join(" + ")}` : ""}`;
}

/**
 * Onboarding's AI page. First choice: local AI on this device: the executor detects the
 * hardware, picks models that fit, downloads llama.cpp and nemo-speech with the models,
 * runs them and registers them. Else a model server (one already on this computer, or a
 * remote API with a key), or no AI.
 */
export default function AISetup(props: {
  client: Ad4mClient;
  /**
   * `null` while the page lacks what it needs to go on (a chat model, or skip); else what
   * "Next" runs first: it registers the chat model chosen on a detected server and makes
   * it the default, and answers whether that worked.
   */
  onReady: (finish: (() => Promise<boolean>) | null) => void;
}) {
  const { client, onReady } = props;
  const [mode, setMode] = useState<Mode>("Local");
  const [serverMode, setServerMode] = useState<ServerMode>("Detected");
  const [busy, setBusy] = useState<string | null>(null);
  const [error, setError] = useState("");
  const [registered, setRegistered] = useState<string[]>([]);
  /** The chat models each detected server already serves, by base URL. */
  const [servedChat, setServedChat] = useState<Record<string, string[]>>({});
  const [chatChoice, setChatChoice] = useState<ChatChoice | null>(null);

  // Local AI
  const [hardware, setHardware] = useState<LocalAiHardware | null>(null);
  const [plan, setPlan] = useState<LocalAiPlan | null>(null);
  const [localStatus, setLocalStatus] = useState<LocalAiStatus | null>(null);
  const [progress, setProgress] = useState<LocalAiProgress | null>(null);
  const [installing, setInstalling] = useState(false);

  // A model server
  const [detected, setDetected] = useState<DetectedEndpoint[] | null>(null);
  const [detecting, setDetecting] = useState(false);
  const [remoteKind, setRemoteKind] = useState(REMOTE_KINDS[0]);
  const [remoteUrl, setRemoteUrl] = useState(REMOTE_KINDS[0].baseUrl);
  const [remoteKey, setRemoteKey] = useState("");
  const [remoteModels, setRemoteModels] = useState<DiscoveredModel[]>([]);
  const [remoteEndpointId, setRemoteEndpointId] = useState<string | null>(null);

  const loadLocal = useCallback(async () => {
    try {
      const [h, p, s] = await Promise.all([
        client.ai.localHardware(),
        client.ai.localPlan(),
        client.ai.localStatus(),
      ]);
      setHardware(h);
      setPlan(p);
      setLocalStatus(s);
    } catch (e) {
      setError(`${e}`);
    }
  }, [client]);

  const detect = useCallback(async () => {
    setDetecting(true);
    setError("");
    try {
      const servers = await client.ai.detectEndpoints();
      const chat: Record<string, string[]> = {};
      await Promise.all(
        servers
          .filter((s) => s.capabilities.includes("chat"))
          .map(async (s) => {
            try {
              chat[s.baseUrl] = chatModels(
                await client.ai.discoverModels({ kind: s.kind, baseUrl: s.baseUrl })
              );
            } catch (e) {
              // The server still offers the recommended download.
              chat[s.baseUrl] = [];
              setError(`Could not list the models of ${s.baseUrl}: ${e}`);
            }
          })
      );
      setServedChat(chat);
      setDetected(servers);
      // A choice the person made, or one made before, stays.
      setChatChoice((chosen) => chosen ?? preselectedChat(servers, chat));
    } catch (e) {
      setError(`${e}`);
      setDetected([]);
    }
    setDetecting(false);
  }, [client]);

  useEffect(() => {
    loadLocal();
  }, [loadLocal]);

  const hasLlm = registered.some((r) => r.startsWith("LLM:"));
  useEffect(() => {
    if (mode === "Server" && detected === null) detect();
  }, [mode, detected, detect]);

  const localChatReady = !!localStatus?.models.some((m) => m.service === "llm" && m.model);

  useEffect(() => {
    if (mode === "Server" && serverMode === "Detected" && chatChoice) {
      onReady(() => commitChatChoice(chatChoice));
    } else {
      const ready =
        mode === "Skip" || (mode === "Local" && localChatReady) || (mode === "Server" && hasLlm);
      onReady(ready ? async () => true : null);
    }
    // commitChatChoice reads only its argument, `client` and stable setters.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [mode, serverMode, chatChoice, hasLlm, localChatReady, onReady]);

  /** Register the chosen chat model on its server and make it the default chat model. */
  async function commitChatChoice({ server, remoteModel }: ChatChoice): Promise<boolean> {
    setBusy(`LLM:${remoteModel}`);
    setError("");
    try {
      const endpoint = await endpointFor(client, server.kind, server.baseUrl);
      const id = await registerModel(client, endpoint, "LLM", remoteModel);
      await client.ai.setDefaultModel("LLM", id);
      return true;
    } catch (e) {
      setError(`${e}`);
      return false;
    } finally {
      setBusy(null);
    }
  }

  async function installLocal() {
    setInstalling(true);
    setError("");
    setProgress(null);
    try {
      const status = await client.ai.localInstall({}, (p) => setProgress(p));
      setLocalStatus(status);
      setPlan(await client.ai.localPlan());
    } catch (e) {
      setError(`${e}`);
    }
    setInstalling(false);
  }

  async function startLocal() {
    setBusy("local-start");
    setError("");
    try {
      setLocalStatus(await client.ai.localStart());
    } catch (e) {
      setError(`${e}`);
    }
    setBusy(null);
  }

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
      if (modelType === "LLM") setChatChoice({ server, remoteModel });
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

  // A recommended chat model the server already serves is listed with its served models.
  const offers = (server: DetectedEndpoint) =>
    AI_RECOMMENDED_MODELS.filter((m) =>
      server.kind === "OLLAMA" ? !!m.ollama : server.kind === "OPEN_AI_AUDIO" && !!m.openAiAudio
    )
      .map((m) => ({
        ...m,
        remote: (server.kind === "OLLAMA" ? m.ollama : m.openAiAudio) as string,
      }))
      .filter(
        (m) => m.modelType !== "LLM" || !serves(servedChat[server.baseUrl] ?? [], m.remote)
      );

  const isChosen = (server: DetectedEndpoint, remoteModel: string) =>
    chatChoice?.server.baseUrl === server.baseUrl && chatChoice.remoteModel === remoteModel;

  const running = localStatus?.runtimes.filter((r) => r.phase === "running") ?? [];
  const localUp = !!localStatus?.installed && running.length === localStatus.runtimes.length && running.length > 0;

  return (
    <j-flex direction="column" a="center" gap="500" style={{ width: "100%", maxWidth: 640 }}>
      <j-flex gap="400" style={{ padding: "0 10px" }}>
        {(
          [
            ["Local", "cpu", "Set up local AI (recommended for this device): runs on this computer, nothing leaves it."],
            ["Server", "hdd-network", "Use a model server: one on this computer (Ollama, llama-server, speaches) or an API such as OpenAI, with your key."],
            ["Skip", "x-lg", "No AI for now. Add a model later in Settings, AI."],
          ] as [Mode, string, string][]
        ).map(([m, icon, text]) => (
          <button key={m} className={`card ${mode === m && "selected"}`} onClick={() => setMode(m)}>
            <j-icon name={icon} size="lg" color="ui-500" />
            <j-text size="600" color="ui-0" nomargin style={{ marginBottom: 5 }}>
              {m === "Local" ? "This device" : m === "Server" ? "A model server" : "Skip"}
            </j-text>
            <j-text size="500" nomargin color="ui-800">
              {text}
            </j-text>
          </button>
        ))}
      </j-flex>

      {mode === "Local" && (
        <j-flex direction="column" gap="400" style={{ width: "100%" }}>
          {!hardware || !plan ? (
            <j-text>Checking this device...</j-text>
          ) : (
            <j-box className="box">
              <j-flex direction="column" gap="300">
                <j-text nomargin color="ui-0">
                  {describeDevice(hardware)}
                </j-text>
                <j-text nomargin size="400" color="ui-700">
                  Class {plan.hardwareClass}; the models may use {gb(plan.memoryBudgetBytes)} of memory.
                </j-text>
                {plan.models.map((m) => (
                  <j-flex key={m.modelId} a="center" j="between" gap="300">
                    <j-text nomargin size="400">
                      {SERVICE_LABELS[m.service] ?? m.service}: {m.modelId}
                    </j-text>
                    <j-text nomargin size="400" color="ui-700">
                      {m.installed ? "downloaded" : gb(m.sizeBytes)}
                    </j-text>
                  </j-flex>
                ))}
                {plan.runtimes.map((r) => (
                  <j-flex key={r.runtimeId} a="center" j="between" gap="300">
                    <j-text nomargin size="400">
                      Runtime: {r.runtimeId} {r.version} ({r.backend})
                    </j-text>
                    <j-text nomargin size="400" color="ui-700">
                      {r.installed ? "downloaded" : gb(r.sizeBytes)}
                    </j-text>
                  </j-flex>
                ))}
                {plan.unavailable.map((u) => (
                  <j-text key={u.service} nomargin size="400" color="warning-500">
                    {SERVICE_LABELS[u.service] ?? u.service}: {u.reason}
                  </j-text>
                ))}
                {plan.reasons.map((r) => (
                  <j-text key={r} nomargin size="400" color={plan.fits ? "ui-700" : "danger-500"}>
                    {r}
                  </j-text>
                ))}
                <j-text nomargin color="ui-0">
                  {plan.downloadBytes > 0 ? `Download: ${gb(plan.downloadBytes)}` : "Everything is downloaded"}
                  {plan.fits ? ", fits this device" : ", does not fit this device"}
                </j-text>

                {installing && progress && (
                  <j-flex direction="column" gap="200">
                    <progress
                      max={Math.max(progress.totalBytes, 1)}
                      value={progress.doneBytes}
                      style={{ width: "100%" }}
                    />
                    <j-text nomargin size="400" color="ui-700">
                      {progress.phase === "download" && progress.item
                        ? `${progress.item}: ${gb(progress.itemDoneBytes)} of ${gb(progress.itemTotalBytes)}`
                        : progress.phase}
                    </j-text>
                  </j-flex>
                )}
                {installing && !progress && <j-text nomargin>Preparing...</j-text>}

                {localStatus?.installed && !installing && (
                  <j-flex direction="column" gap="200">
                    {localStatus.runtimes.map((r) => (
                      <j-text key={r.provider} nomargin size="400">
                        {r.provider}: {r.phase ?? "not started"}
                        {r.port ? ` on port ${r.port}` : ""}
                        {r.onGpu === true ? `, on the GPU (${gb(r.gpuBytes ?? 0)})` : ""}
                        {r.onGpu === false ? ", on the CPU: the GPU build found no device" : ""}
                        {r.detail ? ` (${r.detail})` : ""}
                      </j-text>
                    ))}
                  </j-flex>
                )}

                {localUp ? (
                  <j-text nomargin color="success-500">
                    Local AI runs: its models are the defaults.
                  </j-text>
                ) : localStatus?.installed && plan.downloadBytes === 0 && !installing ? (
                  <j-button variant="primary" onClick={startLocal} loading={busy === "local-start"}>
                    Start local AI
                  </j-button>
                ) : (
                  <j-button
                    variant="primary"
                    disabled={!plan.fits || installing}
                    loading={installing}
                    onClick={installLocal}
                  >
                    {plan.downloadBytes > 0 ? `Install (${gb(plan.downloadBytes)})` : "Install"}
                  </j-button>
                )}
              </j-flex>
            </j-box>
          )}
        </j-flex>
      )}

      {mode === "Server" && (
        <j-flex direction="column" gap="400" style={{ width: "100%" }}>
          <j-flex gap="300" j="center">
            {(["Detected", "Remote"] as ServerMode[]).map((m) => (
              <j-button key={m} variant={serverMode === m ? "primary" : "subtle"} onClick={() => setServerMode(m)}>
                {m === "Detected" ? "On this computer" : "Remote API"}
              </j-button>
            ))}
          </j-flex>

          {serverMode === "Detected" && (
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
                      {(servedChat[server.baseUrl] ?? []).length > 0 && (
                        <j-text nomargin size="400">
                          Chat models it already serves (the chosen one is added when you go on):
                        </j-text>
                      )}
                      {(servedChat[server.baseUrl] ?? []).map((id) => {
                        const chosen = isChosen(server, id);
                        return (
                          <j-flex key={`served:${id}`} a="center" j="between" gap="300">
                            <j-text nomargin size="400">
                              {id}
                            </j-text>
                            <j-button
                              size="sm"
                              variant={chosen ? "primary" : "subtle"}
                              disabled={!!busy}
                              loading={busy === `LLM:${id}`}
                              onClick={() => setChatChoice({ server, remoteModel: id })}
                            >
                              {chosen ? "Chosen" : "Choose"}
                            </j-button>
                          </j-flex>
                        );
                      })}
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

          {serverMode === "Remote" && (
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
