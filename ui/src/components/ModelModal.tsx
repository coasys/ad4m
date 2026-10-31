import {
  AI_RECOMMENDED_MODELS,
  type AIModel,
  type AiEndpoint,
  type DetectedEndpoint,
  type DiscoveredModel,
  type ModelType,
} from "@coasys/ad4m";
import { useCallback, useContext, useEffect, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import "../index.css";
import {
  capabilityOf,
  endpointFor,
  ENDPOINT_KINDS,
  kindLabel,
  serves,
} from "./aiSetup";

const MODEL_TYPES: ModelType[] = ["LLM", "EMBEDDING", "TRANSCRIPTION", "SPEECH"];

function closeMenu(menuId: string) {
  const menu = document.getElementById(menuId);
  const items = menu?.shadowRoot?.querySelector("details");
  if (items) items.open = false;
}

/**
 * Add or edit a model: an upstream model on an endpoint. The endpoint is picked from the
 * saved ones, from the servers detected on this machine, or entered by hand; the model
 * from what the endpoint lists (an Ollama endpoint can pull the recommended one).
 */
export default function ModelModal(props: { close: () => void; oldModel?: AIModel }) {
  const { close, oldModel } = props;
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [name, setName] = useState(oldModel?.name ?? "");
  const [modelType, setModelType] = useState<ModelType>(oldModel?.modelType ?? "LLM");
  const [endpoints, setEndpoints] = useState<AiEndpoint[]>([]);
  const [endpointId, setEndpointId] = useState<string | null>(oldModel?.endpointId ?? null);
  const [remoteModels, setRemoteModels] = useState<DiscoveredModel[]>([]);
  const [remoteModel, setRemoteModel] = useState(oldModel?.remoteModel ?? "");
  const [nativeTools, setNativeTools] = useState<boolean | undefined>(
    oldModel?.nativeTools ?? undefined
  );
  const [busy, setBusy] = useState<string | null>(null);
  const [error, setError] = useState("");

  const [adding, setAdding] = useState(false);
  const [detected, setDetected] = useState<DetectedEndpoint[]>([]);
  const [newKind, setNewKind] = useState(ENDPOINT_KINDS[0]);
  const [newUrl, setNewUrl] = useState(ENDPOINT_KINDS[0].baseUrl);
  const [newKey, setNewKey] = useState("");

  const endpoint = endpoints.find((e) => e.id === endpointId);
  const recommended = AI_RECOMMENDED_MODELS.find((m) => m.modelType === modelType)?.ollama;

  const loadEndpoints = useCallback(async () => {
    if (!client) return;
    setEndpoints(await client.ai.endpoints());
  }, [client]);

  useEffect(() => {
    loadEndpoints();
  }, [loadEndpoints]);

  // The models the chosen endpoint lists, those of the chosen type first.
  useEffect(() => {
    if (!client || !endpointId) return;
    setRemoteModels([]);
    client.ai
      .discoverModels({ endpointId })
      .then((models) => {
        const wanted = capabilityOf(modelType);
        setRemoteModels([
          ...models.filter((m) => m.capabilities.includes(wanted)),
          ...models.filter((m) => !m.capabilities.includes(wanted)),
        ]);
      })
      .catch((e) => setError(`The endpoint does not list its models: ${e}`));
  }, [client, endpointId, modelType]);

  async function startAdding() {
    setAdding(true);
    setError("");
    try {
      setDetected(await client!.ai.detectEndpoints());
    } catch (e) {
      setError(`${e}`);
    }
  }

  async function addEndpoint(kind = newKind.kind, baseUrl = newUrl, apiKey = newKey) {
    setBusy("endpoint");
    setError("");
    try {
      const added = await endpointFor(client!, kind, baseUrl, apiKey);
      await loadEndpoints();
      setEndpointId(added.id);
      setAdding(false);
      if (added.health.state !== "REACHABLE") {
        setError(
          `Saved, but the server answers ${added.health.state}` +
            (added.health.detail ? `: ${added.health.detail}` : "")
        );
      }
    } catch (e) {
      setError(`${e}`);
    }
    setBusy(null);
  }

  async function pull(model: string) {
    setBusy("pull");
    setError("");
    try {
      await client!.ai.pullModel(endpointId!, model);
      setRemoteModel(model);
      await loadEndpoints();
      setRemoteModels(await client!.ai.discoverModels({ endpointId: endpointId! }));
    } catch (e) {
      setError(`${e}`);
    }
    setBusy(null);
  }

  async function save() {
    if (!name || !endpointId || !remoteModel) {
      setError("A name, an endpoint and a model are required");
      return;
    }
    setBusy("save");
    setError("");
    const input = { name, modelType, endpointId, remoteModel, nativeTools };
    try {
      if (oldModel) {
        await client!.ai.updateModel(oldModel.id, input);
      } else {
        const id = await client!.ai.addModel(input);
        if (!(await client!.ai.getDefaultModel(modelType))) {
          await client!.ai.setDefaultModel(modelType, id);
        }
      }
      close();
    } catch (e) {
      setError(`${e}`);
    }
    setBusy(null);
  }

  const openAiShaped = endpoint && !["OLLAMA", "ANTHROPIC"].includes(endpoint.kind);

  return (
    <j-modal
      open
      onToggle={(e: any) => {
        if (!e.target.open) close();
      }}
    >
      <j-box px="100">
        <j-flex direction="column" a="center" gap="600">
          <j-text size="700" color="ui-0">
            {oldModel ? "Edit" : "Add a"} model
          </j-text>

          <j-input
            size="md"
            type="text"
            label="Name"
            value={name}
            onInput={(e: any) => setName(e.target.value)}
          />

          <j-flex a="center" gap="400">
            <j-text nomargin color="ui-800">
              Type:
            </j-text>
            <j-menu style={{ zIndex: 3 }}>
              <j-menu-group collapsible title={modelType} id="model-types">
                {MODEL_TYPES.map((type) => (
                  <j-menu-item
                    key={type}
                    selected={type === modelType}
                    onClick={() => {
                      setModelType(type);
                      closeMenu("model-types");
                    }}
                  >
                    {type}
                  </j-menu-item>
                ))}
              </j-menu-group>
            </j-menu>
          </j-flex>

          <j-flex a="center" gap="400">
            <j-text nomargin color="ui-800">
              Endpoint:
            </j-text>
            <j-menu style={{ zIndex: 2 }}>
              <j-menu-group
                collapsible
                title={endpoint ? `${endpoint.name} (${kindLabel(endpoint.kind)})` : "None selected"}
                id="endpoints"
              >
                {endpoints.map((e) => (
                  <j-menu-item
                    key={e.id}
                    selected={e.id === endpointId}
                    onClick={() => {
                      setEndpointId(e.id);
                      setRemoteModel("");
                      closeMenu("endpoints");
                    }}
                  >
                    {e.name} ({kindLabel(e.kind)}, {e.health.state.toLowerCase()})
                  </j-menu-item>
                ))}
              </j-menu-group>
            </j-menu>
            <j-button variant="subtle" size="sm" onClick={startAdding}>
              <j-icon name="plus-lg" size="sm" /> Endpoint
            </j-button>
          </j-flex>

          {adding && (
            <j-box className="box" style={{ width: "100%" }}>
              <j-flex direction="column" gap="400">
                {detected.length > 0 && <j-text nomargin>On this machine:</j-text>}
                {detected.map((d) => (
                  <j-flex key={d.baseUrl} a="center" j="between" gap="300">
                    <j-text nomargin>
                      {kindLabel(d.kind)} at {d.baseUrl}
                    </j-text>
                    <j-button
                      size="sm"
                      loading={busy === "endpoint"}
                      onClick={() => addEndpoint(d.kind, d.baseUrl, "")}
                    >
                      Use
                    </j-button>
                  </j-flex>
                ))}
                <j-text nomargin>Or enter one:</j-text>
                <j-menu style={{ zIndex: 1 }}>
                  <j-menu-group collapsible title={newKind.label} id="endpoint-kinds">
                    {ENDPOINT_KINDS.map((k) => (
                      <j-menu-item
                        key={k.kind}
                        selected={k.kind === newKind.kind}
                        onClick={() => {
                          setNewKind(k);
                          setNewUrl(k.baseUrl);
                          setNewKey("");
                          closeMenu("endpoint-kinds");
                        }}
                      >
                        {k.label}
                      </j-menu-item>
                    ))}
                  </j-menu-group>
                </j-menu>
                <j-input
                  size="md"
                  label="URL"
                  value={newUrl}
                  onInput={(e: any) => setNewUrl(e.target.value)}
                />
                <j-input
                  size="md"
                  type="password"
                  label="API key (if it needs one)"
                  value={newKey}
                  onInput={(e: any) => setNewKey(e.target.value)}
                />
                <j-button variant="primary" loading={busy === "endpoint"} onClick={() => addEndpoint()}>
                  Add endpoint
                </j-button>
              </j-flex>
            </j-box>
          )}

          {endpoint && (
            <j-flex a="center" gap="400" wrap>
              <j-text nomargin color="ui-800">
                Model:
              </j-text>
              <j-menu style={{ zIndex: 1 }}>
                <j-menu-group collapsible title={remoteModel || "None selected"} id="remote-models">
                  {remoteModels.map((m) => (
                    <j-menu-item
                      key={m.id}
                      selected={m.id === remoteModel}
                      onClick={() => {
                        setRemoteModel(m.id);
                        if (!name) setName(m.id);
                        closeMenu("remote-models");
                      }}
                    >
                      {m.id}
                    </j-menu-item>
                  ))}
                </j-menu-group>
              </j-menu>
              {endpoint.kind === "OLLAMA" &&
                recommended &&
                !serves(endpoint.health.servedModels, recommended) && (
                  <j-button size="sm" loading={busy === "pull"} onClick={() => pull(recommended)}>
                    Download {recommended}
                  </j-button>
                )}
            </j-flex>
          )}

          {modelType === "LLM" && openAiShaped && (
            <j-toggle
              checked={!!nativeTools}
              onChange={(e: any) => setNativeTools(e.target.checked || undefined)}
            >
              The model calls tools natively (the server passes `tools` on)
            </j-toggle>
          )}

          {error && (
            <j-text color="danger-500" style={{ maxWidth: 420 }}>
              {error}
            </j-text>
          )}

          <j-button onClick={save} variant="primary" loading={busy === "save"}>
            Save model
          </j-button>
        </j-flex>
      </j-box>
    </j-modal>
  );
}
