import { ModelInput } from "@coasys/ad4m/lib/src/ai/AIResolver";
import { useContext, useEffect, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import "../index.css";

const AITypes = ["LLM", "EMBEDDING", "TRANSCRIPTION"];
const llmModels = [
  "External API",
  // "tiny_llama_1_1b",
  "llama_7b",
  "llama_8b",
  "llama_13b",
  "llama_70b",
];
const transcriptionModels = ["whisper"];
const embeddingModels = ["bert"];

export default function ModelModal(props: {
  close: () => void;
  oldModel?: any;
}) {
  const { close, oldModel } = props;
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [newModelName, setNewModelName] = useState("");
  const [newModelNameError, setNewModelNameError] = useState(false);
  const [newModelType, setNewModelType] = useState("LLM");
  const [newModels, setNewModels] = useState<any[]>(llmModels);
  const [newModel, setNewModel] = useState("llama_7b");
  const [apiUrl, setApiUrl] = useState("https://api.openai.com/v1");
  const [apiKey, setApiKey] = useState("");
  const [apiUrlError, setApiUrlError] = useState(false);
  const [apiKeyError, setApiKeyError] = useState(false);
  const [apiLoading, setApiLoading] = useState(false);
  const [apiError, setApiError] = useState("");

  function closeMenu(menuId: string) {
    const menu = document.getElementById(menuId);
    const items = menu?.shadowRoot?.querySelector("details");
    if (items) items.open = false;
  }

  async function apiValid(): Promise<boolean> {
    return new Promise(async (resolve) => {
      if (newModel !== "External API") resolve(true);
      else if (!(apiUrl && apiKey)) {
        // missing values
        setApiUrlError(!apiUrl);
        setApiKeyError(!apiKey);
        resolve(false);
      } else {
        // test api
        setApiLoading(true);
        try {
          const response = await fetch(`${apiUrl}/models`, {
            method: "GET",
            headers: {
              Authorization: `Bearer ${apiKey}`,
              "Content-Type": "application/json",
            },
          });
          const { ok, status, statusText } = response;
          if (ok) resolve(true);
          else {
            setApiError(status === 401 ? "Invalid key" : statusText);
            resolve(false);
          }
          setApiLoading(false);
        } catch {
          resolve(false);
          setApiError("Error connecting to API");
          setApiLoading(false);
        }
      }
    });
  }

  async function saveModel() {
    // validate model settings
    if (!newModelName) setNewModelNameError(true);
    else if (await apiValid()) {
      // create new model
      const model = {
        name: newModelName,
        modelType: newModelType,
      } as ModelInput;
      if (newModel === "External API") {
        model.api = { baseUrl: apiUrl, apiKey, apiType: "OPEN_AI" };
      } else {
        model.local = {
          fileName: newModel,
          tokenizerSource: "",
          modelParameters: "",
        };
      }
      if (oldModel) client!.ai.updateModel(oldModel.id, model);
      else client!.ai.addModel(model);
      close();
    }
  }

  useEffect(() => {
    if (oldModel) {
      setNewModelName(oldModel.name);
      setNewModelType(oldModel.modelType);

      if (oldModel.modelType === "LLM") {
        setNewModels(llmModels);
        setNewModel(oldModel.api ? "External API" : oldModel.local.fileName);
      } else if (oldModel.modelType === "EMBEDDING") {
        setNewModels(embeddingModels);
        setNewModel(oldModel.local.fileName);
      } else {
        setNewModels(transcriptionModels);
        setNewModel(oldModel.local.fileName);
      }

      if (oldModel.api) {
        setApiUrl(oldModel.api.baseUrl);
        setApiKey(oldModel.api.apiKey);
      }
    }
  }, []);

  return (
    <j-modal
      open
      onToggle={(e: any) => {
        if (!e.target.open) close();
      }}
    >
      <j-flex direction="column" a="center" gap="700">
        <j-text size="700" color="ui-0">
          {oldModel ? "Edit" : "Add a new"} model
        </j-text>

        <j-flex direction="column" a="center" gap="500">
          <j-input
            size="md"
            type="text"
            label="Name"
            value={newModelName}
            error={newModelNameError}
            errortext="Required"
            onInput={(e: any) => {
              setNewModelNameError(false);
              setNewModelName(e.target.value);
            }}
            style={{ width: "100%" }}
          />

          {!oldModel && (
            <j-flex a="center" gap="400">
              <j-text nomargin color="ui-800" style={{ flexShrink: 0 }}>
                Type:
              </j-text>
              <j-menu style={{ height: 42, zIndex: 3 }}>
                <j-menu-group collapsible title={newModelType} id="ai-types">
                  {AITypes.map((type) => (
                    <j-menu-item
                      selected={newModelType === type}
                      onClick={() => {
                        setNewModelType(type);
                        if (type === "LLM") {
                          setNewModels(llmModels);
                          setNewModel("llama_7b");
                        } else if (type === "EMBEDDING") {
                          setNewModels(embeddingModels);
                          setNewModel("bert");
                        } else {
                          setNewModels(transcriptionModels);
                          setNewModel("whisper");
                        }
                        closeMenu("ai-types");
                      }}
                    >
                      {type}
                    </j-menu-item>
                  ))}
                </j-menu-group>
              </j-menu>
            </j-flex>
          )}

          <j-flex a="center" gap="400">
            <j-text nomargin color="ui-800" style={{ flexShrink: 0 }}>
              Model:
            </j-text>
            <j-menu style={{ zIndex: 2 }}>
              <j-menu-group collapsible title={newModel} id="new-models">
                {newModels.map((model) => (
                  <j-menu-item
                    selected={newModel === model}
                    onClick={() => {
                      setNewModel(model);
                      closeMenu("new-models");
                    }}
                  >
                    {model}
                  </j-menu-item>
                ))}
              </j-menu-group>
            </j-menu>
          </j-flex>

          {newModel === "External API" && (
            <>
              <j-flex a="center" gap="400">
                <j-text nomargin color="ui-800" style={{ flexShrink: 0 }}>
                  API URL:
                </j-text>
                <j-input
                  size="md"
                  type="text"
                  value={apiUrl}
                  error={apiUrlError}
                  errortext="Required"
                  onInput={(e: any) => {
                    setApiUrlError(false);
                    setApiError("");
                    setApiUrl(e.target.value);
                  }}
                  style={{ width: "100%" }}
                />
              </j-flex>
              <j-flex a="center" gap="400">
                <j-text nomargin color="ui-800" style={{ flexShrink: 0 }}>
                  API Key:
                </j-text>
                <j-input
                  size="md"
                  type="text"
                  value={apiKey}
                  error={apiKeyError}
                  errortext="Required"
                  onInput={(e: any) => {
                    setApiKeyError(false);
                    setApiError("");
                    setApiKey(e.target.value);
                  }}
                  style={{ width: "100%" }}
                />
              </j-flex>
              {apiLoading && (
                <j-flex a="center" gap="400">
                  <j-text nomargin color="ui-0" size="600">
                    Testing API...
                  </j-text>
                  <j-spinner size="xs" />
                </j-flex>
              )}
              {apiError && <j-text color="danger-500">{apiError}</j-text>}
            </>
          )}
        </j-flex>

        <j-button
          onClick={saveModel}
          variant="primary"
          //   style={{ marginTop: 40 }}
        >
          Save model
        </j-button>
      </j-flex>
    </j-modal>
  );
}
