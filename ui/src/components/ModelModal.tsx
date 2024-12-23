import { ModelInput } from "@coasys/ad4m/lib/src/ai/AIResolver";
import { useContext, useEffect, useState, useRef } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import "../index.css";

const AITypes = ["LLM", "EMBEDDING", "TRANSCRIPTION"];
const llmModels = [
  "External API",
  // "tiny_llama_1_1b",
  "mistral_7b",
  "mistral_7b_instruct",
  "mistral_7b_instruct_2",
  "solar_10_7b",
  "solar_10_7b_instruct",
  "llama_7b",
  "llama_7b_chat",
  "llama_7b_code",
  "llama_8b",
  "llama_8b_chat",
  "llama_3_1_8b_chat",
  "llama_13b",
  "llama_13b_chat",
  "llama_13b_code",
  "llama_34b_code",
  "llama_70b",
];
const transcriptionModels = ["whisper"];
const embeddingModels = ["bert"];

export default function ModelModal(props: { close: () => void; oldModel?: any }) {
  const { close, oldModel } = props;
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [newModelName, setNewModelName] = useState("");
  const [newModelNameError, setNewModelNameError] = useState(false);
  const [newModelType, setNewModelType] = useState("LLM");
  const [newModels, setNewModels] = useState<any[]>(llmModels);
  const [newModel, setNewModel] = useState("llama_8b");
  const [apiUrl, setApiUrl] = useState("https://api.openai.com/v1");
  const [apiKey, setApiKey] = useState("");
  const [apiUrlError, setApiUrlError] = useState(false);
  const [apiKeyError, setApiKeyError] = useState(false);
  const [apiModelError, setApiModelError] = useState(false);
  const [apiLoading, setApiLoading] = useState(false);
  const [apiErrorMessage, setApiErrorMessage] = useState("");
  const [apiModel, setApiModel] = useState("");
  const [apiModels, setApiModels] = useState<string[]>([]);
  const [apiModelsLoading, setApiModelsLoading] = useState(false);
  const apiUrlRef = useRef("https://api.openai.com/v1");
  const apiKeyRef = useRef("");
  const [saving, setSaving] = useState(false);

  function closeMenu(menuId: string) {
    const menu = document.getElementById(menuId);
    const items = menu?.shadowRoot?.querySelector("details");
    if (items) items.open = false;
  }

  async function checkApi() {
    setApiLoading(true);
    setApiErrorMessage("");
    setApiUrlError(false);
    setApiKeyError(false);
    // validate API
    let apiValid = false;
    try {
      const response = await fetch(`${apiUrlRef.current}/models`, {
        method: "GET",
        headers: {
          Authorization: `Bearer ${apiKeyRef.current}`,
          "Content-Type": "application/json",
        },
      });
      const { ok, status, statusText } = response;
      if (ok) apiValid = true;
      else {
        setApiKeyError(true);
        setApiErrorMessage(status === 401 ? "Invalid key" : statusText);
      }
    } catch {
      setApiUrlError(true);
      setApiErrorMessage("Error connecting to API");
    }
    setApiLoading(false);
    if (apiValid) {
      // get models
      setApiModels([]);
      setApiModel("");
      setApiModelsLoading(true);
      try {
        const response = await fetch(`${apiUrlRef.current}/models`, {
          method: "GET",
          headers: {
            Authorization: `Bearer ${apiKeyRef.current}`,
            "Content-Type": "application/json",
          },
        });
        if (response.ok) {
          const body = await response.json();
          const models = body.data.map((e: any) => e.id);
          setApiModels(models);
          if (apiUrlRef.current === "https://api.openai.com/v1") {
            setApiModel("gpt-4o");
            setApiModelError(false);
          }
        }
      } catch {}
    } else {
      setApiModels([]);
      setApiModel("");
    }
    setApiModelsLoading(false);
  }

  function apiValid() {
    if (newModel === "External API") {
      if (apiKeyError || apiUrlError) return false;
      else if (!apiUrl || !apiKey) {
        // missing values
        setApiUrlError(!apiUrl);
        setApiKeyError(!apiKey);
        return false;
      } else if (!apiModel) {
        // missing model
        setApiModelError(true);
        setApiErrorMessage("Model required");
        return false;
      }
    }
    setApiModelError(false);
    return true;
  }


  async function testRun(): Promise<boolean> {
    setApiLoading(true);
    try {
      const response = await fetch(`${apiUrl}/chat/completions`, {
        method: "POST",
        headers: {
          Authorization: `Bearer ${apiKey}`,
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          model: apiModel,
          messages: [
            {
              role: "user",
              content: "Hi"
            }
          ],
          max_tokens: 5
        })
      });

      setApiLoading(false);
      const { ok, status, statusText } = response;
      if (ok) {
        console.log("API test run successful!")
        return true
      } else {
        if(status === 401) {
          setApiErrorMessage("Invalid Key")
          setApiKeyError(true)
        } else {
          setApiErrorMessage(statusText)
        }
        return false
      }
    } catch (e) {
      setApiErrorMessage(`Error testing API completion: ${e}`);
    }
    return false;
  }


  async function saveModel() {
    setSaving(true);
    if(!await testRun()){
      setSaving(false)
      return;
    } 
    // validate model settings
    if (!newModelName) setNewModelNameError(true);
    else if (apiValid()) {
      // create new model
      const model = {
        name: newModelName,
        modelType: newModelType,
      } as ModelInput;
      if (newModel === "External API") {
        model.api = { baseUrl: apiUrl, apiKey, apiType: "OPEN_AI", model: apiModel };
      } else {
        model.local = {
          fileName: newModel,
          tokenizerSource: "",
          modelParameters: "",
        };
      }
      if (oldModel) client!.ai.updateModel(oldModel.id, model);
      else {
        const newModelId = await client!.ai.addModel(model);
        // if no default LLM set, mark new model as default
        const defaultLLM = await client!.ai.getDefaultModel("LLM");
        if (!defaultLLM) client!.ai.setDefaultModel("LLM", newModelId);
      }
      close();
    }
    setSaving(false);
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
        apiUrlRef.current = oldModel.api.baseUrl;
        setApiKey(oldModel.api.apiKey);
        apiKeyRef.current = oldModel.api.apiKey;
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
      <j-box px="100">
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
                            setNewModel("llama_8b");
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
                  <j-button
                    onClick={() => {
                      setApiUrl("https://api.openai.com/v1");
                      apiUrlRef.current = "https://api.openai.com/v1";
                      if (apiKeyRef.current) checkApi();
                      else setApiKeyError(false);
                    }}
                  >
                    OpenAI
                  </j-button>
                  <j-button
                    onClick={() => {
                      setApiUrl("http://localhost:11434/v1");
                      apiUrlRef.current = "http://localhost:11434/v1";
                      if (apiKeyRef.current) checkApi();
                      else setApiKeyError(false);
                    }}
                  >
                    Ollama
                  </j-button>
                </j-flex>

                <j-flex a="center" gap="400">
                  <j-text nomargin color="ui-800" style={{ flexShrink: 0 }}>
                    API URL:
                  </j-text>
                  <j-input
                    size="md"
                    type="text"
                    value={apiUrl}
                    error={apiUrlError && !apiErrorMessage}
                    errortext="Required"
                    onInput={(e: any) => {
                      setApiUrl(e.target.value);
                      apiUrlRef.current = e.target.value;
                      checkApi();
                    }}
                    style={{ width: "100%" }}
                  />
                  {apiUrlError && <j-icon name="x-circle" color="danger-500" />}
                </j-flex>

                {apiUrl !== "http://localhost:11434/v1" && (
                  <j-flex a="center" gap="400">
                    <j-text nomargin color="ui-800" style={{ flexShrink: 0 }}>
                      API Key:
                    </j-text>
                    <j-input
                      size="md"
                      type="text"
                      value={apiKey}
                      error={apiKeyError && !apiErrorMessage}
                      errortext="Required"
                      onInput={(e: any) => {
                        setApiKey(e.target.value);
                        apiKeyRef.current = e.target.value;
                        checkApi();
                      }}
                      style={{ width: "100%" }}
                    />
                    {apiKey && !apiUrlError && (
                      <>
                        {apiLoading ? (
                          <div style={{ marginRight: 10 }}>
                            <j-spinner size="xs" />
                          </div>
                        ) : (
                          <j-icon
                            name={apiKeyError ? "x-circle" : "check-circle"}
                            color={apiKeyError ? "danger-500" : "success-500"}
                          />
                        )}
                      </>
                    )}
                  </j-flex>
                )}

                <j-flex a="center" gap="400">
                  <j-text nomargin color="ui-800" style={{ flexShrink: 0 }}>
                    API Model:
                  </j-text>
                  <j-menu style={{ zIndex: 2 }}>
                    <j-menu-group collapsible title={apiModel || "None selected"} id="api-models">
                      {apiModels.map((model) => (
                        <j-menu-item
                          selected={model === apiModel}
                          onClick={() => {
                            setApiModelError(false);
                            setApiErrorMessage("");
                            setApiModel(model);
                            closeMenu("api-models");
                          }}
                        >
                          {model}
                        </j-menu-item>
                      ))}
                    </j-menu-group>
                  </j-menu>
                  {apiModelsLoading && <j-spinner size="xs" style={{ marginLeft: "8px" }} />}
                  {apiModelError && <j-icon name="x-circle" color="danger-500" />}
                </j-flex>

                {apiErrorMessage && (
                  <j-text color="danger-500" nomargin>
                    {apiErrorMessage}
                  </j-text>
                )}
              </>
            )}
          </j-flex>

          <j-button onClick={saveModel} variant="primary" loading={saving}>
            Save model
          </j-button>
        </j-flex>
      </j-box>
    </j-modal>
  );
}
