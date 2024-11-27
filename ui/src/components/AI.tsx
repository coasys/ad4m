import { ModelInput } from "@coasys/ad4m/lib/src/ai/AIResolver";
import { useCallback, useContext, useEffect, useRef, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import "../index.css";
import { cardStyle } from "./styles";

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

const AI = () => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [models, setModels] = useState<any[]>([]);
  const modelsRef = useRef<any[]>([]);

  // new model modal
  const [newModelModalOpen, setNewModelModalOpen] = useState(false);
  const [newModelName, setNewModelName] = useState("");
  const [newModelNameError, setNewModelNameError] = useState(false);
  const [newModelType, setNewModelType] = useState("LLM");
  const [newModels, setNewModels] = useState<any[]>(llmModels);
  const [newModel, setNewModel] = useState("llama_7b");
  const [apiUrl, setApiUrl] = useState("https://api.openai.com/v1");
  const [apiKey, setApiKey] = useState("");
  const [apiUrlError, setApiUrlError] = useState(false);
  const [apiKeyError, setApiKeyError] = useState(false);

  const getData = useCallback(async () => {
    const modelsInDB = await client!.ai.getModels();
    const tasksInDB = await client!.ai.tasks();
    console.log("modelsInDB: ", modelsInDB);
    console.log("tasksInDB: ", tasksInDB);
    // attach tasks to models
    const modelsWithTasks = modelsInDB.map((model) => {
      const matchingTasks = tasksInDB.filter(
        (task) => task.modelId === model.name
      );
      return { ...model, tasks: matchingTasks, collapsed: true };
    });
    modelsRef.current = modelsWithTasks;
    setModels(modelsWithTasks);
  }, [client]);

  function saveModel() {
    // validate model settings
    const invalidApi = newModel === "External API" && !(apiUrl && apiKey);
    if (!newModelName || invalidApi) {
      // display errors
      setNewModelNameError(true);
      if (invalidApi) {
        setApiUrlError(!apiUrl);
        setApiKeyError(!apiKey);
      }
    } else {
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
      client!.ai.addModel(model);
      setNewModelModalOpen(false);
      getData();
    }
  }

  function toggleModelTasks(modelName: string) {
    const newModels = [...models];
    const match = newModels.find((model) => model.name === modelName);
    if (match) match.collapsed = !match.collapsed;
    setModels(newModels);
  }

  function closeMenu(menuId: string) {
    const menu = document.getElementById(menuId);
    const items = menu?.shadowRoot?.querySelector("details");
    if (items) items.open = false;
  }

  function statusText(model: any) {
    const { status, downloaded, loaded, progress } = model;
    if (!status) return "Checking status...";
    else if (downloaded && loaded) return "Model ready";
    return `${downloaded ? "Loading" : "Downloading"}: ${progress?.toFixed(2) || 0}%`;
  }

  useEffect(() => {
    if (client) getData();
  }, [client, getData]);

  // update model status every second until all loaded
  useEffect(() => {
    let interval = null as any;
    if (client && models.length) {
      interval = setInterval(async () => {
        const allLoaded = !models.find(
          (model) => !model.status || !model.loaded
        );
        if (allLoaded) clearInterval(interval);
        else {
          // update each models status
          const newModels = await Promise.all(
            modelsRef.current.map(
              (model) =>
                new Promise(async (resolve) => {
                  client.ai
                    .modelLoadingStatus(model.name)
                    .then((status) => resolve({ ...model, ...status }))
                    .catch((error) => {
                      console.log("model not found: ", error, model.name);
                      resolve(model);
                    });
                })
            )
          );
          setModels(newModels);
          modelsRef.current = newModels;
        }
      }, 1000);
    }
    return () => clearInterval(interval);
  }, [client, models]);

  return (
    <j-box p="400">
      <j-flex j="center">
        <div style={{ width: "100%", maxWidth: 900 }}>
          <j-box pb="400">
            <j-button
              onClick={() => setNewModelModalOpen(true)}
              variant="primary"
            >
              <j-icon
                name="plus-lg"
                style={{ marginLeft: -4, marginRight: -6 }}
              />
              New model
            </j-button>
          </j-box>

          <j-modal
            open={newModelModalOpen}
            onToggle={(e: any) => setNewModelModalOpen(e.target.open)}
          >
            <j-flex direction="column" a="center" gap="700">
              <j-text size="700">Add a new model</j-text>

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

                <j-flex a="center" gap="400">
                  <j-text
                    nomargin
                    color="color-white"
                    style={{ flexShrink: 0 }}
                  >
                    Type:
                  </j-text>
                  <j-menu style={{ height: 42, zIndex: 3 }}>
                    <j-menu-group
                      collapsible
                      title={newModelType}
                      id="ai-types"
                    >
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

                <j-flex a="center" gap="400">
                  <j-text
                    nomargin
                    color="color-white"
                    style={{ flexShrink: 0 }}
                  >
                    Model:
                  </j-text>
                  <j-menu style={{ zIndex: 2 }}>
                    <j-menu-group collapsible title={newModel} id="new-models">
                      {newModels.map((model) => (
                        <j-menu-item
                          selected={newModel === model}
                          onClick={() => {
                            setNewModel(model);
                            if (model === "External API") {
                              setApiUrl("https://api.openai.com/v1");
                            } else {
                              setApiUrl("");
                              setApiKey("");
                              setApiUrlError(false);
                              setApiKeyError(false);
                            }
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
                      <j-text
                        nomargin
                        color="color-white"
                        style={{ flexShrink: 0 }}
                      >
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
                          setApiUrl(e.target.value);
                        }}
                        style={{ width: "100%" }}
                      />
                    </j-flex>
                    <j-flex a="center" gap="400">
                      <j-text
                        nomargin
                        color="color-white"
                        style={{ flexShrink: 0 }}
                      >
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
                          setApiKey(e.target.value);
                        }}
                        style={{ width: "100%" }}
                      />
                    </j-flex>
                  </>
                )}
              </j-flex>

              <j-button
                onClick={saveModel}
                variant="primary"
                style={{ marginTop: 60 }}
              >
                Save model
              </j-button>
            </j-flex>
          </j-modal>

          <j-flex direction="column" gap="400">
            {models.map((model) => (
              <j-box className="box">
                <j-button>
                  <j-icon name="trash" />
                </j-button>
                <j-flex direction="column" gap="400">
                  <j-text variant="heading" nomargin>
                    Model name: {model.name}
                  </j-text>
                  <j-text nomargin>Type: {model.modelType}</j-text>
                  <j-text nomargin>Status: {statusText(model)}</j-text>
                  {model.tasks.length === 0 ? (
                    <j-text nomargin>No tasks created</j-text>
                  ) : (
                    <>
                      <j-button onClick={() => toggleModelTasks(model.name)}>
                        {model.collapsed ? "Show" : "Hide"} tasks (
                        {model.tasks.length})
                      </j-button>
                      {!model.collapsed && (
                        <j-flex gap="300">
                          {model.tasks.map((task: any) => (
                            <div
                              key={task.taskId}
                              style={{ ...cardStyle, width: "100%" }}
                            >
                              <j-flex direction="column" gap="400">
                                <j-text variant="heading-sm" nomargin>
                                  Task name: {task.name}
                                </j-text>
                                <j-text nomargin>Id: {task.taskId}</j-text>
                                <j-text nomargin>
                                  Prompt: {task.systemPrompt}
                                </j-text>
                              </j-flex>
                            </div>
                          ))}
                        </j-flex>
                      )}
                    </>
                  )}
                </j-flex>
              </j-box>
            ))}
          </j-flex>
        </div>
      </j-flex>
    </j-box>
  );
};

export default AI;
