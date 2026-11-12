import type { Ad4mClient, AIModel, AiEndpoint, LocalAiStatus, ModelType } from "@coasys/ad4m";
import { useCallback, useContext, useEffect, useRef, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import "../index.css";
import { LocalAiPanel, useLocalAi } from "./LocalAiPanel";
import ModelCard from "./ModelCard";
import ModelModal from "./ModelModal";
import { endpointLabel, readyLocalDefaults } from "./aiSetup";

const MODEL_TYPES: ModelType[] = ["LLM", "EMBEDDING", "TRANSCRIPTION", "SPEECH"];

/**
 * Local AI in Settings: the same panel as onboarding's (device, plan, the question, progress,
 * the runtimes' state), for someone who skipped it there, chose a model server, or lost a
 * model. An install makes local AI's models the defaults, as onboarding's Next does; if its
 * chat model does not run yet, the start that makes it run does. `onChange` reloads the page's
 * endpoints and models.
 */
function LocalAiSettings(props: { client: Ad4mClient; onChange: () => void }) {
  const { client, onChange } = props;
  const [error, setError] = useState("");
  const [stopping, setStopping] = useState(false);
  /** An install ended with its chat model not running: the next start sets the defaults. */
  const [defaultsPending, setDefaultsPending] = useState(false);
  const local = useLocalAi(client, true, setError);

  /** Make local AI's models the defaults when its chat model runs; whether it did. */
  async function applyDefaults(status: LocalAiStatus): Promise<boolean> {
    const defaults = readyLocalDefaults(status);
    if (!defaults) return false;
    try {
      for (const [modelType, id] of defaults) await client.ai.setDefaultModel(modelType, id);
    } catch (e) {
      setError(`${e}`);
    }
    return true;
  }

  async function install() {
    const status = await local.install();
    if (status) setDefaultsPending(!(await applyDefaults(status)));
    onChange();
  }

  async function start() {
    const status = await local.start();
    if (status && defaultsPending && (await applyDefaults(status))) setDefaultsPending(false);
    onChange();
  }

  async function stop() {
    setStopping(true);
    setError("");
    try {
      local.setStatus(await client.ai.localStop());
    } catch (e) {
      setError(`${e}`);
    }
    setStopping(false);
    onChange();
  }

  return (
    <j-flex direction="column" gap="300">
      <LocalAiPanel
        local={local}
        title="Local AI on this device"
        onInstall={install}
        onStart={start}
        onStop={{ run: stop, stopping }}
      />
      {error && (
        <j-text nomargin color="danger-500">
          {error}
        </j-text>
      )}
    </j-flex>
  );
}

const AI = () => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [models, setModels] = useState<any[]>([]);
  const modelsRef = useRef<any[]>([]);
  const [endpoints, setEndpoints] = useState<AiEndpoint[]>([]);
  const [selectedModel, setSelectedModel] = useState<AIModel | null>(null);
  const [newModelModalOpen, setNewModelModalOpen] = useState(false);
  /** Bumped when local AI is removed here: the local AI panel reads it again. */
  const [localGeneration, setLocalGeneration] = useState(0);

  const getData = useCallback(async () => {
    const modelsInDB = await client!.ai.getModels();
    const tasksInDB = await client!.ai.tasks();
    setEndpoints(await client!.ai.endpoints());
    const defaults = await Promise.all(
      MODEL_TYPES.map((type) => client!.ai.getDefaultModel(type))
    );
    const defaultIds = new Set(defaults.map((d) => d?.id).filter(Boolean));
    // attach tasks to models
    const modelsWithTasks = modelsInDB.map((model) => {
      const modelWithTasks = { ...model } as any;
      if (defaultIds.has(model.id)) {
        modelWithTasks.default = true;
      }
      if (model.modelType === "LLM" && defaultIds.has(model.id)) {
        // find tasks for default model
        const matchingTasks = tasksInDB.filter(
          (task) => task.modelId === "default"
        );
        // add collapsed boolean to each task
        modelWithTasks.tasks = matchingTasks.map((task: any) => {
          task.collapsed = true;
          return task;
        });
        modelWithTasks.collapsed = true;
      }
      return modelWithTasks;
    });
    modelsRef.current = modelsWithTasks;
    setModels(modelsWithTasks);
  }, [client]);

  async function removeModel(model: AIModel) {
    await client!.ai.removeModel(model.id);
    getData();
  }

  async function setDefaultModel(model: AIModel) {
    await client!.ai.setDefaultModel(model.modelType, model.id);
    getData();
  }

  async function removeEndpoint(endpoint: AiEndpoint) {
    try {
      if (endpoint.managed) {
        // This executor runs it: removing it stops local AI and removes its models. The
        // downloaded files stay for the next install (other agents may use them).
        if (!confirm("Stop local AI and remove its models from this agent?")) return;
        await client!.ai.localRemove(false);
        setLocalGeneration((g) => g + 1);
      } else {
        await client!.ai.removeEndpoint(endpoint.id);
      }
    } catch (e) {
      alert(`${e}`);
    }
    getData();
  }

  function toggleTask(modelId: string, taskId: string) {
    const newModels = modelsRef.current.map((model) => {
      if (model.id === modelId) {
        const newTasks = model.tasks.map((task: any) => {
          if (task.taskId === taskId) task.collapsed = !task.collapsed;
          return task;
        });
        model.tasks = newTasks;
      }
      return model;
    });
    modelsRef.current = newModels;
    setModels(newModels);
  }

  function deleteTask(modelId: string, taskId: string) {
    const newModels = modelsRef.current.map((model) => {
      if (model.id === modelId) {
        client!.ai.removeTask(taskId)
        
        model.tasks = model.tasks.filter((task: any) => {
          return (task.taskId !== taskId)
        });
      }
      return model;
    });
    modelsRef.current = newModels;
    setModels(newModels);
  }

  useEffect(() => {
    if (client) getData();
  }, [client, getData]);


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

          {newModelModalOpen && (
            <ModelModal
              close={() => {
                setNewModelModalOpen(false);
                getData();
              }}
            />
          )}

          {selectedModel && (
            <ModelModal
              oldModel={selectedModel}
              close={() => {
                setSelectedModel(null);
                getData();
              }}
            />
          )}

          <j-flex direction="column" gap="400">
            {client && <LocalAiSettings key={localGeneration} client={client} onChange={getData} />}
            {endpoints.length > 0 && (
              <j-box className="box">
                <j-flex direction="column" gap="300">
                  <j-text variant="heading-sm" nomargin>
                    Endpoints
                  </j-text>
                  {endpoints.map((endpoint) => (
                    <j-flex key={endpoint.id} a="center" j="between" gap="300">
                      <j-text nomargin color="ui-0">
                        {endpoint.name}: {endpointLabel(endpoint)} at {endpoint.baseUrl} (
                        {endpoint.managed ? "local AI, " : ""}
                        {endpoint.health.state.toLowerCase()})
                      </j-text>
                      <j-button variant="subtle" size="sm" onClick={() => removeEndpoint(endpoint)}>
                        <j-icon name="trash" size="sm" />
                      </j-button>
                    </j-flex>
                  ))}
                </j-flex>
              </j-box>
            )}
            {models.map((model) => (
              <ModelCard
                key={model.id}
                model={model}
                endpoint={endpoints.find((e) => e.id === model.endpointId)}
                editModel={() => setSelectedModel(model)}
                removeModel={() => removeModel(model)}
                setDefaultModel={() => setDefaultModel(model)}
                toggleTask={toggleTask}
                deleteTask={deleteTask}
              />
            ))}
          </j-flex>
        </div>
      </j-flex>
    </j-box>
  );
};

export default AI;
