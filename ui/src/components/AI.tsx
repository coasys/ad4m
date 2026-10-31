import type { AIModel, AiEndpoint, ModelType } from "@coasys/ad4m";
import { useCallback, useContext, useEffect, useRef, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import "../index.css";
import ModelCard from "./ModelCard";
import ModelModal from "./ModelModal";
import { kindLabel } from "./aiSetup";

const MODEL_TYPES: ModelType[] = ["LLM", "EMBEDDING", "TRANSCRIPTION", "SPEECH"];

const AI = () => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [models, setModels] = useState<any[]>([]);
  const modelsRef = useRef<any[]>([]);
  const [endpoints, setEndpoints] = useState<AiEndpoint[]>([]);
  const [selectedModel, setSelectedModel] = useState<AIModel | null>(null);
  const [newModelModalOpen, setNewModelModalOpen] = useState(false);

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
      await client!.ai.removeEndpoint(endpoint.id);
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
            {endpoints.length > 0 && (
              <j-box className="box">
                <j-flex direction="column" gap="300">
                  <j-text variant="heading-sm" nomargin>
                    Endpoints
                  </j-text>
                  {endpoints.map((endpoint) => (
                    <j-flex key={endpoint.id} a="center" j="between" gap="300">
                      <j-text nomargin color="ui-0">
                        {endpoint.name}: {kindLabel(endpoint.kind)} at {endpoint.baseUrl} (
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
