import { Model } from "@coasys/ad4m/lib/src/ai/AIResolver";
import { useCallback, useContext, useEffect, useRef, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import "../index.css";
import ModelModal from "./ModelModal";
import { cardStyle } from "./styles";

const AI = () => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [models, setModels] = useState<any[]>([]);
  const modelsRef = useRef<any[]>([]);
  const [selectedModel, setSelectedModel] = useState(null);
  const [newModelModalOpen, setNewModelModalOpen] = useState(false);

  const getData = useCallback(async () => {
    const modelsInDB = await client!.ai.getModels();
    const tasksInDB = await client!.ai.tasks();
    console.log("modelsInDxB: ", modelsInDB);
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

  function deleteModel(model: Model) {
    console.log("delete model: ", model.name);
    // client!.ai.deleteModel(model.id);
    // getData()
  }

  function toggleModelTasks(modelName: string) {
    const newModels = [...models];
    const match = newModels.find((model) => model.name === modelName);
    if (match) match.collapsed = !match.collapsed;
    setModels(newModels);
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
            {models.map((model) => (
              <j-box className="box">
                <j-flex j="between">
                  <j-text size="700" nomargin>
                    {model.name}
                  </j-text>
                  <j-flex gap="300">
                    <j-button size="sm" onClick={() => setSelectedModel(model)}>
                      <j-icon name="pencil-square" size="sm" />
                    </j-button>
                    <j-button size="sm" onClick={() => deleteModel(model)}>
                      <j-icon name="trash" size="sm" />
                    </j-button>
                  </j-flex>
                </j-flex>
                <j-flex direction="column" gap="400">
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
