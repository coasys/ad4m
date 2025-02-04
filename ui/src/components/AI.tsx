import { Model } from "@coasys/ad4m/lib/src/ai/AIResolver";
import { useCallback, useContext, useEffect, useRef, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import "../index.css";
import ModelCard from "./ModelCard";
import ModelModal from "./ModelModal";

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
    const defaultLLM = await client!.ai.getDefaultModel("LLM");
    // attach tasks to models
    const modelsWithTasks = modelsInDB.map((model) => {
      const modelWithTasks = { ...model } as any;
      if (model.id === defaultLLM?.id) {
        modelWithTasks.default = true;
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

  function removeModel(model: Model) {
    client!.ai.removeModel(model.id);
    getData();
  }

  function setDefaultModel(model: Model) {
    client!.ai.setDefaultModel(model.modelType, model.id);
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
                    .modelLoadingStatus(model.id)
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
              <ModelCard
                key={model.id}
                model={model}
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
