import { useCallback, useContext, useEffect, useRef, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import { cardStyle, listStyle } from "./styles";

const AI = () => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [models, setModels] = useState<any[]>([]);
  const modelsRef = useRef<any[]>([]);

  const getData = useCallback(async () => {
    const modelsInDB = await client!.ai.getModels();
    const tasksInDB = await client!.ai.tasks();
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

  function toggleTasks(modelName: string) {
    const newModels = [...models];
    const match = newModels.find((model) => model.name === modelName);
    if (match) match.collapsed = !match.collapsed;
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
                  const status = await client.ai.modelLoadingStatus(model.name);
                  resolve({ ...model, ...status });
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
    <div style={listStyle}>
      {models.map((model) => (
        <j-flex
          key={model.name}
          gap="400"
          style={{ ...cardStyle, width: "100%" }}
        >
          <j-text variant="heading">{model.name}</j-text>
          <j-text variant="heading-sm">{model.type}</j-text>
          <j-text>Model {model.status}</j-text>
          {!model.downloaded && (
            <j-text>Downloading: {model.progress.toFixed(2)}%</j-text>
          )}
          {model.downloaded && !model.loaded && (
            <j-text>Loading: {model.progress.toFixed(2)}%</j-text>
          )}
          {model.tasks.length === 0 ? (
            <j-text nomargin>No tasks created</j-text>
          ) : (
            <j-flex gap="400">
              <j-button onClick={() => toggleTasks(model.name)}>
                {model.collapsed ? "Show" : "Hide"} tasks ({model.tasks.length})
              </j-button>
              {!model.collapsed && (
                <j-flex gap="300">
                  {model.tasks.map((task: any) => (
                    <j-flex
                      key={task.taskId}
                      gap="300"
                      style={{ ...cardStyle, width: "100%" }}
                    >
                      <j-text variant="heading-sm">{task.name}</j-text>
                      <j-text>{task.modelId}</j-text>
                      <j-text>{task.taskId}</j-text>
                      <j-text>{task.systemPrompt}</j-text>
                    </j-flex>
                  ))}
                </j-flex>
              )}
            </j-flex>
          )}
        </j-flex>
      ))}
    </div>
  );
};

export default AI;
