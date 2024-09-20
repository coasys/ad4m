import {
  AITask,
} from "@coasys/ad4m";
import { useCallback, useContext, useEffect, useMemo, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import { cardStyle, listStyle } from "./styles";
import ActionButton from "./ActionButton";

const tempModels = {
  "bert": {
    "name": "BERT TINY",
    "status": "",
    "progress": 0,
    "downloaded": false,
    "loaded": false,
  },
  "llama": {
    "name": "LLAMA",
    "status": "",
    "progress": 0,
    "downloaded": false,
    "loaded": false,
  },
}

const Tasks = (props: {}) => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [tasks, setTasks] = useState<AITask[] | null[]>([]);
  const [showModels, setShowModels] = useState(false);
  const [models, setModels] = useState(tempModels);

  const fetchTasks = useCallback(async () => {
    const tasks = await client!.ai.tasks();

    console.log(tasks);

    setTasks(tasks);
  }, [client]);


  useEffect(() => {
    fetchTasks();

    let interval =  setInterval(async () => {
      console.log("fetching model status");
      const bert = await client!.ai.modelLoadingStatus("bert");
      const llama = await client!.ai.modelLoadingStatus("llama");
      console.log(bert, llama, (bert.loaded && llama.loaded));

      if (bert.loaded && llama.loaded) {
        clearInterval(interval);
      }

      setModels({
        "bert": {
          name: "BERT TINY",
          status: bert.status,
          progress: bert.progress,
          downloaded: bert.downloaded,
          loaded: bert.loaded,
        },
        "llama": {
          name: "LLAMA",
          status: llama.status,
          progress: llama.progress,
          downloaded: llama.downloaded,
          loaded: llama.loaded,
        },
      });
    }, 1000);

    return () => {
      clearInterval(interval);
    }
  }, [fetchTasks, client]);


  return (
    <div>
      <j-box px="500" py="500">
        <j-flex>
          <ActionButton
            title="Models"
            onClick={() => setShowModels(true)}
            icon="database"
          />
        </j-flex>
      </j-box>
      <div style={listStyle}>
        {tasks.map((task) => (
          <div
            key={task?.name}
            style={{...cardStyle, width: "100%"}}
          >
            <j-box pt="300" pb="400">
              <j-text variant="heading-sm">{task?.name}</j-text>
            </j-box>
            <j-box pb="300">
              <j-text>{task?.modelId}</j-text>
              <j-text>{task?.taskId}</j-text>
              <j-text>{task?.systemPrompt}</j-text>
            </j-box>
          </div>
        ))}
      </div>
      {showModels && (
        <j-modal
          size="fullscreen"
          open={showModels}
          onToggle={(e:any) => setShowModels(e.target.open)}
        >
          <j-box px="400" py="400">
            <j-box pb="400">
              <j-text nomargin size="600" color="black" weight="600">
                Models
              </j-text>
            </j-box>
            <div style={listStyle}>
              {Object.values(models).map((model) => (
                <div
                  key={model?.name}
                  style={{...cardStyle, width: "100%"}}
                >
                  <j-box pt="300" pb="400">
                    <j-text variant="heading-sm">{model?.name}</j-text>
                  </j-box>
                  <j-box pb="300">
                    {!model?.downloaded ? <j-text>Downloading: {model?.progress.toFixed(2)}%</j-text> : null}
                    {(model?.downloaded && !model?.loaded) ? <j-text>Loading: {model?.progress}</j-text> : null}
                    <j-text>Model {model.status}</j-text>
                  </j-box>
                </div>
              ))}
            </div>
          </j-box>
        </j-modal>
      )}
    </div>
  )
}

export default Tasks;