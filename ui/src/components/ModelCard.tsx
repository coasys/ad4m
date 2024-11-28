import { useState } from "react";
import "../index.css";
import { cardStyle } from "./styles";

export default function ModelCard(props: {
  model: any;
  editModel: () => void;
  deleteModel: () => void;
}) {
  const { model, editModel, deleteModel } = props;
  const {
    name,
    modelType,
    api,
    local,
    status,
    downloaded,
    loaded,
    progress,
    tasks,
  } = model;

  const [showTasks, setShowTasks] = useState(false);

  function statusText() {
    if (!status) return "Checking status...";
    else if (downloaded && loaded) return "Ready";
    return `${downloaded ? "Loading" : "Downloading"}: ${progress?.toFixed(2) || 0}%`;
  }

  return (
    <j-box className="box">
      <j-box pb="300">
        <j-flex j="between">
          <j-text size="700" nomargin color="ui-0">
            {name}
          </j-text>
          <j-flex gap="300">
            <j-button size="sm" onClick={editModel}>
              <j-icon name="pencil-square" size="sm" />
            </j-button>
            <j-button size="sm" onClick={deleteModel}>
              <j-icon name="trash" size="sm" />
            </j-button>
          </j-flex>
        </j-flex>
      </j-box>
      <j-flex direction="column" gap="400">
        <j-flex gap="300">
          <j-text nomargin>Type:</j-text>
          <j-text nomargin color="ui-0">
            {modelType}
          </j-text>
        </j-flex>
        <j-flex gap="300">
          <j-text nomargin>Model:</j-text>
          <j-text nomargin color="ui-0">
            {api ? "External API" : local.fileName}
          </j-text>
        </j-flex>
        {api && (
          <>
            <j-flex gap="300">
              <j-text nomargin>API URL:</j-text>
              <j-text nomargin color="ui-0">
                {api.baseUrl}
              </j-text>
            </j-flex>
            <j-flex gap="300">
              <j-text nomargin>API Key:</j-text>
              <j-text nomargin color="ui-0">
                {api.apiKey}
              </j-text>
            </j-flex>
          </>
        )}
        <j-flex gap="300">
          <j-text nomargin>Status:</j-text>
          <j-text nomargin color="ui-0">
            {statusText()}
          </j-text>
        </j-flex>
        {tasks.length === 0 ? (
          <j-text nomargin>No tasks created</j-text>
        ) : (
          <>
            <j-button onClick={() => setShowTasks(!showTasks)}>
              {showTasks ? "Hide" : "Show"} tasks ({tasks.length})
            </j-button>
            {showTasks && (
              <j-flex gap="300">
                {tasks.map((task: any) => (
                  <div
                    key={task.taskId}
                    style={{ ...cardStyle, width: "100%" }}
                  >
                    <j-flex direction="column" gap="400">
                      <j-text variant="heading-sm" nomargin>
                        Task name: {task.name}
                      </j-text>
                      <j-text nomargin>Id: {task.taskId}</j-text>
                      <j-text nomargin>Prompt: {task.systemPrompt}</j-text>
                    </j-flex>
                  </div>
                ))}
              </j-flex>
            )}
          </>
        )}
      </j-flex>
    </j-box>
  );
}
