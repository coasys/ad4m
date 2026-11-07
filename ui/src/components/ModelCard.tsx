import type { AiEndpoint } from "@coasys/ad4m";
import { useState } from "react";
import "../index.css";
import { kindLabel } from "./aiSetup";

/** What a model's status says, in words. */
const STATE_TEXT: Record<string, string> = {
  UNKNOWN: "Not checked yet",
  READY: "Ready",
  MISSING: "The endpoint does not serve this model",
  PULLING: "Downloading",
  ENDPOINT_DOWN: "The endpoint does not answer",
};

export default function ModelCard(props: {
  model: any;
  endpoint?: AiEndpoint;
  editModel: () => void;
  removeModel: () => void;
  setDefaultModel: () => void;
  toggleTask: (modelId: string, taskId: string) => void;
  deleteTask: (modelId: string, taskId: string) => void;
}) {
  const { model, endpoint, editModel, removeModel, setDefaultModel, toggleTask, deleteTask } = props;
  const { id, name, modelType, remoteModel, status, tasks } = model;

  const [showTasks, setShowTasks] = useState(false);

  return (
    <j-box className="box">
      <j-box pb="300">
        <j-flex j="between">
          <j-text variant="heading-sm" size="600" color="primary-800">
            {name}
          </j-text>
          <j-flex gap="300">
            {model.default ? (
              <j-button variant="subtle" size="sm" disabled>
                Default
              </j-button>
            ) : (
              <j-button size="sm" onClick={setDefaultModel}>
                Set Default
              </j-button>
            )}
            <j-button variant="subtle" size="sm" onClick={editModel}>
              <j-icon name="pencil-square" size="sm" />
            </j-button>
            <j-button variant="subtle" size="sm" onClick={removeModel}>
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
            {remoteModel}
          </j-text>
        </j-flex>
        <j-flex gap="300" wrap>
          <j-text nomargin>Endpoint:</j-text>
          <j-text
            nomargin
            color="ui-0"
            style={{ maxWidth: "100%", overflowWrap: "break-word" }}
          >
            {endpoint
              ? `${endpoint.name} (${kindLabel(endpoint.kind)}, ${endpoint.baseUrl})`
              : "Gone"}
          </j-text>
        </j-flex>
        <j-flex gap="300">
          <j-text nomargin>Status:</j-text>
          <j-text nomargin color="ui-0">
            {STATE_TEXT[status?.state] ?? status?.state}
            {status?.detail ? ` (${status.detail})` : ""}
          </j-text>
        </j-flex>
        {modelType === "LLM" && (
          <>
            {!tasks || tasks.length === 0 ? (
              <j-text nomargin>No tasks created yet...</j-text>
            ) : (
              <>
                <j-button
                  variant="subtle"
                  onClick={() => setShowTasks(!showTasks)}
                >
                  {showTasks ? "Hide" : "Show"} tasks ( {tasks.length} )
                  <j-icon name={`chevron-${showTasks ? "up" : "down"}`} />
                </j-button>
                {showTasks && (
                  <j-flex gap="300">
                    {tasks.map((task: any) => (
                      <j-box className="box light">
                        <j-flex direction="column" gap="400">
                          <j-text variant="heading-sm" nomargin>
                            {task.name}
                          </j-text>
                          <j-text nomargin>Id: {task.taskId}</j-text>
                          <j-text
                            nomargin
                            style={{
                              overflow: "hidden",
                              maxHeight: task.collapsed ? 60 : "none",
                            }}
                          >
                            {task.systemPrompt}
                          </j-text>
                          <j-button
                            variant="subtle"
                            onClick={() => toggleTask(id, task.taskId)}
                          >
                            Show {task.collapsed ? "more" : "less"}
                            <j-icon
                              name={`chevron-${task.collapsed ? "down" : "up"}`}
                            />
                          </j-button>
                          <j-button
                            variant="subtle"
                            onClick={() => deleteTask(id, task.taskId)}
                          >
                            Delete
                            <j-icon
                              name={"trash"}
                            />
                          </j-button>
                        </j-flex>
                      </j-box>
                    ))}
                  </j-flex>
                )}
              </>
            )}
          </>
        )}
      </j-flex>
    </j-box>
  );
}
