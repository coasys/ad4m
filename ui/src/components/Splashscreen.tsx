import { getCurrentWebviewWindow } from "@tauri-apps/api/webviewWindow";
import { useEffect, useState } from "react";
import "../index.css";
import Logo from "./Logo";

const appWindow = getCurrentWebviewWindow();

export default function Splashscreen() {
  const [copied, setCopied] = useState(false);
  const [error, setError] = useState(false);

  function copyFile() {
    appWindow.emit("copyLogs");

    setTimeout(() => {
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    }, 500);
  }

  useEffect(() => {
    setTimeout(() => setError(true), 30000);
  }, []);

  return (
    <div className="wrapper">
      <Logo style={{ marginBottom: 40 }} />
      {error ? (
        <j-box style={{ maxWidth: 500, margin: "0 20px" }}>
          <j-flex direction="column" gap="500" a="center">
            <j-text
              variant="heading-lg"
              nomargin
              style={{ textAlign: "center" }}
            >
              Whoops, something broke! 😅
            </j-text>
            <j-text variant="ingress" nomargin style={{ textAlign: "center" }}>
              To help us fix this, please click the button below to open your
              AD4M data folder. Please then send the ad4m.log file found there
              to us on Discord.
            </j-text>
            <j-button
              variant="primary"
              onClick={copyFile}
              style={{ marginTop: 20 }}
            >
              {copied ? "Opened" : "Open Logs"}
            </j-button>
          </j-flex>
        </j-box>
      ) : (
        <j-flex a="center" gap="400">
          <j-text size="700" nomargin>
            Loading...
          </j-text>
          <j-spinner size="sm" />
        </j-flex>
      )}
    </div>
  );
}
