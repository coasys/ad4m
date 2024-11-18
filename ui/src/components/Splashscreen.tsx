import { getCurrentWebviewWindow } from "@tauri-apps/api/webviewWindow";
import { useState } from "react";
import { useEffect } from "react";

import Logo from "./Logo";
import { splashscreenContainer, splashscreenError, splashscreenErrorFlex } from "./styles";
const appWindow = getCurrentWebviewWindow()

export default function Splashscreen() {
  const [copied, setCopied] = useState(false);

  function copyFile() {
    appWindow.emit("copyLogs");

    setTimeout(() => {
      setCopied(true);

      setTimeout(() => {
        setCopied(false);
      }, 2000);
    }, 500);
  }

  useEffect(() => {
    setTimeout(() => {
      const error = document.getElementById("error");
      if (error) {
        error.style.display = "block";
        error.style.visibility = "visible";
        error.style.opacity = "1";
        error.style.height = "160px";
      }
    }, 30000);
  }, []);

  return (
    <div style={splashscreenContainer}>
      <Logo gradient style={{ width: "100px", height: "100px" }}></Logo>
      <div id="error" style={splashscreenError}>
        <div style={splashscreenErrorFlex}>
          <j-text variant="heading-lg">Whoops, something broke! 😅</j-text>
          <j-text variant="ingress">
            To help us fix this, please click the button below to open your AD4M
            data folder. Please then send the ad4m.log file found there to us on
            Discord.
          </j-text>
          <j-button variant="primary" onClick={copyFile}>
            {copied ? "Opened" : "Open Logs"}
          </j-button>
        </div>
      </div>
    </div>
  );
}
