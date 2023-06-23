import React, { useEffect, useState } from 'react'
import { splashscreenContainer, splashscreenError, splashscreenErrorFlex } from "./styles";
import { appWindow } from '@tauri-apps/api/window';

export default function TrayMessage() {
  const [count, setCount] = useState(5);

  useEffect(() => {
    appWindow.listen('tray_message_open', () => {  
      let i = 5;    
      const interval = setInterval(() => {
        if (count === 0) {
          clearInterval(interval)
        } else {
          i -= 1;
          setCount(i)
        }
  
      }, 1000)
    })
  }, [])

  return (
    <div style={splashscreenContainer}>
      <div style={{padding: "20px 20px 0 20px"}}>
        <j-text size="400" variant="ingress">
          Ad4m launcher is minimized, click on the tray icon to open it.
        </j-text>
        <j-text size="300"  variant="ingress">
          This popup will automatically close in {count}
        </j-text>
      </div>
  </div>
  )
}
