import { createStyles, Image } from '@mantine/core'
import { appWindow } from '@tauri-apps/api/window';
import { useState } from 'react';
import { useEffect } from 'react';

const useStyles = createStyles((theme, _params, getRef) => {
  return {
    container: {
      display: 'flex',
      alignItems: 'center',
      justifyContent: 'center',
      flexDirection: 'column',
      height: '100vh',
      width: '100vw',
      color: 'white',
      fontFamily: 'comfortaa'
    },
    error: {
      padding: '40px 80px',
      visibility: 'collapse',
      opacity: 0,
      transition: 'visibility 0s, opacity 0.5s linear, height 1s',
      height: 0,
      fontFamily: 'comfortaa'
    },
    errorFlex: {
      display: 'flex',
      flexDirection: 'column',
      alignItems: 'center',
      fontFamily: 'comfortaa'
    }
  };
});

export default function Splashscreen() {
  const { classes } = useStyles();
  const [copied, setCopied] = useState(false);

  function copyFile() {
    appWindow.emit('copyLogs')

    setTimeout(() => {
      setCopied(true);

      setTimeout(() => {
        setCopied(false);
      }, 2000);
    }, 500);
  }

  useEffect(() => {
    setTimeout(() => {
      const error = document.getElementById('error');
      if (error) {
        error.style.display = 'block'
        error.style.visibility = 'visible'
        error.style.opacity = '1'
        error.style.height = '160px'
      }
    }, 10000);
  }, [])

  return (
    <div className={classes.container}>
      <Image style={{width: '200px'}} src="ad4msquarelogo2_white_colouremblem.png"></Image>
      <div id="error" className={classes.error}>
        <div className={classes.errorFlex}>
          <j-text variant="heading-lg">Whoops, something broke! 😅</j-text>
          <j-text variant="ingress">To help us fix this, please click the button below to open your AD4M data folder. Please then send the ad4min.log file found there to us on Discord.</j-text>
          <j-button variant="primary" onClick={copyFile}>{copied ? "Opened" : "Open Logs"}</j-button>
        </div>
      </div>
    </div>
  )
}
