import { Title, Text, Button, createStyles, Space, Image } from '@mantine/core'
import { appWindow } from '@tauri-apps/api/window';
import React from 'react'
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
      background: 'linear-gradient(90deg, rgba(2,0,36,1) 0%, rgba(38,3,23,1) 41%, rgba(51,4,31,1) 100%)',
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
      <Title order={1} style={{
            fontSize: 90
          }}>
            <Image style={{width: '300px'}} src="ad4msquarelogo2_white_colouremblem.png"></Image>
      </Title>
      <div id="error" className={classes.error}>
        <div className={classes.errorFlex}>
          <Text align="center" size='xl' weight={700} style={{
            fontSize: 40,
            fontFamily: 'comfortaa',
          }} >Whoops, something broke! 😅</Text>
          <Space h="md" />
          <Text align="center" size='lg' style={{
            fontSize: 22,
            fontFamily: 'comfortaa',
          }}>
            If you have also been asked to include a log file with your report,
            click the button below to copy a log file to your desktop:
          </Text>
          <Space h="md" />
          <Button onClick={copyFile}>{copied ? "Copied" : "Copy"}</Button>
        </div>
      </div>
    </div>
  )
}
