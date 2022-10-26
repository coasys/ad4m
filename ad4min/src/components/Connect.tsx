import Header from './Header';
import { TextInput, Button, Stack, Loader } from "@mantine/core";
import { showNotification } from "@mantine/notifications";
import { useContext, useState, useEffect } from "react";
import { useNavigate } from "react-router-dom";
import { Ad4minContext } from "../context/Ad4minContext";
import { buildAd4mClient } from "../util";

export function Connect() {
  const {state: {
    connectedLaoding,
    connected,
    isUnlocked
  }, methods: {
    configureEndpoint
  }} = useContext(Ad4minContext);

  const [url, setURL] = useState("");
  const [urlError, setURLError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  let navigate = useNavigate();

  const onUrlChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault();
    const { value } = event.target;
    setURL(value);
  }

  const onInitialize = async () => {
    setLoading(true);
    
    return new Promise(async (resolve, reject) => {      
      if (!url) {
        setURLError('URL is required')
      } else if (!/^(wss?:\/\/)([0-9]{1,3}(?:\.[0-9]{1,3}){3}|[a-zA-Z]+):([0-9]{1,5})(?:\/[a-zA-Z]{0,100})$/.test(url)) {
        setURLError('Invalid websocket URL')
      } else {
        try {
          const client = await buildAd4mClient(url!)

          const id = setTimeout(() => {
            resolve(true)

            showNotification({
              color: 'red',
              message: 'Failed to connect to the endpoint provided'
            });

            setLoading(false)
          }, 2000)
          
          await client.runtime.hcAgentInfos();
  
          clearTimeout(id)
          
          configureEndpoint(url);

          resolve(true);
        } catch(e) {
          showNotification({
            color: 'red',
            message: 'Failed to connect to the endpoint provided'
          })

          reject()
        } finally {
          setLoading(false)
        }
      }
      setLoading(false);
      resolve(true)
    })
  }

  useEffect(() => {
    if (connected && !isUnlocked) {
      navigate('/login');
    }
  }, [connected, isUnlocked, navigate])

  return (
    <Stack align="center" spacing="xl" style={{margin: "auto"}}>
      <Header />
      {connectedLaoding ? (
          <Loader />
        ) : (
          <>
            <TextInput 
              label="Ad4m URL" 
              placeholder='ws://www.example.com/graphql' 
              radius="md" 
              size="md" 
              onChange={onUrlChange}
              required
              error={urlError}
            />
            <Button onClick={onInitialize} loading={loading}>
              Initialize Client
            </Button>
          </>
        )
      }
  </Stack>
  )
}
