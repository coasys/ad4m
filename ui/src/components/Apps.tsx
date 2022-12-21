import { useState, useContext, useEffect } from 'preact/compat';
import { Ad4minContext } from '../context/Ad4minContext';
import {  cardStyle, listStyle, MainContainer } from './styles';


const Apps = () => {
  const {state: {
    client
  }} = useContext(Ad4minContext);
  
  const [apps, setApps] = useState<any[] | null[]>([]);

  const getApps = async () => {
    const apps = await client!.agent.getApps();

    setApps(apps);
  }

  useEffect(() => {
    getApps();
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])
  
  return (
    <div style={MainContainer}>
      <div 
        style={{...listStyle, height: 590}}
      >        
        {
          apps.map((app, index) => (
            <div key={`app-${index}`} style={{...cardStyle, width: '100%'}}>
              <j-flex a="flex-start" direction="column">
                <j-flex direction='column' style={{marginTop: 4}}>
                  <j-text variant="bold" size="600">{app.auth.appName}</j-text>
                  <j-box p="200"></j-box>
                  <j-text  size="400">{app.auth.appDesc}</j-text>
                  <a href={app.auth.appUrl} target="_blank" size="400">
                  <j-text variant="link" >{app.auth.appUrl}</j-text>
                    </a>
                </j-flex>
              </j-flex>
            </div>
          ))
        }
      </div>
    </div>
  )
}

export default Apps