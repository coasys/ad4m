import { Agent, Literal } from '@perspect3vism/ad4m';
import { useContext, useEffect, useState } from 'react';
import { PREDICATE_FIRSTNAME, PREDICATE_LASTNAME, PREDICATE_USERNAME } from '../constants/triples';
import { cardStyle, gridButton, MainContainer, MainHeader } from './styles';
import { Ad4minContext } from '../context/Ad4minContext';
import { buildAd4mClient } from '../util';
import { useCallback } from 'react';
import CardItems from './CardItems';
import { showNotification } from '@mantine/notifications';
import { invoke } from '@tauri-apps/api';
import QRCode from 'react-qr-code';
import { AgentContext } from '../context/AgentContext';
import ActionButton from './ActionButton';

type Props = {
  did: String,
  opened: boolean,
  setOpened: (val: boolean) => void
}

export const fetchProfile = async (agent: Agent) => {
  const tempProfile = {
    firstName: "",
    lastName: "",
    username: ""
  }

  for (const { data: {source, predicate, target} } of agent.perspective?.links!) {
    if (source === agent.did) {
      if (predicate === PREDICATE_FIRSTNAME) {
        tempProfile.firstName = Literal.fromUrl(target).get()
      } else if (predicate === PREDICATE_LASTNAME) {
        tempProfile.lastName = Literal.fromUrl(target).get();
      } else if (predicate === PREDICATE_USERNAME) {
        tempProfile.username = Literal.fromUrl(target).get();
      }
    }
  }

  return tempProfile;
}

const Profile = (props: Props) => {
  const {
    state: {
      loading,
    } } = useContext(AgentContext);

  const {state: {
    url,
    did,
    client,
    expertMode
  }, methods: {
    toggleExpertMode
  }} = useContext(Ad4minContext);

  const [trustedAgents, setTrustedAgents] = useState<any[]>([]);

  const [trustedAgentModalOpen, settrustedAgentModalOpen] = useState(false);

  const [clearAgentModalOpen, setClearAgentModalOpen] = useState(false);

  const [proxy, setProxy] = useState('');

  const [qrcodeModal, setQRCodeModal] = useState(false);

  const [password, setPassword] = useState('');
  
  const [profile, setProfile] = useState({
    firstName: "",
    lastName: "",
    username: ""
  });

  const onPasswordChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault();
    let { value } = event.target;
    setPassword(value);
  }

  const getTrustedAgents = useCallback(async () => {
    if (url) {
      const client = await buildAd4mClient(url);
      const trustedAgents = await client!.runtime.getTrustedAgents()
      
      const tempTempAgents = [];
      
      for (const agent of trustedAgents) {
        const fetchedAgent = await client!.agent.byDID(agent)
  
        if (fetchedAgent) {
          const profile = await fetchProfile(fetchedAgent)
    
          tempTempAgents.push({did: agent, ...profile});
        } else {
          tempTempAgents.push({did: agent});
        }
  
      }
  
      setTrustedAgents(tempTempAgents);
    }
  }, [url])

  const fetchCurrentAgentProfile = useCallback(async () => {
    if (url) {
      const client = await buildAd4mClient(url);
      const agent = await client!.agent.me();
  
      const profile = await fetchProfile(agent);
      
      setProfile(profile);
    }
  }, [url])

  useEffect(() => {
    fetchCurrentAgentProfile();
    getTrustedAgents();
  }, [fetchCurrentAgentProfile, getTrustedAgents])

  useEffect(() => {
    const getProxy = async () => {
      const proxy: string = await invoke("get_proxy");
      console.log(proxy);
      setProxy(formatProxy(proxy));
    }
    getProxy().catch(console.error);;
  }, []);

  console.log(trustedAgentModalOpen);

  const formatProxy = (proxy: string) => {
    return proxy.replace(/^https(.*)/, 'wss$1').replace(/^http(.*)/, 'ws$1') + "/graphql";
  }

  const copyProxy = () => {
    navigator.clipboard.writeText(proxy);
    showNotification({
      message: 'Proxy endpoint copied to clipboard',
      autoClose: 1000
    });
  }

  const showProxyQRCode = () => {
    setQRCodeModal(true);
  }

  const clearAgent = async (password: string) => {
    console.log('clearAgent 0', password)
    let agentStatus = await client?.agent.lock(password);
    console.log('clearAgent 1', agentStatus)
    if (!agentStatus?.isUnlocked) {
      await invoke("clear_state");
    }
  }

  const copyUrl = () => {
    navigator.clipboard.writeText(url);
    showNotification({
      message: 'URL copied to clipboard',
      autoClose: 1000
    });
  }

  const setupProxy = async () => {
    try {      
      const proxy: string = await invoke("setup_proxy", { subdomain: did });
      console.log("Finish setup proxy, ", proxy);
      setProxy(formatProxy(proxy));
    } catch (e) {
      showNotification({
        color: 'red',
        message: 'Error while starting proxy',
        autoClose: 5000
      });
    }  
  }

  const stopProxy = async () => {
    await invoke("stop_proxy");
    setProxy('');
  }

  const showProxy = () => {
    return (
      <>
        <ActionButton 
          tooltip={proxy.length === 0 ? "Setup proxy" : "Stop Proxy"}
          onClick={() => proxy.length === 0 ? setupProxy() : stopProxy()}
          icon="wifi"
        />
        {
          proxy && (
            <>
              <ActionButton 
                tooltip="Copy proxy URL"
                onClick={copyProxy}
                icon="clipboard"
              />
              <ActionButton 
                tooltip="Show Proxy QR"
                onClick={showProxyQRCode}
                icon="qr-code-scan"
              />
            </>
          )
        }
      </>
    )
  }

  return (
    <div style={MainContainer}>
      <div style={{padding: '20px 30px 0 30px'}}>
        <j-toggle
        full=""
        checked={expertMode}
        onChange={e => toggleExpertMode()}
        >
          Expert mode
        </j-toggle>
      </div>
      <div style={{...gridButton, paddingTop: 20}}>
        <ActionButton 
          tooltip="Trusted agents"
          onClick={() => settrustedAgentModalOpen(true)}
          icon="shield-check"
        />
        <ActionButton 
          tooltip="Delete Agent"
          onClick={() => setClearAgentModalOpen(true)}
          icon="trash"
        />
        {showProxy()}
        <j-box p="200" />
      </div>
      <j-modal
          size="fullscreen"
          open={trustedAgentModalOpen}
          onToggle={(e: any) => settrustedAgentModalOpen(e.target.open)}
        >
          <j-box p="400">
            <j-flex gap="500" direction="column">
              <j-text nomargin variant="heading-sm">
                Trusted Agents
              </j-text>
              {trustedAgents.map((e, i) => (
                <div key={`trusted-agent-${e.did}`} style={{...cardStyle, marginBottom: 0, width: '85%'}}>
                  <j-flex direction='column' style={{marginTop: 4}}>
                    <j-text weight="bold" >{e?.username || 'No username'}</j-text>
                    <j-flex a="center" j="between">
                      <j-text nomargin variant="body" size="xs">{e?.did.length > 25 ? `${e?.did.substring(0, 25)}...` : e?.did}</j-text>
                      <j-box p="100"></j-box>
                      <j-button size="xs" variant="transparent"  onClick={() => console.log('wow')}>
                        <j-icon size="xs" slot="end" name="clipboard"></j-icon>
                      </j-button>
                    </j-flex>
                  </j-flex>
                </div>
              ))}
            </j-flex>
          </j-box>
      </j-modal>
      <j-modal
        size="fullscreen"
        open={qrcodeModal}
        onToggle={(e: any) => setQRCodeModal(e.target.open)}
        title="Proxy QR Code"
        centered
      >
        <j-box p="400">
          <j-flex gap="200" direction="column">
            <j-text nomargin variant="heading-sm">
            Proxy QR Code
            </j-text>
            <j-box p="200"></j-box>
          <QRCode value={proxy} />
          </j-flex>
        </j-box>
      </j-modal>

      <j-modal
        size="fullscreen"
        open={clearAgentModalOpen}
        onToggle={(e: any) => setClearAgentModalOpen(e.target.open)}
      >
        <j-box p="400">
          <j-flex gap="200" direction="column">
            <j-text nomargin variant="heading-sm">
            Clear Agent
            </j-text>
            <j-box p="200"></j-box>
            <j-text>Warning:By clearing the agent you will loose all the data and will have to start with a fresh agent<br /><br /> Please enter your pass below to proceed</j-text>
            <j-box p="200"></j-box>
            <j-input
              placeholder="Password"
              label="Input your passphrase"
              size="lg"
              required
              onInput={onPasswordChange}
            ></j-input>
            <j-box p="200"></j-box>
              <j-flex>
                <j-button onClick={() => clearAgent(password)} loading={loading}>
                Delete Agent
                </j-button>
              </j-flex>
          </j-flex>
        </j-box>
      </j-modal>
    </div>
  )
}

export default Profile