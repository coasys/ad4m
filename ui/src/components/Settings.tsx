import { ActionIcon, Button, createStyles, Group, Space, Text } from '@mantine/core';
import { showNotification } from '@mantine/notifications';
import { useCallback, useContext, useEffect, useState } from 'react';
import { Copy, Qrcode as QRCodeIcon } from 'tabler-icons-react';
import { Ad4minContext } from '../context/Ad4minContext';
import { AgentContext } from '../context/AgentContext';
import { invoke } from '@tauri-apps/api';
import QRCode from 'react-qr-code';
import { buildAd4mClient } from '../util';
import { fetchProfile } from './Profile';

const useStyles = createStyles((theme) => ({
  label: {
    color: theme.colors.dark[1]
  },
}));

function Settings() {
  const { classes } = useStyles();

  const {
    state: {
      loading,
    },
    methods: {
      lockAgent
    } } = useContext(AgentContext);

  const {
    state: {
      url,
      did,
      client
    } } = useContext(Ad4minContext);

  const [password, setPassword] = useState('');
  const [lockAgentModalOpen, setLockAgentModalOpen] = useState(false);
  const [clearAgentModalOpen, setClearAgentModalOpen] = useState(false);
  const [proxy, setProxy] = useState('');
  const [qrcodeModal, setQRCodeModal] = useState(false);
  const [profile, setProfile] = useState({
    firstName: "",
    lastName: "",
    username: ""
  });


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
  }, [fetchCurrentAgentProfile])

  useEffect(() => {
    const getProxy = async () => {
      const proxy: string = await invoke("get_proxy");
      console.log(proxy);
      setProxy(formatProxy(proxy));
    }
    getProxy().catch(console.error);;
  }, []);

  const onPasswordChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    event.preventDefault();
    let { value } = event.target;
    setPassword(value);
  }

  const copyUrl = () => {
    navigator.clipboard.writeText(url);
    showNotification({
      message: 'URL copied to clipboard',
      autoClose: 1000
    });
  }

  const setupProxy = async () => {
    const proxy: string = await invoke("setup_proxy", { subdomain: did });
    console.log("Finish setup proxy, ", proxy);
    setProxy(formatProxy(proxy));
  }

  const stopProxy = async () => {
    await invoke("stop_proxy");
    setProxy('');
  }

  const formatProxy = (proxy: string) => {
    return proxy.replace(/^https(.*)/, 'wss$1').replace(/^http(.*)/, 'ws$1') + "/graphql";
  }

  const clearAgent = async (password: string) => {
    console.log('clearAgent 0', password)
    let agentStatus = await client?.agent.lock(password);
    console.log('clearAgent 1', agentStatus)
    if (!agentStatus?.isUnlocked) {
      await invoke("clear_state");
    }
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

  const showProxy = () => {
    if (proxy) {
      return (
        <div>
          <Group align="center" style={{}}>
            <Text size="lg" weight={700}>Proxy endpoint: </Text>
            <span>{proxy}</span>
            <ActionIcon onClick={copyProxy}>
              <Copy />
            </ActionIcon>
            <ActionIcon onClick={showProxyQRCode}>
              <QRCodeIcon />
            </ActionIcon>
          </Group>
          <Space h="md" />
          <Button style={{ width: '160px' }} onClick={stopProxy}>Stop Proxy</Button>
        </div>
      )
    } else {
      return (
        <Button style={{ width: '160px' }} onClick={setupProxy}>Setup Proxy</Button>
      )
    }
  }

  return (
    <div
    > 
      <j-popover placement="top" event="contextmenu">
        <j-button slot="trigger" variant="ghost" size="sm">
          <j-flex a="center">
            {profile.username}
            <j-box p="200"></j-box>
            <j-icon size="xs" name="chevron-down"></j-icon>
          </j-flex>
        </j-button>
        <div slot="content">
          <j-menu-item onClick={() => setLockAgentModalOpen(true)}>
            Lock Agent
            <j-icon size="xs" slot="start" name="lock"></j-icon>
          </j-menu-item>
          <j-menu-item onClick={() => setClearAgentModalOpen(true)}>
            Delete Agent
            <j-icon size="xs" slot="start" name="trash"></j-icon>
          </j-menu-item>
          <j-menu-item onClick={() => invoke("close_application")}>
            Poweroff Agent
            <j-icon size="xs" slot="start" name="x-circle"></j-icon>
          </j-menu-item>
          <j-menu-item>
            Setup Proxy
            <j-icon size="xs" slot="start" name="wifi"></j-icon>
          </j-menu-item>
        </div>
      </j-popover>
      <j-modal
        size="lg"
        open={lockAgentModalOpen}
        onToggle={(e: any) => setLockAgentModalOpen(e.target.open)}
      >
        <j-box p="400">
          <j-flex gap="200" direction="column">
            <j-text nomargin variant="heading-sm">
            Lock Agent
            </j-text>
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
                <j-button onClick={() => lockAgent(password)} loading={loading}>
                Lock agent
                </j-button>
              </j-flex>
          </j-flex>
        </j-box>
      </j-modal>
      <j-modal
        size="lg"
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
      <j-modal
        size="lg"
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
    </div>
  )
}

export default Settings