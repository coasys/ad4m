import { ActionIcon, Burger, Button, Center, Container, createStyles, Group, Image, MediaQuery, Modal, PasswordInput, Space, Stack, Text, Title } from '@mantine/core';
import { showNotification } from '@mantine/notifications';
import { useContext, useEffect, useState } from 'react';
import { Copy, Qrcode as QRCodeIcon } from 'tabler-icons-react';
import { Ad4minContext } from '../context/Ad4minContext';
import { AgentContext } from '../context/AgentContext';
import { MainContainer, MainHeader } from './styles';
import { invoke } from '@tauri-apps/api';
import QRCode from 'react-qr-code';

type Props = {
  opened: boolean,
  setOpened: (val: boolean) => void
}

const useStyles = createStyles((theme) => ({
  label: {
    color: theme.colors.dark[1]
  },
}));

function Settings(props: Props) {
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
    <Container
      style={MainContainer}
    >
      <div style={MainHeader}>
        <div style={{display: 'flex'}}>          
          <MediaQuery largerThan="sm" styles={{ display: 'none' }}>
              <Burger
                opened={props.opened}
                onClick={() => props.setOpened(!props.opened)}
                size="sm"
                color={'#fff'}
                mr="xl"
              />
            </MediaQuery>
          <Image src="ad4mlogo_white_angle2_colouremblem.png"></Image>
        </div>
      </div>
      
      <Stack style={{
        padding: '20px'
      }}>
        <Group align="center" style={{}}>
          <Text size="lg" weight={700}>Connected executor URL: </Text>
          <span>{url}</span>
          <ActionIcon onClick={copyUrl}>
            <Copy />
          </ActionIcon>
        </Group>
        <Button style={{ width: '160px' }} onClick={() => setLockAgentModalOpen(true)}>Lock Agent</Button>
        <Button style={{ width: '160px' }} onClick={() => setClearAgentModalOpen(true)}>Delete Agent</Button>
        <Button style={{ width: '160px' }} onClick={() => invoke("close_application")}>Poweroff AD4Min</Button>
        {showProxy()}
      </Stack>
      <Modal
        opened={lockAgentModalOpen}
        onClose={() => setLockAgentModalOpen(false)}
        title="Lock Agent"
        size={700}
        style={{ zIndex: 100 }}
      >
        <PasswordInput
          placeholder="Password"
          label="Input your passphrase"
          radius="md"
          size="md"
          required
          onChange={onPasswordChange}
          classNames={{
            label: classes.label
          }}
        />
        <Space h={20} />
        <Button onClick={() => lockAgent(password)} loading={loading}>
          Lock agent
        </Button>
      </Modal>
      <Modal
        opened={clearAgentModalOpen}
        onClose={() => setClearAgentModalOpen(false)}
        title="Clear Agent"
        size={700}
        style={{ zIndex: 100 }}
      >
        <Text size="sm"><span style={{color: 'red'}}>Warning:</span> By clearing the agent you will loose all the data and will have to start with a fresh agent.<br /><br /> Please enter your pass below to proceed.</Text>
        <Space h="md" />
        <PasswordInput
          placeholder="Password"
          label="Input your passphrase"
          radius="md"
          size="md"
          required
          onChange={onPasswordChange}
          classNames={{
            label: classes.label
          }}
        />
        <Space h={20} />
        <Button onClick={() => clearAgent(password)} loading={loading}>
          Delete Agent
        </Button>
      </Modal>
      <Modal
        opened={qrcodeModal}
        onClose={() => setQRCodeModal(false)}
        title="Proxy QR Code"
        centered
      >
        <Center>
          <QRCode value={proxy} />
        </Center>
      </Modal>
    </Container>
  )
}

export default Settings