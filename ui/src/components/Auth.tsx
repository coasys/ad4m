import { Button, Group, Modal, Space, Stack, Text, TextInput, List, ThemeIcon } from '@mantine/core';
import { useContext, useEffect, useState } from 'react';
import { Ad4minContext } from '../context/Ad4minContext';
import { CircleCheck } from 'tabler-icons-react';

interface Capability {
  with: Resource,
  can: string[],
}

interface Resource {
  domain: string,
  pointers: string[],
}

const Auth = () => {
  const {
    state: {
      client,
      auth,
    }
  } = useContext(Ad4minContext);

  const [requestModalOpened, setRequestModalOpened] = useState(true);
  const [secretCodeModalOpened, setSecretCodeModalOpened] = useState(false);
  const [secretCode, setSecretCode] = useState("");

  useEffect(() => {
    setRequestModalOpened(true);
  }, [auth])

  const authInfo = JSON.parse(auth).auth;

  const permitCapability = async () => {
    let result = await client!.agent.permitCapability(auth);

    console.log(`permit result: ${result}`);

    closeRequestModal();

    setSecretCode(result);
    setSecretCodeModalOpened(true);
  }

  const closeRequestModal = () => {
    setRequestModalOpened(false)
  }

  const closeSecretCodeModal = () => {
    setSecretCodeModalOpened(false)
  }

  const showCapabilities = (capabilities: Capability[]) => {
    return (
      <List
        spacing="xs"
        size="sm"
        center
        icon={
          <ThemeIcon color="teal" size={24} radius="xl">
            <CircleCheck size={16} />
          </ThemeIcon>
        }
      >
        {
          capabilities.map(cap => <List.Item>{`${cap.can} => ${cap.with.domain}.${cap.with.pointers}`}</List.Item>)
        }
      </List>

    )
  }

  return (
    <div>
      <Modal
        size="lg"
        opened={requestModalOpened}
        onClose={closeRequestModal}
        title="Request Capabilities"
      >
        <Stack>
          <TextInput
            value={authInfo.appName}
            label="App Name"
            disabled
          />
          <TextInput
            value={authInfo.appDesc}
            label="App Description"
            disabled
          />
          <TextInput
            value={authInfo.appUrl}
            label="App URL"
            disabled
          />
          <Text>Request for these capabilities,</Text>
          {showCapabilities(authInfo.capabilities)}
          <Group>
            <Button variant="outline" onClick={closeRequestModal}>
              Close
            </Button>
            <Space h="md" />
            <Button onClick={permitCapability}>
              Confirm
            </Button>
          </Group>
        </Stack>
      </Modal>

      <Modal
        size="lg"
        opened={secretCodeModalOpened}
        onClose={closeSecretCodeModal}
        title="Secret Code"
      >
        <Text weight={700} size="lg" underline>{secretCode}</Text>
        <Space h="md" />
        <Group position="center">
          <Button onClick={closeSecretCodeModal}>
            Close
          </Button>
        </Group>
      </Modal>
    </div>
  )
}

export default Auth
