import { useContext, useEffect, useState } from 'react';
import { Ad4minContext } from '../context/Ad4minContext';
import { showNotification } from '@mantine/notifications';

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
      <j-flex
        direction="column"
        gap="200"
        a="flex-start"
      >
        {
          capabilities.map(cap => <j-text>{`${cap.can} => ${cap.with.domain}.${cap.with.pointers}`}</j-text>)
        }
      </j-flex>

    )
  }

  const copyCode = () => {
    navigator.clipboard.writeText(secretCode);
    showNotification({
      message: 'Secret code copied to clipboard',
      autoClose: 1000
    });
  }

  return (
    <div>
      <j-modal
        size="lg"
        open={requestModalOpened}
        onToggle={(e: any) => setRequestModalOpened(e.target.open)}
      >
        <j-box p="400">
          <j-flex gap="200" direction="column">
            <j-text nomargin variant="heading-sm">
            Request Capabilities
            </j-text>
            <j-box p="200"></j-box>
            <j-input
              label="App Name"
              size="lg"
              disabled
              value={authInfo.appName}
            ></j-input>
            <j-input
              label="App Description"
              size="lg"
              disabled
              value={authInfo.appDesc}
            ></j-input>
            <j-input
              label="App URL"
              size="lg"
              disabled
              value={authInfo.appUrl}
            ></j-input>
            <j-text>Request for these capabilities,</j-text>
            {showCapabilities(authInfo.capabilities)}
            <j-box p="200"></j-box>
              <j-flex>
                <j-button onClick={closeRequestModal}>
                Close
                </j-button>
                <j-box p="200"></j-box>
                <j-button onClick={permitCapability} >
                Confirm
                </j-button>
              </j-flex>
          </j-flex>
        </j-box>
      </j-modal>

      <j-modal
        size="lg"
        open={secretCodeModalOpened}
        onToggle={(e: any) => setSecretCodeModalOpened(e.target.open)}
      >
        <j-box p="400">
          <j-flex gap="200" direction="column">
            <j-flex a="center" gap="200">
              <j-text nomargin variant="heading-sm">
              {secretCode}
                <j-Button variant="subtle" onClick={copyCode}>
                  <j-icon size="sm" name="clipboard"></j-icon>
                </j-Button>
              </j-text>
            </j-flex>
            <j-box p="200"></j-box>
              <j-flex>
                <j-button onClick={closeSecretCodeModal}>
                Close
                </j-button>
              </j-flex>
          </j-flex>
        </j-box>
      </j-modal>
    </div>
  )
}

export default Auth
