import { useContext, useEffect, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import { showNotification } from "@mantine/notifications";

interface Capability {
  with: Resource;
  can: string[];
}

interface Resource {
  domain: string;
  pointers: string[];
}

const Auth = () => {
  const {
    state: { client, auth },
  } = useContext(Ad4minContext);

  const [requestModalOpened, setRequestModalOpened] = useState(true);
  const [secretCodeModalOpened, setSecretCodeModalOpened] = useState(false);
  const [secretCode, setSecretCode] = useState("");
  const [copied, setCopied] = useState(false);

  useEffect(() => {
    setRequestModalOpened(true);
  }, [auth]);

  const authInfo = JSON.parse(auth).auth;

  const permitCapability = async () => {
    let result = await client!.agent.permitCapability(auth);

    console.log(`permit result: ${result}`);

    closeRequestModal();

    setSecretCode(result);
    setSecretCodeModalOpened(true);
  };

  const closeRequestModal = () => {
    setRequestModalOpened(false);
  };

  const closeSecretCodeModal = () => {
    setSecretCodeModalOpened(false);
  };

  const showCapabilities = (capabilities: Capability[]) => {
    return (
      <j-flex direction="column" gap="200" a="flex-start">
        {capabilities.map((cap) => (
          <j-text>{`${cap.can} => ${cap.with.domain}.${cap.with.pointers}`}</j-text>
        ))}
      </j-flex>
    );
  };

  const copyCode = () => {
    navigator.clipboard.writeText(secretCode);

    setCopied(true);

    setTimeout(() => {
      setCopied(false);
    }, 2000);

    showNotification({
      message: "Secret code copied to clipboard",
      autoClose: 1000,
    });
  };

  return (
    <div>
      <j-modal
        size="fullscreen"
        open={requestModalOpened}
        onToggle={(e: any) => setRequestModalOpened(e.target.open)}
      >
        <j-box px="400" py="600">
          <j-flex gap="200" direction="column">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                Authorize Capabilities
              </j-text>
            </j-box>
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
              <j-button variant="link" onClick={closeRequestModal}>
                Close
              </j-button>
              <j-box p="200"></j-box>
              <j-button variant="primary" onClick={permitCapability}>
                Confirm
              </j-button>
            </j-flex>
          </j-flex>
        </j-box>
      </j-modal>

      <j-modal
        size="fullscreen"
        open={secretCodeModalOpened}
        onToggle={(e: any) => setSecretCodeModalOpened(e.target.open)}
      >
        <div
          className="center"
          style={{
            height: "100%",
            width: "100%",
            maxWidth: "500px",
            paddingLeft: "var(--j-space-400)",
            paddingRight: "var(--j-space-400)",
          }}
        >
          <div>
            <j-text size="700" color="black" weight="600">
              Verification Code
            </j-text>
            <j-input
              className="one-time-pass"
              label="Go back to your app to verify"
              readonly
              size="xl"
              value={secretCode}
            >
              <j-button square slot="end" variant="subtle" onClick={copyCode}>
                <j-icon size="sm" name={!copied ? 'clipboard' : 'clipboard-check'}></j-icon>
              </j-button>
            </j-input>
            <j-button variant="primary" onClick={closeSecretCodeModal}>
              Close
            </j-button>
          </div>
        </div>
      </j-modal>
    </div>
  );
};

export default Auth;
