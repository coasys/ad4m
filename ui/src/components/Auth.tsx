import { useContext, useEffect, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import { copyTextToClipboard } from "../util";
import { capSentence } from "@perspect3vism/ad4m";

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

  const copyCode = () => {
    copyTextToClipboard(secretCode);

    setCopied(true);

    setTimeout(() => {
      setCopied(false);
      closeSecretCodeModal();
    }, 3000);
  };

  return (
    <div>
      {requestModalOpened && (
        <j-modal
          size="fullscreen"
          open={requestModalOpened}
          onToggle={(e: any) => setRequestModalOpened(e.target.open)}
        >
          <j-box px="500" py="600">
            <j-flex gap="200" direction="column">
              <j-box pb="900">
                <j-text nomargin size="600" color="black" weight="600">
                  Authorize Capabilities
                </j-text>
              </j-box>

              <j-flex gap="500">
                <div>
                  <j-avatar size="xl" src={authInfo.appIconPath}></j-avatar>
                </div>
                <div>
                  <j-text variant="heading-sm">{authInfo.appName}</j-text>
                  <j-text nomargin size="500">
                    {authInfo.appDesc}
                  </j-text>
                </div>
              </j-flex>

              <j-box py="500">
                <j-text weight="800" size="400" uppercase>
                  Wants permissions to:
                </j-text>
                <ul style={{ listStyle: "none", padding: 0 }}>
                  {authInfo.capabilities.map((cap) => {
                    return (
                      <li style={{ display: "flex", gap: "var(--j-space-300" }}>
                        <span>
                          <j-icon
                            size="sm"
                            name="check-circle-fill"
                            color="success-500"
                          ></j-icon>
                        </span>
                        <span>{capSentence(cap)}</span>
                      </li>
                    );
                  })}
                </ul>
              </j-box>
              <j-flex gap="300">
                <j-button variant="link" onClick={closeRequestModal}>
                  Close
                </j-button>
                <j-button variant="primary" onClick={permitCapability}>
                  Confirm
                </j-button>
              </j-flex>
            </j-flex>
          </j-box>
        </j-modal>
      )}

      {secretCodeModalOpened && (
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
                <j-button square slot="end" variant="link" onClick={copyCode}>
                  <j-icon
                    size="sm"
                    name={!copied ? "clipboard" : "clipboard-check"}
                  ></j-icon>
                </j-button>
              </j-input>
              <j-box pt="500">
                <j-button
                  full
                  size="lg"
                  variant=""
                  onClick={closeSecretCodeModal}
                >
                  Close
                </j-button>
              </j-box>
            </div>
          </div>
        </j-modal>
      )}
    </div>
  );
};

export default Auth;
