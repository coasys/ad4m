import { useContext, useEffect, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext";
import { showNotification } from "@mantine/notifications";
import { copyTextToClipboard } from "../util";

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
          <j-text>{`${mapCanCapToString(cap.can)} ${mapDomainCapToString(cap.with.domain)} with specific access to: ${mapPointerCapToString(cap.with.pointers)}`}</j-text>
        ))}
      </j-flex>
    );
  };

  const mapCanCapToString = (cans: string[]) => {
    let out = [];
    for (const can of cans) {
      switch (can) {
        case "*":
          out.push("Access all: ");
          break;
        case "READ":
          out.push("Read your: ");
          break;
        case "CREATE":
          out.push("Create: ");
          break;
        case "UPDATE":
          out.push("Update: ");
          break;
        case "DELETE":
          out.push("Delete: ");
          break;
        case "SUBSCRIBE":
          out.push("Subscribe to: ");
          break;
        default:
          out.push("");
          break;
      }
    }
    if (out.length > 1) {
      out = out.map(val => val.replace(": ", ""));
      out[out.length - 1] = out[out.length - 1] + ": ";
    }
    let outString = out.join(" And ");
    return outString;
  }

  const mapDomainCapToString = (domain: string) => {
    switch (domain) {
      case "*":
          return "actions";
      case "agent":
          return "agent actions";
      case "expression":
          return "expressions";
      case "language":
          return "languages";
      case "perspective":
          return "perspectives";
      case "neighbourhood":
          return "neighbourhoods";
      case "runtime":
          return "runtime";
      case "runtime.trusted_agents":
          return "trusted agents";
      case "runtime.known_link_languages":
          return "known link languages";
      case "runtime.friends":
        return "friends";
      case "runtime.messages":
        return "messages";
    }
    return domain;
  }

  const mapPointerCapToString = (pointers: string[]) => {
    let out = [];
    for (const pointer of pointers) {
      switch (pointer) {
        case "*":
          out.push("all");
          break;
        default:
          out.push("unknown");
          break;
      }
    }
    let outString = out.join(" and ");
    return outString;
  }
        
  const copyCode = () => {
    copyTextToClipboard(secretCode);

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
            <j-text>Wants permissions to: </j-text>
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
              <j-button square slot="end" variant="link" onClick={copyCode}>
                <j-icon
                  size="sm"
                  name={!copied ? "clipboard" : "clipboard-check"}
                ></j-icon>
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
