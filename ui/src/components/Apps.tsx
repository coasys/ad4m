import { useState, useContext, useEffect } from "preact/compat";
import { Ad4minContext } from "../context/Ad4minContext";
import { cardStyle, linkStyle, listStyle, MainContainer } from "./styles";

const Apps = () => {
  const {
    state: { client },
  } = useContext(Ad4minContext);

  const [apps, setApps] = useState<any[] | null[]>([]);
  const [showModal, setShowModal] = useState(false);
  const [selectedRequestId, setSelectedRequestId] = useState("");

  const getApps = async () => {
    const apps = await client!.agent.getApps();

    setApps(apps);
  };

  const removeApps = async (requestId: string) => {
    const apps = await client!.agent.removeApp(requestId);

    setApps(apps);

    setShowModal(false);
  };

  const revokeToken = async (requestId: string) => {
    const apps = await client!.agent.revokeToken(requestId);

    setApps(apps);

    setShowModal(false);
  };

  useEffect(() => {
    getApps();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <div style={MainContainer}>
      {apps.length === 0 ? (
        <div>
          <j-text variant="heading">Welcome to AD4M, here you can manage your connected applications</j-text>
          <br></br>
          <j-text variant="heading-sm">You have no apps connected...</j-text>
          <j-text variant="body">Try Flux <a href="https://app.fluxsocial.io"  target="_blank">here</a> to get started</j-text>
        </div>
      ) : (
        <div style={{ ...listStyle }}>
          {apps.map((app, index) => (
            <div key={`app-${index}`} style={{ ...cardStyle, width: "100%" }}>
              <j-flex a="flex-start" direction="column">
                <j-flex direction="column" style={{ marginTop: 4 }}>
                  <j-text variant="bold" size="600">
                    {app.auth.appName}
                    &nbsp;&nbsp;
                    {app.revoked && <j-badge variant="warning">revoked</j-badge>}
                  </j-text>
                  <j-box p="200"></j-box>
                  <j-text size="400">{app.auth.appDesc}</j-text>
                  <a
                    style={{ ...linkStyle }}
                    href={app.auth.appUrl}
                    target="_blank"
                    rel="noreferrer"
                  >
                    {app.auth.appUrl}
                  </a>
                </j-flex>
                <div style={{ position: "absolute", top: 0, right: 0 }}>
                  <j-button
                    variant="link"
                    onClick={() => {
                      setShowModal(true);
                      setSelectedRequestId(app.requestId);
                    }}
                  >
                    <j-icon name="x" />
                  </j-button>
                </div>
              </j-flex>
            </div>
          ))}
        </div>
      )} 
      {showModal && (
        <j-modal
          size="fullscreen"
          open={showModal}
          onToggle={(e: any) => setShowModal(e.target.open)}
        >
          <j-box px="400" py="600">
            <j-box pb="500">
              <j-text nomargin size="600" color="black" weight="600">
                App actions
              </j-text>
            </j-box>
            <j-text>
              Warning: You can either revoke the apps token which will make sure
              the app using it doesn't have access to your data but the apps
              stays in the list or remove the app completely.
            </j-text>
            <j-box p="200"></j-box>
            <j-box p="200"></j-box>
            <j-flex>
              {!apps.find((app) => app.requestId === selectedRequestId)
                .revoked && (
                <>
                  <j-button
                    variant="primary"
                    onClick={() => revokeToken(selectedRequestId)}
                  >
                    Revoke Token
                  </j-button>
                  <j-box px="400"></j-box>
                </>
              )}
              <j-button
                variant="primary"
                onClick={() => removeApps(selectedRequestId)}
              >
                Remove App
              </j-button>
            </j-flex>
          </j-box>
        </j-modal>
      )}
    </div>
  );
};

export default Apps;
