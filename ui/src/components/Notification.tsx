import { useContext, useEffect, useState } from "react";
import { Ad4minContext } from "../context/Ad4minContext"
import { Notification as NotificationType } from "@coasys/ad4m/lib/src/runtime/RuntimeResolver"; import { PerspectiveProxy } from "@coasys/ad4m";
;

const Notification = ({ notification }: { notification: NotificationType }) => {
  const {
    state: { client },
    methods: { handleNotification },
  } = useContext(Ad4minContext);

  const [requestModalOpened, setRequestModalOpened] = useState(true);
  const [showTriggerCode, setShowTriggerCode] = useState(false);
  const [showPerspectives, setShowPerspectives] = useState(false);
  const [perspectives, setPerspectives] = useState<PerspectiveProxy[]>([]);

  useEffect(() => {
    setRequestModalOpened(true);
    getPerspectives();
  }, [notification]);


  const getPerspectives = async () => {
    const perspectives = await client?.perspective.all();
    const filteredPerspectives = perspectives?.filter((perspective) => notification.perspectiveIds.includes(perspective.uuid));
    setPerspectives(filteredPerspectives);
  }

  const permitNotification = async () => {
    // @ts-ignore
    let result = await client!.runtime.grantNotification(notification?.id);

    console.log(`permit result: ${result}`);

    // @ts-ignore
    handleNotification(notification);

    closeRequestModal();
  };

  const closeRequestModal = () => {
    setRequestModalOpened(false);
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
            <j-flex direction="column">
              <j-box pb="400">
                <j-text nomargin size="600" color="black" weight="600">
                  App wants to register a Notification trigger
                </j-text>
                <br></br>
                <j-text nomargin>ADAM will create system notifications when trigger condition is met</j-text>
              </j-box>

              <j-flex gap="500">
                <div>
                  <j-avatar size="xl" src={notification?.appIconPath}></j-avatar>
                </div>
                <div>
                  <j-text variant="heading-sm">{notification?.appName}</j-text>
                  <j-text nomargin size="500">
                    {notification?.description}
                  </j-text>
                </div>
              </j-flex>
              <j-flex gap="200" direction="column">
                <j-box pb="100" style={{ cursor: 'pointer' }} onClick={() => setShowPerspectives(!showPerspectives)}>
                  <j-flex a="center" gap="200">
                    <j-text>
                      Perspectives
                    </j-text>
                    <j-icon name={showPerspectives ? "chevron-down" : "chevron-right"} size="sm" />
                  </j-flex>
                  {showPerspectives && (
                    <j-box pl="400" pt="200">
                      <ul>
                        {perspectives?.map((perspective) => (
                          <li key={perspective.uuid}>
                            <j-text nomargin size="500">
                              {perspective.name}
                            </j-text>
                          </li>
                        ))}
                      </ul>
                    </j-box>
                  )}
                </j-box>
                <j-box pb="200" style={{ cursor: 'pointer' }} onClick={() => setShowTriggerCode(!showTriggerCode)}>
                <j-flex a="center" gap="200">
                  <j-text>
                    Notification trigger Prolog code
                  </j-text>
                  <j-icon name={showTriggerCode ? "chevron-down" : "chevron-right"} size="sm" />
                </j-flex>
                {showTriggerCode && (
                  <j-box pl="400" pt="200">
                    <j-text>
                      {notification?.trigger}
                    </j-text>
                  </j-box>
                )}
              </j-box>
                {
                  notification?.webhookUrl && (
                    <div>
                      <j-flex>
                        <j-text>
                          Webhook URL: {notification?.webhookUrl}
                        </j-text>
                      </j-flex>
                      {notification?.webhookUrl.startsWith("http://push-notifications.ad4m.dev") ? (
                        <p style="height: 60px; color: green; font-size: 14px; margin: 0; ">
                          Trusted AD4M Push Notification relay
                        </p>
                      ) : (
                        <p style="height: 60px; color: red; font-size: 14px; margin: 0; ">
                          Caution: This notification will be sent to the above URL and the data can be leaked outside of the app. Please make sure you trust the app.
                        </p>
                      )
                      }
                    </div>
                  )
                }
              </j-flex>

              <j-flex gap="300">
                <j-button variant="link" onClick={closeRequestModal}>
                  Close
                </j-button>
                <j-button variant="primary" onClick={permitNotification}>
                  Confirm
                </j-button>
              </j-flex>            
            </j-flex>
          </j-box>
        </j-modal>
      )}
    </div>
  );
};

export default Notification;
