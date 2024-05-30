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
            <j-flex gap="200" direction="column">
              <j-box pb="900">
                <j-text nomargin size="600" color="black" weight="600">
                  Authorize Notification
                </j-text>
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
                <div>
                  <j-text nomargin size="500">
                    Perspectives:
                  </j-text>
                  <ul>
                    {perspectives?.map((perspective) => (
                      <li key={perspective.uuid}>
                        <j-text nomargin size="500">
                          {perspective.name}
                        </j-text>
                      </li>
                    ))}
                  </ul>
                </div>
                <div>
                  <j-flex>
                    <j-text>
                      Notification Trigger: {notification?.trigger}
                    </j-text>
                  </j-flex>
                </div>
                <div>
                  <j-flex>
                    <j-text>
                      Webhook URL: {notification?.webhookUrl}
                    </j-text>
                  </j-flex>
                  <j-text color="danger" variant="caption">

                  </j-text>
                  <p style="height: 60px; color: red; font-size: 14px; margin: 0; ">
                    Caution: This notification will be sent to the above URL and the data can be leaked outside of the app. Please make sure you trust the app.
                  </p>
                </div>
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
