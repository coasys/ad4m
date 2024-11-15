import { Ad4mClient, ExceptionType } from "@coasys/ad4m";
import { sendNotification } from "@tauri-apps/plugin-notification";
import { ExceptionInfo, Notification as NotificationType } from "@coasys/ad4m/lib/src/runtime/RuntimeResolver";
import { createContext, useCallback, useEffect, useState } from "react";
import {
  buildAd4mClient,
  getForVersion,
  setForVersion,
  removeForVersion,
} from "../util";
import { getCurrentWebviewWindow } from "@tauri-apps/api/webviewWindow";
import { invoke } from "@tauri-apps/api/core";
const appWindow = getCurrentWebviewWindow()

type State = {
  url: string;
  did: string;
  isInitialized: Boolean;
  isUnlocked: Boolean;
  loading: boolean;
  client: Ad4mClient | null;
  candidate: string;
  auth: string;
  connected: boolean;
  connectedLaoding: boolean;
  expertMode: boolean;
  notifications: NotificationType[];
};

type ContextProps = {
  state: State;
  methods: {
    configureEndpoint: (str: string) => void;
    resetEndpoint: () => void;
    handleTrustAgent: (str: string) => void;
    handleLogin: (client: Ad4mClient, login: Boolean, did: string) => void;
    toggleExpertMode: () => void;
    handleNotification: (notification: NotificationType) => void;
  };
};

const initialState: ContextProps = {
  state: {
    url: "",
    isInitialized: false,
    did: "",
    isUnlocked: false,
    client: null,
    loading: false,
    candidate: "",
    auth: "",
    connected: false,
    connectedLaoding: true,
    expertMode: getForVersion("expertMode") === "true",
    notifications: [],
  },
  methods: {
    configureEndpoint: () => null,
    resetEndpoint: () => null,
    handleTrustAgent: () => null,
    handleLogin: () => null,
    toggleExpertMode: () => null,
    handleNotification: () => null,
  },
};

export const Ad4minContext = createContext(initialState);

export function Ad4minProvider({ children }: any) {
  const [state, setState] = useState(initialState.state);

  useEffect(() => {
    setForVersion("expertMode", state.expertMode.toString());
  }, [state]);

  const toggleExpertMode = () => {
    setState((prevState) => ({
      ...prevState,
      expertMode: !prevState.expertMode,
    }));
  };

  const checkConnection = useCallback(
    async (url: string, client: Ad4mClient): Promise<string> => {
      return new Promise(async (resolve, reject) => {
        try {
          if (client) {
            const id = setTimeout(() => {
              resolve("");
            }, 1000);

            await client.agent.status(); // TODO runtime info is broken
            clearTimeout(id);

            console.log("get hc agent infos success.");

            resolve(url);
          }
        } catch (err) {
          if (url) {
            console.error(
              "Could not connect to the executor on a given url: ",
              url
            );
          }

          resolve("");
        }
      });
    },
    []
  );

  const handleLogin = useCallback(
    (client: Ad4mClient, login: Boolean, did: string) => {
      setState((prev) => ({
        ...prev,
        isUnlocked: login,
        did: did,
        loading: false,
      }));

      if (login) {
        client.runtime.addExceptionCallback((exception: ExceptionInfo) => {
          if (exception.type === ExceptionType.AgentIsUntrusted) {
            setState((prev) => ({
              ...prev,
              candidate: exception.addon!,
            }));
          }
          if (exception.type === ExceptionType.CapabilityRequested) {
            setState((prev) => ({
              ...prev,
              auth: exception.addon!,
            }));
          }

          if (exception.type === ExceptionType.InstallNotificationRequest) {
            setState((prev) => ({
              ...prev,
              notifications: [...prev.notifications, JSON.parse(exception.addon!)],
            }));
          }

          Notification.requestPermission().then((response) => {
            if (response === "granted") {
              new Notification(exception.title, { body: exception.message });
            }
          });
          console.log(exception);
          invoke("show_main_window");
          return null;
        });

        // @ts-ignore
        client.runtime.addNotificationTriggeredCallback((notification) => {
          console.log("Notification triggered: ", notification);
          const match = notification.triggerMatch;
          const parsed = JSON.parse(match);
          const firstMatch = parsed[0];
          const title = firstMatch?.Title
          sendNotification({
            icon: notification.notification.appIconPath,
            title: notification.notification.appName + (title ? ": " + title : ""),
            body: firstMatch?.Description || "Received a new notification",
            //body: match
          });
        })
      }
    },
    []
  );

  const checkIfAgentIsInitialized = useCallback(
    async (client: Ad4mClient) => {
      console.log("Check if agent is initialized.", client);

      let status = await client?.agent.status();
      console.log("agent status in init: ", status);

      handleLogin(client, status.isUnlocked, status.did ? status.did! : "");

      return status;
    },
    [handleLogin]
  );

  const connect = useCallback(
    async (url: string) => {
      const client = await buildAd4mClient(url);
      try {
        await checkConnection(url, client);

        const { isInitialized, isUnlocked } = await checkIfAgentIsInitialized(
          client
        );

        setState((prev) => ({
          ...prev,
          client,
          url,
          isInitialized,
          isUnlocked,
          connected: true,
          connectedLaoding: false,
        }));

        setForVersion("url", url as string);
      } catch (e) {
        console.log("err", e);
      }
    },
    [checkConnection, checkIfAgentIsInitialized]
  );

  useEffect(() => {
    let localStorageURL = getForVersion("url");

    if (
      localStorageURL &&
      localStorageURL !== "null" &&
      !localStorageURL.includes("localhost")
    ) {
      if (localStorageURL) {
        connect(localStorageURL);
      }
    } else {
      invoke("get_port").then((message) => {
        if (message) {
          const url = `ws://localhost:${message}/graphql`;
          connect(url);
        }
      });
    }
  }, [checkConnection, checkIfAgentIsInitialized, connect]);

  useEffect(() => {
    appWindow.listen("ready", async () => {
      const message = await invoke("get_port");
      if (message) {
        const url = `ws://localhost:${message}/graphql`;
        connect(url);
      }
    });
  }, [connect]);

  const handleTrustAgent = (candidate: string) => {
    setState((prev) => ({
      ...prev,
      candidate,
    }));
  };

  const handleNotification = (notification: NotificationType) => {
    setState((prev) => {
      const filteredNotifications = prev.notifications.filter(
        (n) => n.id !== notification.id
      );

      return {
        ...prev,
        notifications: filteredNotifications,
      }
    });
  }

  const configureEndpoint = async (url: string) => {
    if (url) {
      setState((prev) => ({
        ...prev,
        url,
      }));

      await connect(url);
    }
  };

  const resetEndpoint = () => {
    setState((prev) => ({
      ...prev,
      url: "",
      connected: false,
    }));

    removeForVersion("url");
  };

  useEffect(() => {
    const build = async () => {
      const client = await buildAd4mClient(state.url);

      setState((prev) => ({
        ...prev,
        client,
      }));
    };
    if (state.url) {
      console.log("gggg 0", state.url);
      build();
    }
  }, [state.url]);

  return (
    <Ad4minContext.Provider
      value={{
        state,
        methods: {
          configureEndpoint,
          handleTrustAgent,
          resetEndpoint,
          handleLogin,
          toggleExpertMode,
          handleNotification
        },
      }}
    >
      {children}
    </Ad4minContext.Provider>
  );
}
