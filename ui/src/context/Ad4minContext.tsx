import { Ad4mClient, ExceptionInfo, ExceptionType, Notification as NotificationType } from "@coasys/ad4m";
import { invoke } from "@tauri-apps/api/core";
import { getCurrentWebviewWindow } from "@tauri-apps/api/webviewWindow";
import { sendNotification } from "@tauri-apps/plugin-notification";
import { createContext, useCallback, useEffect, useState } from "react";
import {
  buildAd4mClient,
  getForVersion,
  removeForVersion,
  setForVersion,
} from "../util";
const appWindow = getCurrentWebviewWindow();

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
  connectedLoading: boolean;
  expertMode: boolean;
  multiUserEnabled: boolean;
  freeHostingEnabled: boolean;
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
    setMultiUserEnabled: (enabled: boolean) => void;
    setFreeHostingEnabled: (enabled: boolean) => void;
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
    connectedLoading: true,
    expertMode: getForVersion("expertMode") === "true",
    multiUserEnabled: false,
    freeHostingEnabled: true,
    notifications: [],
  },
  methods: {
    configureEndpoint: () => null,
    resetEndpoint: () => null,
    handleTrustAgent: () => null,
    handleLogin: () => null,
    toggleExpertMode: () => null,
    setMultiUserEnabled: () => null,
    setFreeHostingEnabled: () => null,
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

  const setMultiUserEnabled = async (enabled: boolean) => {
    if (state.client) {
      try {
        await state.client.runtime.setMultiUserEnabled(enabled);
        setState((prevState) => ({
          ...prevState,
          multiUserEnabled: enabled,
        }));
      } catch (error) {
        console.error("Failed to set multi-user mode:", error);
        throw error;
      }
    }
  };

  const setFreeHostingEnabled = async (enabled: boolean) => {
    if (state.client) {
      try {
        await state.client.runtime.setFreeHostingEnabled(enabled);
        setState((prevState) => ({
          ...prevState,
          freeHostingEnabled: enabled,
        }));
      } catch (error) {
        console.error("Failed to set free hosting mode:", error);
        throw error;
      }
    }
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
              notifications: [
                ...prev.notifications,
                JSON.parse(exception.addon!),
              ],
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
          const title = firstMatch?.Title;
          sendNotification({
            icon: notification.notification.appIconPath,
            title:
              notification.notification.appName + (title ? ": " + title : ""),
            body: firstMatch?.Description || "Received a new notification",
            //body: match
          });
        });
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

        const { isInitialized, isUnlocked } =
          await checkIfAgentIsInitialized(client);

        setState((prev) => ({
          ...prev,
          client,
          url,
          isInitialized,
          isUnlocked,
          connected: true,
          connectedLoading: false,
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
      invoke<{ port: number; tls_enabled: boolean }>("get_port").then((portInfo) => {
        if (portInfo) {
          const url = `http://localhost:${portInfo.port}`;
          connect(url);
        }
      });
    }
  }, [checkConnection, checkIfAgentIsInitialized, connect]);

  useEffect(() => {
    appWindow.listen("ready", async () => {
      const portInfo = await invoke<{ port: number; tls_enabled: boolean }>("get_port");
      if (portInfo) {
        const url = `http://localhost:${portInfo.port}`;
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
      };
    });
  };

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

  // Load multi-user and free hosting state when client is available
  useEffect(() => {
    const loadHostingState = async () => {
      if (state.client) {
        const [multiUserResult, freeHostingResult] = await Promise.allSettled([
          state.client.runtime.multiUserEnabled(),
          state.client.runtime.freeHostingEnabled(),
        ]);
        setState((prev) => ({
          ...prev,
          ...(multiUserResult.status === "fulfilled"
            ? { multiUserEnabled: multiUserResult.value }
            : {}),
          ...(freeHostingResult.status === "fulfilled"
            ? { freeHostingEnabled: freeHostingResult.value }
            : {}),
        }));
        if (multiUserResult.status === "rejected") {
          console.error("Failed to load multi-user state:", multiUserResult.reason);
        }
        if (freeHostingResult.status === "rejected") {
          console.error("Failed to load free hosting state:", freeHostingResult.reason);
        }
      }
    };
    loadHostingState();
  }, [state.client]);

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
          setMultiUserEnabled,
          setFreeHostingEnabled,
          handleNotification,
        },
      }}
    >
      {children}
    </Ad4minContext.Provider>
  );
}
