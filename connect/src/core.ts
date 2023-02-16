import {
  ApolloClient,
  InMemoryCache,
  NormalizedCacheObject,
} from "@apollo/client/core";
import { createClient, Client as WSClient } from "graphql-ws";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { Ad4mClient } from "@perspect3vism/ad4m";
import { checkPort, connectWebSocket } from "./utils";

export type Ad4mConnectOptions = {
  appName: string;
  appDesc: string;
  appDomain: string;
  appIconPath?: string;
  capabilities: { [x: string]: any }[];
  dataPath?: string;
  port?: number;
  token?: string;
  url?: string;
};

export type AuthStates = "authenticated" | "locked" | "unauthenticated";

export type Event =
  | "authstatechange"
  | "connectionstatechange"
  | "configstatechange";

export type ConfigStates = "port" | "url" | "token";
export type ConnectionStates =
  | "connecting"
  | "connected"
  | "error"
  | "port_not_found"
  | "not_connected"
  | "disconnected";

export default class Ad4mConnect {
  authState: AuthStates = "unauthenticated";
  connectionState: ConnectionStates = "not_connected";
  wsClient?: WSClient;
  apolloClient?: ApolloClient<NormalizedCacheObject>;
  ad4mClient?: Ad4mClient;
  requestId?: string;
  url: string;
  token: string;
  isFullyInitialized = false;
  port = 12000;
  capabilities: { [x: string]: any }[] = [];
  appName: string;
  appDesc: string;
  appDomain: string;
  listeners: Record<Event, Function[]> = {
    ["authstatechange"]: [],
    ["configstatechange"]: [],
    ["connectionstatechange"]: [],
  };

  // @fayeed - params
  constructor({
    appName,
    appDesc,
    appDomain,
    capabilities,
    port,
    token,
    url,
  }: Ad4mConnectOptions) {
    //! @fayeed - make it support node.js
    this.appName = appName;
    this.appDesc = appDesc;
    this.appDomain = appDomain;
    this.capabilities = capabilities;
    this.port = port || this.port;
    this.url = url || `ws://localhost:${this.port}/graphql`;
    this.token = token || this.token;

    setTimeout(() => {
      this.checkConnection();
    }, 0);
  }

  private notifyConfigChange(val: ConfigStates, data: string | number) {
    this.listeners["configstatechange"].forEach((listener) => {
      listener(val, data);
    });
  }

  private notifyConnectionChange(val: ConnectionStates) {
    if (this.connectionState === val) return;
    this.connectionState = val;
    this.listeners["connectionstatechange"].forEach((listener) => {
      listener(val);
    });
  }

  private notifyAuthChange(val: AuthStates) {
    if (this.authState === val) return;
    this.authState = val;
    this.listeners["authstatechange"].forEach((listener) => {
      listener(val);
    });
  }

  setPort(port: number) {
    if (this.port === port) return;
    this.port = port;
    this.setUrl(`ws://localhost:${this.port}/graphql`);
    this.notifyConfigChange("port", port);
  }

  setUrl(url: string) {
    if (this.url === url) return;
    this.url = url;
    this.notifyConfigChange("url", url);
  }

  setToken(token: string) {
    if (this.token === token) return;
    this.token = token;
    this.notifyConfigChange("token", token);
  }

  on(event: Event, cb: Function) {
    this.listeners[event].push(cb);
  }

  async connect(url?: string) {
    if (url) {
      this.setUrl(url);
    }
    this.checkConnection();
  }

  async checkConnection() {
    try {
      this.notifyConnectionChange("connecting");
      const res = await connectWebSocket(this.url, 10000);
      this.buildClient();
    } catch (e) {
      this.connectToPort();
    }
  }

  async connectToPort() {
    try {
      this.notifyConnectionChange("connecting");
      const port = await this.findPort();
      this.setPort(port);
      this.buildClient();
    } catch (error) {
      this.notifyConnectionChange("not_connected");
    }
  }

  async findPort() {
    const ports = [...Array(10).keys()].map((i) => {
      return checkPort(12000 + i);
    });

    const results = await Promise.all(ports);
    const result = results.find((port) => port);

    if (result) return result;
    else {
      throw Error("Couldn't find an open port");
    }
  }

  async buildClient() {
    if (this.apolloClient) {
      this.wsClient.terminate();
      this.apolloClient.stop();
    }

    this.wsClient = createClient({
      url: this.url,
      connectionParams: async () => ({
        headers: {
          authorization: this.token,
        },
      }),
      on: {
        error: (e) => {
          this.notifyConnectionChange("not_connected");
        },
        connected: () => {
          this.notifyConnectionChange("connected");
        },
        closed: () => {
          if (this.isFullyInitialized) {
            this.notifyConnectionChange("disconnected");
          }
        },
      },
    });

    this.apolloClient = new ApolloClient({
      link: new GraphQLWsLink(this.wsClient),
      cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
      defaultOptions: {
        watchQuery: {
          fetchPolicy: "no-cache",
        },
        query: {
          fetchPolicy: "no-cache",
        },
      },
    });

    // @ts-ignore
    this.ad4mClient = new Ad4mClient(this.apolloClient);

    this.checkAuth();

    this.ad4mClient.agent.addAgentStatusChangedListener(() => {
      this.checkAuth();
    });
  }

  async checkAuth() {
    try {
      const isLocked = await this.ad4mClient.agent.isLocked();
      if (isLocked) {
        this.notifyAuthChange("locked");
      } else {
        const status = await this.ad4mClient.agent.status();
        this.notifyAuthChange("authenticated");
        this.isFullyInitialized = true;
      }
    } catch (error) {
      this.notifyAuthChange("unauthenticated");
    }
  }

  async requestCapability(invalidateToken = false) {
    if (invalidateToken) {
      this.setToken(null);
    }

    this.requestId = await this.ad4mClient?.agent.requestCapability(
      this.appName,
      this.appDesc,
      this.appDomain,
      JSON.stringify(this.capabilities)
    );
  }

  async verifyCode(code: string) {
    const jwt = await this.ad4mClient?.agent.generateJwt(this.requestId!, code);

    this.setToken(jwt);

    this.isFullyInitialized = true;

    this.notifyAuthChange("authenticated");
  }
}
