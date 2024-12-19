import {
  ApolloClient,
  InMemoryCache,
  NormalizedCacheObject,
} from "@apollo/client/core";
import { createClient, Client as WSClient } from "graphql-ws";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { Ad4mClient, CapabilityInput } from "@coasys/ad4m";
import { checkPort, connectWebSocket, removeForVersion, setForVersion } from "./utils";
import autoBind from "auto-bind";

export type Ad4mConnectOptions = {
  appName: string;
  appDesc: string;
  appDomain: string;
  appUrl?: string;
  appIconPath?: string;
  capabilities: CapabilityInput[];
  dataPath?: string;
  port?: number;
  token?: string;
  url?: string;
  hosting?: boolean;
  mobile?: boolean;
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
  activeSocket: WebSocket = null;
  requestedRestart: boolean = false;
  authState: AuthStates = "unauthenticated";
  connectionState: ConnectionStates = "not_connected";
  wsClient?: WSClient;
  apolloClient?: ApolloClient<NormalizedCacheObject>;
  ad4mClient?: Ad4mClient;
  requestId?: string;
  url: string;
  token: string;
  port = 12000;
  capabilities: CapabilityInput[] = [];
  appName: string;
  appDesc: string;
  appDomain: string;
  appIconPath: string;
  appUrl?: string;
  isHosting: boolean = false;
  listeners: Record<Event, Function[]> = {
    ["authstatechange"]: [],
    ["configstatechange"]: [],
    ["connectionstatechange"]: [],
  };

  // @fayeed - params
  constructor({
    appName,
    appDesc,
    appIconPath,
    appUrl,
    appDomain,
    capabilities,
    port,
    token,
    url,
    hosting
  }: Ad4mConnectOptions) {
    autoBind(this);
    //! @fayeed - make it support node.js
    this.appName = appName;
    this.appDesc = appDesc;
    this.appDomain = appDomain;
    this.appUrl = appUrl;
    this.appIconPath = appIconPath;
    this.capabilities = capabilities;
    this.port = port || this.port;
    this.url = url || `ws://localhost:${this.port}/graphql`;
    this.token = token || this.token;
    this.isHosting = hosting || false;
    this.buildClient();
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

  // If url is explicit , don't search for open ports
  async connect(url?: string): Promise<Ad4mClient> {
    try {
      if (url) {
        await connectWebSocket(url, 10000);
        this.setUrl(url);
        const client = this.buildClient();
        await this.checkAuth();
        return client;
      } else {
        const client = await this.ensureConnection();
        await this.checkAuth();
        return client;
      }
    } catch {
      this.notifyConnectionChange("not_connected");
      this.notifyAuthChange("unauthenticated");
    }
  }


  async loginToHosting(email: string, password: string) {
    try {
      const response = await fetch('https://hosting.ad4m.dev/api/auth/login', {
          method: 'POST',
          headers: {
              'Content-Type': 'application/json'
          },
          body: JSON.stringify({
              email,
              password
          })
      });

      if (response.status === 200) {
        const data = await response.json();
        // @ts-ignore
        localStorage.setItem('hosting_token', data.token);

        let token = localStorage.getItem('hosting_token');

        const response2 = await fetch('https://hosting.ad4m.dev/api/service/info', {
          method: 'GET',
          headers: {
              'Content-Type': 'application/json',
              'Authorization': 'Bearer ' + token
          },
        });

        if (response2.status === 200) {
          const data = await response2.json();

          if (data.serviceId) {
            this.setPort(data.port);
            this.setUrl(data.url);

            this.isHosting = true;

            setForVersion('ad4mhosting', 'true');

            if (!data.paused) {
              this.connect();
            } else {
              throw new Error('Hosting is not running');
            }
          }
        }
      }  else {
        const data = await response.json();

        if (data.message === 'Passwords did not match') {
          throw new Error('Passwords did not match');
        }
      }
    } catch (e) {
      console.log(e)
      throw new Error(`Error logging in ${e}`);
    }
  }

  async checkEmail(email: string) {
    try {
      const response = await fetch(`https://hosting.ad4m.dev/api/auth/check-email?email=${email}`, {
          method: 'GET',
          headers: {
                'Content-Type': 'application/json'
            }
        });

        return response.status === 200;
      } catch (e) {
        console.log(e)
      }
  }

  // If port is explicit, don't search for port
  async connectToPort(port?: number): Promise<Ad4mClient> {
    try {
      if (port) {
        const found = await checkPort(port);
        this.setPort(found);

        return this.buildClient();
      } else {
        if (this.url.includes("localhost")) {
          const port = await this.findPort();
          this.setPort(port);

          return this.buildClient();
        }
      }
    } catch (error) {
      this.notifyConnectionChange("not_connected");
      this.notifyAuthChange("unauthenticated");
    }
  }

  async ensureConnection(): Promise<Ad4mClient> {
    const socketIsActive =
      this.activeSocket?.readyState === WebSocket.OPEN &&
      this.activeSocket?.url === this.url;

    if (socketIsActive && this.ad4mClient) {
      return this.ad4mClient;
    }

    try {
      this.notifyConnectionChange("connecting");

      await connectWebSocket(this.url, 10000);
      return this.buildClient();
    } catch (e) {
      this.notifyConnectionChange("not_connected");
      return this.connectToPort();
    }
  }

  async findPort(): Promise<number> {
    const ports = [...Array(10).keys()].map((_, i) => {
      return checkPort(12000 + i);
    });

    const results = await Promise.allSettled(ports);
    const result = results.find((port) => port.status === "fulfilled");

    // @ts-ignore
    if (result) return result.value;
    else {
      throw Error("Couldn't find an open port");
    }
  }

  buildClient(): Ad4mClient {
    this.notifyConnectionChange("connecting");

    // Make sure the url is valid
    try {
      const websocket = new WebSocket(this.url);
    } catch (e) {
      this.notifyConnectionChange("not_connected");
      return;
    }

    if (this.apolloClient && this.wsClient) {
      this.requestedRestart = true;
      this.wsClient.dispose();
      this.apolloClient.stop();
      this.wsClient = null;
      this.apolloClient = null;
    }

    this.wsClient = createClient({
      url: this.url,
      connectionParams: async () => ({
        headers: {
          authorization: this.token,
        },
      }),
      on: {
        opened: (socket: WebSocket) => {
          this.activeSocket = socket;
        },
        error: (e) => {
          this.notifyConnectionChange("not_connected");
          this.notifyAuthChange("unauthenticated");
        },
        connected: () => {
          this.notifyConnectionChange("connected");
          this.checkAuth();
        },
        closed: async () => {
          if (!this.requestedRestart) {
            if (!this.token) {
              this.notifyConnectionChange(!this.token ? "not_connected" : "disconnected");
              this.notifyAuthChange("unauthenticated");
              this.requestedRestart = false;
            } else {
              const client = await this.connect();
              if (client) {
                this.ad4mClient = client;
              } else {
                this.notifyConnectionChange(!this.token ? "not_connected" : "disconnected");
                this.notifyAuthChange("unauthenticated");
                this.requestedRestart = false;
              }
            }
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
        mutate: {
          fetchPolicy: "no-cache",
        }
      },
    });

    this.ad4mClient = new Ad4mClient(this.apolloClient);

    return this.ad4mClient;
  }

  async checkAuth(): Promise<boolean> {
    try {
      const isLocked = await this.ad4mClient.agent.isLocked();

      if (isLocked) {
        this.notifyAuthChange("locked");
      } else {
        await this.ad4mClient.agent.status();
        this.notifyAuthChange("authenticated");
      }
      // Return true as we are authenticated
      return true;
    } catch (error) {
      if (
        error.message ===
        "Socket closed with event 4500 Cannot extractByTags from a ciphered wallet. You must unlock first."
      ) {
        // TODO: isLocked throws an error, should just return a boolean. Temp fix
        this.notifyAuthChange("locked");
        return true;
      } else {
        this.notifyAuthChange("unauthenticated");
        return false;
      }
    }
  }

  async requestCapability(invalidateToken = false): Promise<string> {
    if (invalidateToken) {
      this.setToken(null);
    }

    if (this.isHosting) {
      let token = localStorage.getItem('hosting_token');

      const response = await fetch('https://hosting.ad4m.dev/api/service/checkStatus', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Authorization': 'Bearer ' + token
        },
      });

      if (response.status !== 200) {
        console.error('Looks like the client is not running you might not recieve the mail with the code, please check your dashboard logs.');
      }
    }

    this.requestId = await this.ad4mClient?.agent.requestCapability({
      appName: this.appName,
      appDesc: this.appDesc,
      appUrl: this.appUrl,
      appIconPath: this.appIconPath,
      appDomain: this.appDomain,
      capabilities: this.capabilities,
    });

    return this.requestId;
  }

  async verifyCode(code: string): Promise<string> {
    try {
      const jwt = await this.ad4mClient?.agent.generateJwt(this.requestId!, code);
      this.setToken(jwt);
      await this.buildClient();
      await this.checkAuth();
      return this.token;
    } catch (error) {
      throw new Error("Invalid code");
    }
  }

  clearState() {
    this.setToken(null);
    this.setPort(12000);
    this.notifyConnectionChange("not_connected");
    this.notifyAuthChange("unauthenticated");
  }
}
