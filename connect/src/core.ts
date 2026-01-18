import { ApolloClient, InMemoryCache, NormalizedCacheObject } from "@apollo/client/core";
import { createClient, Client as WSClient } from "graphql-ws";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { isEmbedded, setLocal, getLocal, removeLocal, connectWebSocket } from './utils';
import { Ad4mClient, CapabilityInput } from "@coasys/ad4m";
import autoBind from "auto-bind";

import { Ad4mConnectOptions, ConnectionStates, AuthStates, ConfigStates } from './types';


// export type AppInfo = {
//   name: string;
//   description: string;
//   domain: string;
//   // url?: string;
//   iconPath?: string;
// };

// export type Ad4mConnectOptions = {
//   appName: string;
//   appDesc: string;
//   appDomain: string;
//   appUrl?: string;
//   appIconPath?: string;
//   capabilities: CapabilityInput[];
//   dataPath?: string;
//   port?: number;
//   token?: string;
//   url?: string;
//   hosting?: boolean;
//   mobile?: boolean;
//   // Multi-user options
//   multiUser?: boolean;
//   remoteUrl?: string;
//   userEmail?: string;
//   userPassword?: string;
// };

// export type AuthStates = "authenticated" | "locked" | "unauthenticated";

// export type Event =
//   | "authstatechange"
//   | "connectionstatechange"
//   | "configstatechange";

// export type ConfigStates = "port" | "url" | "token";
// export type ConnectionStates =
//   | "connecting"
//   | "connected"
//   | "error"
//   | "port_not_found"
//   | "not_connected"
//   | "disconnected"
//   | "checking_local";

const DEFAULT_PORT = 12000;

export default class Ad4mConnect extends EventTarget {
  options: Ad4mConnectOptions;
  embedded: boolean;
  port: number;
  url: string;
  token: string;

  connectionState: ConnectionStates = "not-connected";
  authState: AuthStates = "unauthenticated";

  ad4mClient?: Ad4mClient;
  wsClient?: WSClient;
  apolloClient?: ApolloClient<NormalizedCacheObject>;
  activeSocket: WebSocket = null;

  requestId?: string;
  requestedRestart: boolean = false;

  // capabilities: CapabilityInput[] = [];

  // activeSocket: WebSocket = null;
  // requestedRestart: boolean = false;
  // authState: AuthStates = "unauthenticated";
  // connectionState: ConnectionStates = "not_connected";
  // wsClient?: WSClient;
  // apolloClient?: ApolloClient<NormalizedCacheObject>;
  // ad4mClient?: Ad4mClient;
  // requestId?: string;
  // url: string;
  // token: string;
  // port = DEFAULT_PORT;
  // capabilities: CapabilityInput[] = [];
  // appName: string;
  // appDesc: string;
  // appDomain: string;
  // appIconPath: string;
  // appUrl?: string;
  // isHosting: boolean = false;
  // options: Ad4mConnectOptions;
  
  // // Legacy listener system (kept for backwards compatibility)
  // private listeners: Record<Event, Function[]> = {
  //   ["authstatechange"]: [],
  //   ["configstatechange"]: [],
  //   ["connectionstatechange"]: [],
  // };
  
  // // Embedded mode support
  // private embeddedMode: boolean = false;
  // private embeddedResolve?: (client: Ad4mClient) => void;
  // private embeddedReject?: (error: Error) => void;

  constructor(options: Ad4mConnectOptions) {
    super();
    autoBind(this);
  
    this.options = options;
    this.embedded = isEmbedded();

    if (this.embedded) {
      this.port = options.port || DEFAULT_PORT;
      this.url = options.url || `ws://localhost:${this.port}/graphql`;
      this.token = '';
      // this.initializeEmbeddedMode();
    } else {
      this.port = options.port || parseInt(getLocal("ad4m-port")) || DEFAULT_PORT
      this.url = options.url || getLocal("ad4m-url") || `ws://localhost:${this.port}/graphql`;
      this.token = getLocal("ad4m-token") || '';
    }
    
    // if (this.embeddedMode) {
    //   // In embedded mode, ignore localStorage/options - wait for parent to send config
    //   // This prevents using stale tokens from previous sessions
    //   this.port = DEFAULT_PORT;
    //   this.url = '';
    //   this.token = '';
    //   this.initializeEmbeddedMode();
    // } else {
    //   // In standalone mode, use provided options OR fallback to localStorage
    //   this.port = options.port || parseInt(getForVersion("ad4mport")) || DEFAULT_PORT;
    //   this.url = options.url || getForVersion("ad4murl") || `ws://localhost:${this.port}/graphql`; // TODO: change
    //   this.token = options.token || getForVersion("ad4mtoken") || '';
    // }
    
    // this.isHosting = options.hosting || false;
    // // Don't auto-connect - let the UI decide when to connect
    // // this.buildClient();
  }

  // on(event: Event, cb: Function) {
  //   this.listeners[event].push(cb);
  // }

  private notifyConnectionChange(val: ConnectionStates) {
    if (this.connectionState === val) return;
    this.connectionState = val;
    this.dispatchEvent(new CustomEvent("connectionstatechange", { detail: val }));
  }

  private notifyAuthChange(val: AuthStates) {
    if (this.authState === val) return;
    this.authState = val;
    this.dispatchEvent(new CustomEvent("authstatechange", { detail: val }));

    
    // // In embedded mode, resolve the connect() promise when authenticated
    // if (this.embeddedMode && val === "authenticated" && this.embeddedResolve) {
    //   this.embeddedResolve(this.ad4mClient);
    //   this.embeddedResolve = undefined;
    //   this.embeddedReject = undefined;
    // }
  }

  private notifyConfigChange(type: ConfigStates, val: string) {
    this.dispatchEvent(new CustomEvent("configstatechange", { detail: { type, val } }));
  }

  async checkAuth(): Promise<boolean> {
    try {
      console.log('[Ad4m Connect] Checking authentication status...');
      const isLocked = await this.ad4mClient.agent.isLocked();

      if (isLocked) {
        console.log('[Ad4m Connect] Agent wallet is locked');
        this.notifyAuthChange("locked");
      } else {
        console.log('[Ad4m Connect] Agent wallet is unlocked, verifying status...');
        await this.ad4mClient.agent.status();
        this.notifyAuthChange("authenticated");
      }

      // Return true as we are authenticated
      return true;
    } catch (error) {
      console.error('[Ad4m Connect] Authentication check failed:', error);
      const lockedMessage = "Cannot extractByTags from a ciphered wallet. You must unlock first.";
      if (error.message === lockedMessage) {
        // TODO: isLocked throws an error, should just return a boolean. Temp fix
        this.notifyAuthChange("locked");
        return true;
      } else {
        this.notifyAuthChange("unauthenticated");
        return false;
      }
    }
  }

  async verifyCode(code: string): Promise<boolean> {
    try {
      const jwt = await this.ad4mClient.agent.generateJwt(this.requestId!, code);
      console.log('[Ad4m Connect] Code verified, received JWT:', jwt);
      this.token = jwt;
      setLocal("ad4m-token", this.token);
      // await this.buildClient();
      // await this.checkAuth();
      await this.connect();
      return true;
    } catch (error) {
      console.error('[Ad4m Connect] Code verification failed:', error);
      return false;
    }
  }

  private createApolloClient(wsClient: WSClient): ApolloClient<NormalizedCacheObject> {
    return new ApolloClient({
      link: new GraphQLWsLink(wsClient),
      cache: new InMemoryCache({ resultCaching: false, addTypename: false }),
      defaultOptions: {
        watchQuery: { fetchPolicy: "no-cache" as const },
        query: { fetchPolicy: "no-cache" as const },
        mutate: { fetchPolicy: "no-cache" as const },
      },
    });
  }

  private async buildClient(): Promise<Ad4mClient> {
    this.notifyConnectionChange("connecting");

    if (this.apolloClient && this.wsClient) {
      this.requestedRestart = true;
      this.wsClient.dispose();
      this.apolloClient.stop();
      this.wsClient = null;
      this.apolloClient = null;
    }

    this.wsClient = createClient({
      url: this.url,
      connectionParams: async () => ({ headers: { authorization: this.token } }),
      on: {
        opened: (socket: WebSocket) => {
          this.activeSocket = socket;
        },
        error: (e) => {
          this.notifyConnectionChange("error");
        },
        connected: () => {
          this.notifyConnectionChange("connected");
        },
        closed: async (event: CloseEvent) => {
          // If the connection was closed cleanly, which happens on every first connection, don't treat this as a disconnect
          if (event.wasClean || this.requestedRestart) return;

          if (!this.token) {
            this.notifyConnectionChange("error");
          } else {
            const client = await this.connect();
            if (client) {
              this.ad4mClient = client;
            } else {
              this.notifyConnectionChange("error");
            }
          }
        },
      },
    });

    this.apolloClient = this.createApolloClient(this.wsClient);
    this.ad4mClient = new Ad4mClient(this.apolloClient);
    this.requestedRestart = false;

    return this.ad4mClient;
  }

  async connect(): Promise<Ad4mClient> {
    //  if (this.embedded) {

    //  }

    console.log('*** connect called');

    return new Promise(async (resolve, reject) => {
      try {
        // Try to connect to local executor
        console.log('[Ad4m Connect] Attempting connection to:', this.url);
        
        await connectWebSocket(this.url);
        setLocal("ad4m-url", this.url);
        this.ad4mClient = await this.buildClient();
        console.log('*** new client built', this.ad4mClient);
        await this.checkAuth();
        console.log('*** auth checked', this.authState);

        resolve(this.ad4mClient);
      } catch (error) {
        console.error('[Ad4m Connect] Connection failed:', error);
        this.notifyConnectionChange("error");
        reject(error);
      }
    });
  }

  async requestCapability(invalidateToken = false): Promise<string> {
    console.log('[Ad4m Connect] Requesting capability...');
    if (invalidateToken) {
        this.token = null;
        this.notifyConfigChange("token", this.token);
    }

    this.requestId = await this.ad4mClient.agent.requestCapability({
      appName: this.options.appInfo.name,
      appDesc: this.options.appInfo.description,
      appUrl: this.options.appInfo.url,
      appDomain: this.options.appInfo.url,
      appIconPath: this.options.appInfo.iconPath,
      capabilities: this.options.capabilities,
    });

    return this.requestId;
  }

}
