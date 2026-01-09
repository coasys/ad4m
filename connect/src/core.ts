import {
  ApolloClient,
  InMemoryCache,
  NormalizedCacheObject,
} from "@apollo/client/core";
import { createClient, Client as WSClient } from "graphql-ws";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { Ad4mClient, CapabilityInput } from "@coasys/ad4m";
import { checkPort, connectWebSocket, DEFAULT_PORT, getForVersion, setForVersion, isEmbedded } from "./utils";
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
  // Multi-user options
  multiUser?: boolean;
  backendUrl?: string;
  userEmail?: string;
  userPassword?: string;
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
  | "disconnected"
  | "checking_local";

export default class Ad4mConnect extends EventTarget {
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
  port = DEFAULT_PORT;
  capabilities: CapabilityInput[] = [];
  appName: string;
  appDesc: string;
  appDomain: string;
  appIconPath: string;
  appUrl?: string;
  isHosting: boolean = false;
  options: Ad4mConnectOptions;
  
  // Legacy listener system (kept for backwards compatibility)
  private listeners: Record<Event, Function[]> = {
    ["authstatechange"]: [],
    ["configstatechange"]: [],
    ["connectionstatechange"]: [],
  };
  
  // Embedded mode support
  private embeddedMode: boolean = false;
  private embeddedResolve?: (client: Ad4mClient) => void;
  private embeddedReject?: (error: Error) => void;

  // @fayeed - params
  constructor(options: Ad4mConnectOptions) {
    super(); // Call EventTarget constructor
    autoBind(this);
    //! @fayeed - make it support node.js
    this.options = options;
    this.appName = options.appName;
    this.appDesc = options.appDesc;
    this.appDomain = options.appDomain;
    this.appUrl = options.appUrl;
    this.appIconPath = options.appIconPath;
    this.capabilities = options.capabilities;
    
    // Check if running in embedded mode first
    this.embeddedMode = isEmbedded();
    
    if (this.embeddedMode) {
      // In embedded mode, ignore localStorage/options - wait for parent to send config
      // This prevents using stale tokens from previous sessions
      this.port = DEFAULT_PORT;
      this.url = '';
      this.token = '';
      this.initializeEmbeddedMode();
    } else {
      // In standalone mode, use provided options OR fallback to localStorage
      this.port = options.port || parseInt(getForVersion("ad4mport")) || DEFAULT_PORT;
      this.url = options.url || getForVersion("ad4murl") || `ws://localhost:${this.port}/graphql`;
      this.token = options.token || getForVersion("ad4mtoken") || '';
    }
    
    this.isHosting = options.hosting || false;
    // Don't auto-connect - let the UI decide when to connect
    // this.buildClient();
  }
  
  /**
   * Initialize embedded mode - set up postMessage protocol
   */
  private initializeEmbeddedMode(): void {
    console.log('[Ad4m Connect] Running in embedded mode - waiting for AD4M config from parent');
    
    // Set up listener for AD4M_CONFIG from parent window
    window.addEventListener('message', async (event: MessageEvent) => {
      if (event.data?.type === 'AD4M_CONFIG') {
        console.log('[Ad4m Connect] Received AD4M_CONFIG from parent:', { 
          port: event.data.port,
          hasToken: !!event.data.token 
        });
        
        try {
          const { port, token } = event.data;
          
          // Set connection details from parent
          this.port = port;
          this.token = token;
          this.url = `ws://localhost:${port}/graphql`;
          
          // Store in localStorage for persistence
          setForVersion('ad4mport', port.toString());
          setForVersion('ad4mtoken', token);
          setForVersion('ad4murl', this.url);
          
          // Build the client with received credentials
          // The client will connect asynchronously and checkAuth() will be called
          // by the 'connected' callback in buildClient()
          // The embeddedResolve will be called when authState changes to 'authenticated'
          this.ad4mClient = this.buildClient();
        } catch (error) {
          console.error('[Ad4m Connect] Failed to initialize from AD4M_CONFIG:', error);
          if (this.embeddedReject) {
            this.embeddedReject(error as Error);
            this.embeddedResolve = undefined;
            this.embeddedReject = undefined;
          }
        }
      }
    });
    
    // Request AD4M config from parent window
    console.log('[Ad4m Connect] Requesting AD4M config from parent window');
    window.parent.postMessage({ type: 'REQUEST_AD4M_CONFIG' }, '*');
  }

  private notifyConfigChange(val: ConfigStates, data: string | number) {
    // Dispatch as DOM CustomEvent (EventTarget API)
    this.dispatchEvent(new CustomEvent("configstatechange", { 
      detail: { type: val, data } 
    }));
    
    // Also call legacy listeners for backwards compatibility
    this.listeners["configstatechange"].forEach((listener) => {
      listener(val, data);
    });
  }

  private notifyConnectionChange(val: ConnectionStates) {
    if (this.connectionState === val) return;
    this.connectionState = val;
    
    // Dispatch as DOM CustomEvent (EventTarget API)
    this.dispatchEvent(new CustomEvent("connectionstatechange", { detail: val }));
    
    // Also call legacy listeners for backwards compatibility
    this.listeners["connectionstatechange"].forEach((listener) => {
      listener(val);
    });
  }

  private notifyAuthChange(val: AuthStates) {
    if (this.authState === val) return;
    this.authState = val;
    
    // Dispatch as DOM CustomEvent (EventTarget API)
    this.dispatchEvent(new CustomEvent("authstatechange", { detail: val }));
    
    // Also call legacy listeners for backwards compatibility
    this.listeners["authstatechange"].forEach((listener) => {
      listener(val);
    });
    
    // In embedded mode, resolve the connect() promise when authenticated
    if (this.embeddedMode && val === "authenticated" && this.embeddedResolve) {
      this.embeddedResolve(this.ad4mClient);
      this.embeddedResolve = undefined;
      this.embeddedReject = undefined;
    }
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
    // Store URL in localStorage so auto-reconnect uses the correct backend
    setForVersion('ad4murl', url);
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

  // Build a temporary client for detection/queries without changing state
  buildTempClient(url: string): Ad4mClient {
    const wsClient = createClient({
      url: url,
      connectionParams: async () => ({
        headers: {
          authorization: "",
        },
      }),
    });

    const apolloClient = new ApolloClient({
      link: new GraphQLWsLink(wsClient),
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

    return new Ad4mClient(apolloClient);
  }

  // If url is explicit , don't search for open ports
  async connect(url?: string): Promise<Ad4mClient> {
    // In embedded mode, wait for postMessage from parent instead of connecting
    if (this.embeddedMode) {
      console.log('[Ad4m Connect] Embedded mode - waiting for AD4M config via postMessage');
      
      return new Promise((resolve, reject) => {
        // Set up timeout
        const timeout = setTimeout(() => {
          reject(new Error('Timeout waiting for AD4M config from parent window'));
        }, 30000); // 30 second timeout
        
        // Store resolvers to call when AD4M_CONFIG arrives
        this.embeddedResolve = (client: Ad4mClient) => {
          clearTimeout(timeout);
          console.log('[Ad4m Connect] Successfully connected in embedded mode');
          resolve(client);
        };
        
        this.embeddedReject = (error: Error) => {
          clearTimeout(timeout);
          reject(error);
        };
        
        // If we already have a client (message arrived before connect() was called)
        if (this.ad4mClient && this.authState === 'authenticated') {
          clearTimeout(timeout);
          console.log('[Ad4m Connect] Client already initialized in embedded mode');
          resolve(this.ad4mClient);
        }
      });
    }
    
    // Standalone mode - return a Promise that resolves when authenticated
    return new Promise(async (resolve, reject) => {
      try {
        // Try to connect to local executor
        const localUrl = url || `ws://localhost:${this.port}/graphql`;
        console.log('[Ad4m Connect] Attempting connection to:', localUrl);
        
        await connectWebSocket(localUrl);
        this.setUrl(localUrl);
        this.ad4mClient = this.buildClient();
        await this.checkAuth();
        
        // If already authenticated (has stored token), resolve immediately
        if (this.authState === 'authenticated' && this.ad4mClient) {
          console.log('[Ad4m Connect] Already authenticated, resolving immediately');
          resolve(this.ad4mClient);
        } else {
          // Not authenticated - UI modal will show
          // Set up listener to resolve when authentication completes
          console.log('[Ad4m Connect] Waiting for authentication...');
          const authHandler = (state: AuthStates) => {
            if (state === 'authenticated' && this.ad4mClient) {
              console.log('[Ad4m Connect] Authentication completed, resolving client');
              this.listeners['authstatechange'] = this.listeners['authstatechange'].filter(l => l !== authHandler);
              resolve(this.ad4mClient);
            }
          };
          this.on('authstatechange', authHandler);
        }
      } catch (error) {
        console.error('[Ad4m Connect] Connection failed:', error);
        this.notifyConnectionChange("not_connected");
        this.notifyAuthChange("unauthenticated");
        reject(error);
      }
    });
  }

  /**
   * @deprecated This method is deprecated in favor of the UI-driven email verification flow.
   * The new flow uses requestLoginVerification() -> verifyEmailCode() which is handled
   * automatically by the ad4m-connect web component. This method is kept for backwards
   * compatibility but will only work if email verification is not enabled on the server.
   */
  async connectMultiUser(): Promise<Ad4mClient> {
    console.warn("[Ad4m Connect] connectMultiUser() is deprecated. The new email verification flow is handled automatically by the UI.");

    try {
      // Connect to the multi-user backend
      const backendUrl = this.options.backendUrl!;
      console.debug("[Ad4m Connect] Connecting to backend:", backendUrl);
      await connectWebSocket(backendUrl);

      this.setUrl(backendUrl);

      // Build client for user management operations (without token initially)
      const tempClient = this.buildClient();
      console.debug("[Ad4m Connect] Built client:", tempClient);

      // For backwards compatibility with servers that don't have email verification enabled,
      // try the old password-based login/signup flow
      let token: string;
      try {
        console.debug("[Ad4m Connect] Logging in user:", this.options.userEmail!);
        token = await tempClient.agent.loginUser(this.options.userEmail!, this.options.userPassword!);
        console.debug("[Ad4m Connect] Login successful");
      } catch (loginError) {
        console.debug("[Ad4m Connect] Login error:", loginError);
        // User doesn't exist, try to create
        console.debug("[Ad4m Connect] User does not exist, trying to create user:", this.options.userEmail!);
        try {
          console.debug("[Ad4m Connect] Creating user:", this.options.userEmail!);
          const createResult = await tempClient.agent.createUser(this.options.userEmail!, this.options.userPassword!);
          console.debug("[Ad4m Connect] Create result:", createResult);
          if (!createResult.success) {
            throw new Error(createResult.error || "Failed to create user. Server may require email verification.");
          }

          // Now login
          console.debug("[Ad4m Connect] Logging in user after creation:", this.options.userEmail!);
          token = await tempClient.agent.loginUser(this.options.userEmail!, this.options.userPassword!);
          console.log("[Ad4m Connect] Successfully created and logged in user");
        } catch (createError) {
          throw new Error(`Failed to create/login user: ${createError.message}. If email verification is enabled, please use the UI-driven flow instead.`);
        }
      }

      // Set the token and build authenticated client
      this.setToken(token);
      const authenticatedClient = this.buildClient();

      // Verify authentication
      await authenticatedClient.agent.status();

      // Store the authenticated client so it can be retrieved via getAd4mClient()
      this.ad4mClient = authenticatedClient;

      this.notifyAuthChange("authenticated");
      this.notifyConnectionChange("connected");

      return authenticatedClient;
    } catch (error) {
      this.notifyConnectionChange("error");
      this.notifyAuthChange("unauthenticated");
      throw error;
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

      await connectWebSocket(this.url);
      return this.buildClient();
    } catch (e) {
      this.notifyConnectionChange("not_connected");
      return this.connectToPort();
    }
  }

  async findPort(): Promise<number> {
    const ports = [...Array(10).keys()].map((_, i) => {
      return checkPort(DEFAULT_PORT + i);
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
      const websocket = new WebSocket(this.url, "graphql-transport-ws");
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
        closed: async (e: CloseEvent) => {
          // If the connection was closed cleanly, which happens on every
          // first connection, don't treat this as a disconnect
          if(e.wasClean) {
            return
          }

          // Iff the user explicitly requested a restart, also don't treat 
          // this as a disconnect (handling the disconnect makes sense when an
          // established connection gets lost after the first handshake)
          if (this.requestedRestart) {
            return
          }

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

  async isAuthenticated(): Promise<boolean> {
    return this.authState === "authenticated";
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
    this.setPort(DEFAULT_PORT);
    this.notifyConnectionChange("not_connected");
    this.notifyAuthChange("unauthenticated");
  }
}
