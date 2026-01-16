import { ApolloClient, InMemoryCache, NormalizedCacheObject } from "@apollo/client/core";
import { createClient, Client as WSClient } from "graphql-ws";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { isEmbedded, setLocal, getLocal, removeLocal, connectWebSocket } from './utils';
import { Ad4mClient, CapabilityInput } from "@coasys/ad4m";
// import { checkPort, connectWebSocket, DEFAULT_PORT, getForVersion, setForVersion, isEmbedded } from "./old/utils";
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
      this.token = jwt;
      setLocal("ad4m-token", this.token);
      await this.buildClient();
      await this.checkAuth();
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
          // TODO: remove this?
          // // If the connection was closed cleanly, which happens on every first connection, don't treat this as a disconnect
          // if (event.wasClean || this.requestedRestart) return;

          if (!this.token) {
            this.notifyConnectionChange("error");
            // this.requestedRestart = false;
          } else {
            const client = await this.connect();
            if (client) {
              this.ad4mClient = client;
            } else {
              this.notifyConnectionChange("error");
              // this.requestedRestart = false;
            }
          }
        },
      },
    });

    this.apolloClient = this.createApolloClient(this.wsClient);
    this.ad4mClient = new Ad4mClient(this.apolloClient);

    return this.ad4mClient;
  }

  async connect(url?: string): Promise<Ad4mClient> {
    //  if (this.embedded) {

    //  }

    console.log('*** connect called with url', url);

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
        
        // // If already authenticated (has stored token), resolve immediately
        // if (this.authState === 'authenticated' && this.ad4mClient) {
        //   console.log('[Ad4m Connect] Already authenticated, resolving immediately');
        //   resolve(this.ad4mClient);
        // } else {
        //   // Not authenticated - UI modal will show
        //   // Set up listener to resolve when authentication completes
        //   console.log('[Ad4m Connect] Waiting for authentication...');
        //   const authHandler = (state: AuthStates) => {
        //     if (state === 'authenticated' && this.ad4mClient) {
        //       console.log('[Ad4m Connect] Authentication completed, resolving client');
        //       // this.listeners['authstatechange'] = this.listeners['authstatechange'].filter(l => l !== authHandler);
        //       resolve(this.ad4mClient);
        //     }
        //   };
        //   this.on('authstatechange', authHandler);
        // }
      } catch (error) {
        console.error('[Ad4m Connect] Connection failed:', error);
        this.notifyConnectionChange("error");
        // this.notifyDisconnected();
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

    // if (this.isHosting) {
    //   let token = localStorage.getItem('hosting_token');

    //   const response = await fetch('https://hosting.ad4m.dev/api/service/checkStatus', {
    //     method: 'POST',
    //     headers: {
    //         'Content-Type': 'application/json',
    //         'Authorization': 'Bearer ' + token
    //     },
    //   });

    //   if (response.status !== 200) {
    //     console.error('Looks like the client is not running you might not recieve the mail with the code, please check your dashboard logs.');
    //   }
    // }

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
  
  // // /**
  // //  * Initialize embedded mode - set up postMessage protocol
  // //  */
  // private initializeEmbeddedMode(): void {
  //   console.log('[Ad4m Connect] Running in embedded mode - waiting for AD4M config from parent');
    
  //   // Set up listener for AD4M_CONFIG from parent window
  //   window.addEventListener('message', async (event: MessageEvent) => {
  //     if (event.data?.type === 'AD4M_CONFIG') {
  //       console.log('[Ad4m Connect] Received AD4M_CONFIG from parent:', { 
  //         port: event.data.port,
  //         hasToken: !!event.data.token 
  //       });
        
  //       try {
  //         const { port, token } = event.data;
          
  //         // Set connection details from parent
  //         this.port = port;
  //         this.token = token;
  //         this.url = `ws://localhost:${port}/graphql`;
          
  //         // Store in localStorage for persistence
  //         setForVersion('ad4mport', port.toString());
  //         setForVersion('ad4mtoken', token);
  //         setForVersion('ad4murl', this.url);
          
  //         // Build the client with received credentials
  //         // The client will connect asynchronously and checkAuth() will be called
  //         // by the 'connected' callback in buildClient()
  //         // The embeddedResolve will be called when authState changes to 'authenticated'
  //         this.ad4mClient = this.buildClient();
  //       } catch (error) {
  //         console.error('[Ad4m Connect] Failed to initialize from AD4M_CONFIG:', error);
  //         if (this.embeddedReject) {
  //           this.embeddedReject(error as Error);
  //           this.embeddedResolve = undefined;
  //           this.embeddedReject = undefined;
  //         }
  //       }
  //     }
  //   });
    
  //   // Request AD4M config from parent window
  //   console.log('[Ad4m Connect] Requesting AD4M config from parent window');
  //   window.parent.postMessage({ type: 'REQUEST_AD4M_CONFIG' }, '*');
  // }

  // private notifyConfigChange(val: ConfigStates, data: string | number) {
  //   // Dispatch as DOM CustomEvent (EventTarget API)
  //   this.dispatchEvent(new CustomEvent("configstatechange", { 
  //     detail: { type: val, data } 
  //   }));
    
  //   // Also call legacy listeners for backwards compatibility
  //   this.listeners["configstatechange"].forEach((listener) => {
  //     listener(val, data);
  //   });
  // }

  // private notifyConnectionChange(val: ConnectionStates) {
  //   if (this.connectionState === val) return;
  //   this.connectionState = val;
    
  //   // Dispatch as DOM CustomEvent (EventTarget API)
  //   this.dispatchEvent(new CustomEvent("connectionstatechange", { detail: val }));
    
  //   // Also call legacy listeners for backwards compatibility
  //   this.listeners["connectionstatechange"].forEach((listener) => {
  //     listener(val);
  //   });
  // }

  // private notifyAuthChange(val: AuthStates) {
  //   if (this.authState === val) return;
  //   this.authState = val;
    
  //   // Dispatch as DOM CustomEvent (EventTarget API)
  //   this.dispatchEvent(new CustomEvent("authstatechange", { detail: val }));
    
  //   // Also call legacy listeners for backwards compatibility
  //   this.listeners["authstatechange"].forEach((listener) => {
  //     listener(val);
  //   });
    
  //   // In embedded mode, resolve the connect() promise when authenticated
  //   if (this.embeddedMode && val === "authenticated" && this.embeddedResolve) {
  //     this.embeddedResolve(this.ad4mClient);
  //     this.embeddedResolve = undefined;
  //     this.embeddedReject = undefined;
  //   }
  // }

  // setPort(port: number) {
  //   if (this.port === port) return;
  //   this.port = port;
  //   this.setUrl(`ws://localhost:${this.port}/graphql`);
  //   this.notifyConfigChange("port", port);
  // }

  // setUrl(url: string) {
  //   if (this.url === url) return;
  //   this.url = url;
  //   // Store URL in localStorage so auto-reconnect uses the correct backend
  //   setForVersion('ad4murl', url);
  //   this.notifyConfigChange("url", url);
  // }

  // setToken(token: string) {
  //   if (this.token === token) return;
  //   this.token = token;
  //   this.notifyConfigChange("token", token);
  // }

  // on(event: Event, cb: Function) {
  //   this.listeners[event].push(cb);
  // }

  // Create Apollo Client with GraphQL WS link and standard configuration


  // // Notify both connection and auth state changes (common pattern for errors/disconnections)
  // private notifyDisconnected(connectionState: ConnectionStates = "not_connected") {
  //   this.notifyConnectionChange(connectionState);
  //   this.notifyAuthChange("unauthenticated");
  // }

  // // Build a temporary client for detection/queries without changing state
  // buildTempClient(url: string): Ad4mClient {
  //   const wsClient = createClient({ url, connectionParams: async () => ({ headers: { authorization: "" } }) });
  //   const apolloClient = this.createApolloClient(wsClient);
  //   return new Ad4mClient(apolloClient);
  // }

  // If url is explicit , don't search for open ports
  // async connect(url?: string): Promise<Ad4mClient> {
  //   // // In embedded mode, wait for postMessage from parent instead of connecting
  //   // if (this.embeddedMode) {
  //   //   console.log('[Ad4m Connect] Embedded mode - waiting for AD4M config via postMessage');
      
  //   //   return new Promise((resolve, reject) => {
  //   //     // Set up timeout
  //   //     const timeout = setTimeout(() => {
  //   //       reject(new Error('Timeout waiting for AD4M config from parent window'));
  //   //     }, 30000); // 30 second timeout
        
  //   //     // Store resolvers to call when AD4M_CONFIG arrives
  //   //     this.embeddedResolve = (client: Ad4mClient) => {
  //   //       clearTimeout(timeout);
  //   //       console.log('[Ad4m Connect] Successfully connected in embedded mode');
  //   //       resolve(client);
  //   //     };
        
  //   //     this.embeddedReject = (error: Error) => {
  //   //       clearTimeout(timeout);
  //   //       reject(error);
  //   //     };
        
  //   //     // If we already have a client (message arrived before connect() was called)
  //   //     if (this.ad4mClient && this.authState === 'authenticated') {
  //   //       clearTimeout(timeout);
  //   //       console.log('[Ad4m Connect] Client already initialized in embedded mode');
  //   //       resolve(this.ad4mClient);
  //   //     }
  //   //   });
  //   // }
    
  //   // Standalone mode - return a Promise that resolves when authenticated
  //   return new Promise(async (resolve, reject) => {
  //     try {
  //       // Try to connect to local executor
  //       const localUrl = url || `ws://localhost:${this.port}/graphql`;
  //       console.log('[Ad4m Connect] Attempting connection to:', localUrl);
        
  //       await connectWebSocket(localUrl);
  //       this.setUrl(localUrl);
  //       this.ad4mClient = this.buildClient();
  //       console.log('*** new client built', this.ad4mClient);
  //       await this.checkAuth();
        
  //       // If already authenticated (has stored token), resolve immediately
  //       if (this.authState === 'authenticated' && this.ad4mClient) {
  //         console.log('[Ad4m Connect] Already authenticated, resolving immediately');
  //         resolve(this.ad4mClient);
  //       } else {
  //         // Not authenticated - UI modal will show
  //         // Set up listener to resolve when authentication completes
  //         console.log('[Ad4m Connect] Waiting for authentication...');
  //         const authHandler = (state: AuthStates) => {
  //           if (state === 'authenticated' && this.ad4mClient) {
  //             console.log('[Ad4m Connect] Authentication completed, resolving client');
  //             this.listeners['authstatechange'] = this.listeners['authstatechange'].filter(l => l !== authHandler);
  //             resolve(this.ad4mClient);
  //           }
  //         };
  //         this.on('authstatechange', authHandler);
  //       }
  //     } catch (error) {
  //       console.error('[Ad4m Connect] Connection failed:', error);
  //       this.notifyDisconnected();
  //       reject(error);
  //     }
  //   });
  // }

  // /**
  //  * Connect to a multi-user backend with authentication.
  //  * This method handles both password-based and email verification flows.
  //  * For UI-driven flows, use the ad4m-connect web component which provides a better UX.
  //  */
  // async connectMultiUser(): Promise<Ad4mClient> {
  //   try {
  //     // Connect to the multi-user backend
  //     const backendUrl = this.options.remoteUrl!;
  //     console.debug("[Ad4m Connect] Connecting to backend:", backendUrl);
  //     await connectWebSocket(backendUrl);

  //     this.setUrl(backendUrl);

  //     // Build client for user management operations (without token initially)
  //     const tempClient = this.buildClient();
  //     console.debug("[Ad4m Connect] Built client:", tempClient);

  //     // For backwards compatibility with servers that don't have email verification enabled,
  //     // try the old password-based login/signup flow
  //     let token: string;
  //     try {
  //       console.debug("[Ad4m Connect] Logging in user:", this.options.userEmail!);
  //       token = await tempClient.agent.loginUser(this.options.userEmail!, this.options.userPassword!);
  //       console.debug("[Ad4m Connect] Login successful");
  //     } catch (loginError) {
  //       console.debug("[Ad4m Connect] Login error:", loginError);
  //       // User doesn't exist, try to create
  //       console.debug("[Ad4m Connect] User does not exist, trying to create user:", this.options.userEmail!);
  //       try {
  //         console.debug("[Ad4m Connect] Creating user:", this.options.userEmail!);
  //         const createResult = await tempClient.agent.createUser(this.options.userEmail!, this.options.userPassword!);
  //         console.debug("[Ad4m Connect] Create result:", createResult);
  //         if (!createResult.success) {
  //           throw new Error(createResult.error || "Failed to create user. Server may require email verification.");
  //         }

  //         // Now login
  //         console.debug("[Ad4m Connect] Logging in user after creation:", this.options.userEmail!);
  //         token = await tempClient.agent.loginUser(this.options.userEmail!, this.options.userPassword!);
  //         console.log("[Ad4m Connect] Successfully created and logged in user");
  //       } catch (createError) {
  //         throw new Error(`Failed to create/login user: ${createError.message}. If email verification is enabled, please use the UI-driven flow instead.`);
  //       }
  //     }

  //     // Set the token and build authenticated client
  //     this.setToken(token);
  //     const authenticatedClient = this.buildClient();

  //     // Verify authentication
  //     await authenticatedClient.agent.status();

  //     // Store the authenticated client so it can be retrieved via getAd4mClient()
  //     this.ad4mClient = authenticatedClient;

  //     this.notifyAuthChange("authenticated");
  //     this.notifyConnectionChange("connected");

  //     return authenticatedClient;
  //   } catch (error) {
  //     this.notifyDisconnected("error");
  //     throw error;
  //   }
  // }

  // async loginToHosting(email: string, password: string) {
  //   try {
  //     const response = await fetch('https://hosting.ad4m.dev/api/auth/login', {
  //         method: 'POST',
  //         headers: {
  //             'Content-Type': 'application/json'
  //         },
  //         body: JSON.stringify({
  //             email,
  //             password
  //         })
  //     });

  //     if (response.status === 200) {
  //       const data = await response.json();
  //       // @ts-ignore
  //       localStorage.setItem('hosting_token', data.token);

  //       let token = localStorage.getItem('hosting_token');

  //       const response2 = await fetch('https://hosting.ad4m.dev/api/service/info', {
  //         method: 'GET',
  //         headers: {
  //             'Content-Type': 'application/json',
  //             'Authorization': 'Bearer ' + token
  //         },
  //       });

  //       if (response2.status === 200) {
  //         const data = await response2.json();

  //         if (data.serviceId) {
  //           this.setPort(data.port);
  //           this.setUrl(data.url);

  //           this.isHosting = true;

  //           setForVersion('ad4mhosting', 'true');

  //           if (!data.paused) {
  //             this.connect();
  //           } else {
  //             throw new Error('Hosting is not running');
  //           }
  //         }
  //       }
  //     }  else {
  //       const data = await response.json();

  //       if (data.message === 'Passwords did not match') {
  //         throw new Error('Passwords did not match');
  //       }
  //     }
  //   } catch (e) {
  //     console.log(e)
  //     throw new Error(`Error logging in ${e}`);
  //   }
  // }

  // async checkEmail(email: string) {
  //   try {
  //     const response = await fetch(`https://hosting.ad4m.dev/api/auth/check-email?email=${email}`, {
  //         method: 'GET',
  //         headers: {
  //               'Content-Type': 'application/json'
  //           }
  //       });

  //       return response.status === 200;
  //     } catch (e) {
  //       console.log(e)
  //     }
  // }

  // // If port is explicit, don't search for port
  // async connectToPort(port?: number): Promise<Ad4mClient> {
  //   try {
  //     if (port) {
  //       const found = await checkPort(port);
  //       this.setPort(found);

  //       return this.buildClient();
  //     } else {
  //       if (this.url.includes("localhost")) {
  //         const port = await this.findPort();
  //         this.setPort(port);

  //         return this.buildClient();
  //       }
  //     }
  //   } catch (error) {
  //     this.notifyDisconnected();
  //   }
  // }

  // async ensureConnection(): Promise<Ad4mClient> {
  //   const socketIsActive =
  //     this.activeSocket?.readyState === WebSocket.OPEN &&
  //     this.activeSocket?.url === this.url;

  //   if (socketIsActive && this.ad4mClient) {
  //     return this.ad4mClient;
  //   }

  //   try {
  //     this.notifyConnectionChange("connecting");

  //     await connectWebSocket(this.url);
  //     return this.buildClient();
  //   } catch (e) {
  //     this.notifyConnectionChange("not_connected");
  //     return this.connectToPort();
  //   }
  // }

  // async findPort(): Promise<number> {
  //   const ports = [...Array(10).keys()].map((_, i) => {
  //     return checkPort(DEFAULT_PORT + i);
  //   });

  //   const results = await Promise.allSettled(ports);
  //   const result = results.find((port) => port.status === "fulfilled");

  //   // @ts-ignore
  //   if (result) return result.value;
  //   else {
  //     throw Error("Couldn't find an open port");
  //   }
  // }

  // buildClient(): Ad4mClient {
  //   this.notifyConnectionChange("connecting");

  //   // Make sure the url is valid
  //   try {
  //     const websocket = new WebSocket(this.url, "graphql-transport-ws");
  //   } catch (e) {
  //     this.notifyConnectionChange("not_connected");
  //     return;
  //   }

  //   if (this.apolloClient && this.wsClient) {
  //     this.requestedRestart = true;
  //     this.wsClient.dispose();
  //     this.apolloClient.stop();
  //     this.wsClient = null;
  //     this.apolloClient = null;
  //   }

  //   this.wsClient = createClient({
  //     url: this.url,
  //     connectionParams: async () => ({
  //       headers: {
  //         authorization: this.token,
  //       },
  //     }),
  //     on: {
  //       opened: (socket: WebSocket) => {
  //         this.activeSocket = socket;
  //       },
  //       error: (e) => {
  //         this.notifyDisconnected();
  //       },
  //       connected: () => {
  //         this.notifyConnectionChange("connected");
  //         this.checkAuth();
  //       },
  //       closed: async (e: CloseEvent) => {
  //         // If the connection was closed cleanly, which happens on every
  //         // first connection, don't treat this as a disconnect
  //         if(e.wasClean) {
  //           return
  //         }

  //         // Iff the user explicitly requested a restart, also don't treat 
  //         // this as a disconnect (handling the disconnect makes sense when an
  //         // established connection gets lost after the first handshake)
  //         if (this.requestedRestart) {
  //           return
  //         }

  //         if (!this.token) {
  //           this.notifyDisconnected(!this.token ? "not_connected" : "disconnected");
  //           this.requestedRestart = false;
  //         } else {
  //           const client = await this.connect();
  //           if (client) {
  //             this.ad4mClient = client;
  //           } else {
  //             this.notifyDisconnected(!this.token ? "not_connected" : "disconnected");
  //             this.requestedRestart = false;
  //           }
  //         }
  //       },
  //     },
  //   });

  //   this.apolloClient = this.createApolloClient(this.wsClient);
  //   this.ad4mClient = new Ad4mClient(this.apolloClient);

  //   return this.ad4mClient;
  // }

  // async checkAuth(): Promise<boolean> {
  //   try {
  //     console.log('[Ad4m Connect] Checking authentication status...');
  //     const isLocked = await this.ad4mClient.agent.isLocked();

  //     if (isLocked) {
  //       console.log('[Ad4m Connect] Agent wallet is locked');
  //       this.notifyAuthChange("locked");
  //     } else {
  //       console.log('[Ad4m Connect] Agent wallet is unlocked, verifying status...');
  //       await this.ad4mClient.agent.status();
  //       this.notifyAuthChange("authenticated");
  //     }

  //     // Return true as we are authenticated
  //     return true;
  //   } catch (error) {
  //     console.error('[Ad4m Connect] Authentication check failed:', error);
  //     if (
  //       error.message ===
  //       "Socket closed with event 4500 Cannot extractByTags from a ciphered wallet. You must unlock first."
  //     ) {
  //       // TODO: isLocked throws an error, should just return a boolean. Temp fix
  //       this.notifyAuthChange("locked");
  //       return true;
  //     } else {
  //       this.notifyAuthChange("unauthenticated");
  //       return false;
  //     }
  //   }
  // }

  // async isAuthenticated(): Promise<boolean> {
  //   return this.authState === "authenticated";
  // }

  // async requestCapability(invalidateToken = false): Promise<string> {
  //   console.log('[Ad4m Connect] Requesting capability...');
  //   if (invalidateToken) {
  //     this.setToken(null);
  //   }

  //   if (this.isHosting) {
  //     let token = localStorage.getItem('hosting_token');

  //     const response = await fetch('https://hosting.ad4m.dev/api/service/checkStatus', {
  //       method: 'POST',
  //       headers: {
  //           'Content-Type': 'application/json',
  //           'Authorization': 'Bearer ' + token
  //       },
  //     });

  //     if (response.status !== 200) {
  //       console.error('Looks like the client is not running you might not recieve the mail with the code, please check your dashboard logs.');
  //     }
  //   }

  //   this.requestId = await this.ad4mClient?.agent.requestCapability({
  //     appName: this.appName,
  //     appDesc: this.appDesc,
  //     appUrl: this.appUrl,
  //     appIconPath: this.appIconPath,
  //     appDomain: this.appDomain,
  //     capabilities: this.capabilities,
  //   });

  //   return this.requestId;
  // }

  // async verifyCode(code: string): Promise<string> {
  //   try {
  //     const jwt = await this.ad4mClient?.agent.generateJwt(this.requestId!, code);
  //     this.setToken(jwt);
  //     await this.buildClient();
  //     await this.checkAuth();
  //     return this.token;
  //   } catch (error) {
  //     throw new Error("Invalid code");
  //   }
  // }

  // clearState() {
  //   this.setToken(null);
  //   this.setPort(DEFAULT_PORT);
  //   this.notifyDisconnected();
  // }
}
