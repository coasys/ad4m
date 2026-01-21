import { ApolloClient, InMemoryCache, NormalizedCacheObject } from "@apollo/client/core";
import { createClient, Client as WSClient } from "graphql-ws";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { isEmbedded, setLocal, getLocal, removeLocal, connectWebSocket } from './utils';
import { Ad4mClient, CapabilityInput, VerificationRequestResult } from "@coasys/ad4m";
import autoBind from "auto-bind";

import { Ad4mConnectOptions, ConnectionStates, AuthStates, ConfigStates } from './types';

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
  activeSocket: WebSocket | null = null;
  requestId?: string;
  requestedRestart: boolean = false;

  private embeddedResolve?: (client: Ad4mClient) => void;
  private embeddedReject?: (error: Error) => void;

  constructor(options: Ad4mConnectOptions) {
    super();
    autoBind(this);
  
    this.options = options;
    this.port = options.port || parseInt(getLocal("ad4m-port")) || DEFAULT_PORT
    this.url = options.url || getLocal("ad4m-url") || `ws://localhost:${this.port}/graphql`;
    this.token = getLocal("ad4m-token") || '';
    this.embedded = isEmbedded();

    if (this.embedded) this.initializeEmbeddedMode();
  }

  // Core connection flow
  async connect(): Promise<Ad4mClient> {
    // In embedded mode, wait for postMessage from parent instead of connecting
    if (this.embedded) {
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

    // Standalone mode - connect directly
    try {
      await connectWebSocket(this.url);
      setLocal("ad4m-url", this.url);
      this.ad4mClient = await this.buildClient();
      await this.checkAuth();
      return this.ad4mClient;
    } catch (error) {
      console.error('[Ad4m Connect] Connection failed:', error);
      this.notifyConnectionChange("error");
      throw error;
    }
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
            try {
              // Force a fresh connection by rebuilding the client
              // instead of potentially reusing a dead embedded client
              this.ad4mClient = await this.buildClient();
              await this.checkAuth();
            } catch (error) {
              console.error('[Ad4m Connect] Reconnection failed:', error);
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

  private async withTempClient<T>(url: string, callback: (client: Ad4mClient) => Promise<T>): Promise<T> {
    // Create a temporary client for the duration of the callback
    const wsClient = createClient({ url, connectionParams: async () => ({ headers: { authorization: "" } }) });
    const apolloClient = this.createApolloClient(wsClient);
    const client = new Ad4mClient(apolloClient);

    try {
      return await callback(client);
    } finally {
      wsClient.dispose();
    }
  }

  async checkAuth(): Promise<boolean> {
    try {
      console.log('[Ad4m Connect] Checking authentication status...');
      const isLocked = await this.ad4mClient.agent.isLocked();

      if (isLocked) {
        console.log('[Ad4m Connect] Agent wallet is locked');
        this.notifyAuthChange("locked");
      } else {
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

  // Embedded mode
  private initializeEmbeddedMode(): void {
    console.log('[Ad4m Connect] Running in embedded mode - waiting for AD4M config from parent');
    
    // Set up listener for AD4M_CONFIG from parent window
    window.addEventListener('message', async (event: MessageEvent) => {
      if (event.data?.type === 'AD4M_CONFIG') {
        // Security checks: verify sender
        if (event.source !== window.parent) {
          console.warn('[Ad4m Connect] Rejected AD4M_CONFIG from invalid source (not parent window)');
          return;
        }

        // Verify origin is in allowlist (if configured)
        if (this.options.allowedOrigins && this.options.allowedOrigins.length > 0) {
          if (!event.origin || !this.options.allowedOrigins.includes(event.origin)) {
            console.warn('[Ad4m Connect] Rejected AD4M_CONFIG from unauthorized origin:', event.origin);
            this.rejectEmbedded(new Error(`Unauthorized origin: ${event.origin}`));
            return;
          }
        }

        console.log('[Ad4m Connect] Received AD4M_CONFIG from parent:', { 
          port: event.data.port,
          hasToken: !!event.data.token 
        });
        
        // Validate and normalize port
        const { port: rawPort, token: rawToken } = event.data;
        
        if (rawPort === undefined || rawPort === null) {
          const error = new Error('AD4M_CONFIG missing required field: port');
          console.error('[Ad4m Connect]', error.message);
          this.rejectEmbedded(error);
          return;
        }
        
        const parsedPort = parseInt(rawPort, 10);
        if (!Number.isFinite(parsedPort) || parsedPort <= 0) {
          const error = new Error(`AD4M_CONFIG invalid port: ${rawPort} (must be a positive integer)`);
          console.error('[Ad4m Connect]', error.message);
          this.rejectEmbedded(error);
          return;
        }
        
        // Validate and normalize token (optional but must be string if present)
        const normalizedToken = rawToken !== undefined && rawToken !== null && typeof rawToken === 'string' 
          ? rawToken 
          : '';
        
        try {
          // Set connection details from parent (after successful validation)
          this.port = parsedPort;
          this.token = normalizedToken;
          this.url = `ws://localhost:${parsedPort}/graphql`;
          
          // Store in localStorage for persistence (avoid storing undefined or stale credentials)
          setLocal('ad4m-port', parsedPort.toString());
          if (normalizedToken) {
            setLocal('ad4m-token', normalizedToken);
          } else {
            // Explicitly clear stale token when absent/invalid to prevent reuse on reload
            removeLocal('ad4m-token');
          }
          setLocal('ad4m-url', this.url);
          
          // Build the client with received credentials
          this.ad4mClient = await this.buildClient();
          await this.checkAuth();
        } catch (error) {
          console.error('[Ad4m Connect] Failed to initialize from AD4M_CONFIG:', error);
          this.rejectEmbedded(error as Error);
        }
      }
    });
    
    // Request AD4M config from parent window
    console.log('[Ad4m Connect] Requesting AD4M config from parent window');
    window.parent.postMessage({ type: 'REQUEST_AD4M_CONFIG' }, '*');
  }

  // Local authentication
  private buildAppInfo() {
    return {
      appName: this.options.appInfo.name,
      appDesc: this.options.appInfo.description,
      appDomain: this.options.appInfo.url,
      appUrl: window.location.origin,
      appIconPath: this.options.appInfo.iconPath,
    };
  }

  async requestCapability(invalidateToken = false): Promise<string> {
    if (invalidateToken) {
      this.token = '';
      removeLocal('ad4m-token');
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

  async verifyLocalAd4mCode(code: string): Promise<boolean> {
    if (!this.requestId) {
      console.error('[Ad4m Connect] Cannot verify code: requestId is missing. Call requestCapability() first to obtain a requestId.');
      return false;
    }

    try {
      const jwt = await this.ad4mClient.agent.generateJwt(this.requestId, code);
      this.token = jwt;
      setLocal("ad4m-token", this.token);
      await this.connect();
      return true;
    } catch (error) {
      console.error('[Ad4m Connect] Code verification failed:', error);
      return false;
    }
  }

  // Remote authentication
  async isValidAd4mAPI(): Promise<boolean> {
    try {
      await this.withTempClient(this.url, async (client) => await client.runtime.info());
      return true;
    } catch (error) {
      console.error("[Ad4m Connect] Failed to verify AD4M API:", error);
      return false;
    }
  }

  async isMultiUser(): Promise<boolean> {
    try {
      return await this.withTempClient(this.url, async (client) => await client.runtime.multiUserEnabled());
    } catch (error) {
      console.error("[Ad4m Connect] Failed to detect multi-user mode:", error);
      // If multi-user query fails, assume single-user mode
      return false;
    }
  }

  async submitEmail(email: string): Promise<VerificationRequestResult> {
    try {
      return await this.withTempClient(this.url, async (client) => await client.agent.requestLoginVerification(email, this.buildAppInfo()));
    } catch (e) {
      const errorMessage = (e as Error).message || "Failed to process email. Please try again.";
      return { success: false, message: errorMessage, requiresPassword: false, isExistingUser: false };
    }
  }

  async verifyEmailCode(email: string, code: string): Promise<boolean> {
    try {
      const token = await this.withTempClient(this.url, async (client) => await client.agent.verifyEmailCode(email, code, "login"));
      this.token = token;
      setLocal("ad4m-token", this.token);
      await this.connect();
      return true;
    } catch (e) {
      console.error("[Ad4m Connect] Email code verification failed:", e);
      return false;
    }
  }

  async loginWithPassword(email: string, password: string): Promise<boolean> {
    try {
      const token = await this.withTempClient(this.url, async (client) => await client.agent.loginUser(email, password));
      this.token = token;
      setLocal("ad4m-token", this.token);
      await this.connect();
      return true;
    } catch (e) {
      console.error("[Ad4m Connect] Password login failed:", e);
      return false;
    }
  }

  async createAccount(email: string, password: string): Promise<boolean> {
    try {
      const token = await this.withTempClient(this.url, async (client) => {
        const result = await client.agent.createUser(email, password);
        // If creation successful, log in the new user
        if (result.success) return await client.agent.loginUser(email, password);
        // Otherwise, throw an error with the failure message
        else throw new Error(result.error || "Failed to create account");
      });

      this.token = token;
      setLocal("ad4m-token", this.token);
      await this.connect();
      return true;
    } catch (e) {
      console.error("[Ad4m Connect] Account creation error:", e);
      return false;
    }
  }

  // Private helpers
  private resolveEmbedded(client: Ad4mClient): void {
    if (this.embeddedResolve) {
      this.embeddedResolve(client);
      this.embeddedResolve = undefined;
      this.embeddedReject = undefined;
    }
  }

  private rejectEmbedded(error: Error): void {
    if (this.embeddedReject) {
      this.embeddedReject(error);
      this.embeddedResolve = undefined;
      this.embeddedReject = undefined;
    }
  }

  private notifyConnectionChange(value: ConnectionStates) {
    if (this.connectionState === value) return;
    this.connectionState = value;
    this.dispatchEvent(new CustomEvent("connectionstatechange", { detail: value }));
  }

  private notifyAuthChange(value: AuthStates) {
    if (this.authState === value) return;
    this.authState = value;
    this.dispatchEvent(new CustomEvent("authstatechange", { detail: value }));

    // In embedded mode, handle connect() promise resolution/rejection
    if (this.embedded) {
      // Resolve when authenticated
      if (value === "authenticated") {
        this.resolveEmbedded(this.ad4mClient);
      }
      
      // Reject on failing auth states
      if (value === "unauthenticated" || value === "locked") {
        const errorMessage = value === "locked" 
          ? "Authentication failed: Agent is locked"
          : "Authentication failed: Unauthenticated";
        this.rejectEmbedded(new Error(errorMessage));
      }
    }
  }

  private notifyConfigChange(type: ConfigStates, value: string) {
    this.dispatchEvent(new CustomEvent("configstatechange", { detail: { type, value } }));
  }

}
