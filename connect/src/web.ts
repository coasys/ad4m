import { BarcodeDetectorPolyfill } from "@undecaf/barcode-detector-polyfill";
import { css, html, LitElement } from "lit";
import { customElement, property, state } from "lit/decorators.js";

import Ad4mConnect, {
  Ad4mConnectOptions,
  AuthStates,
  ConnectionStates,
} from "./core";

import autoBind from "auto-bind";
import AgentLocked from "./components/AgentLocked";
import ConnectionOverview from "./components/ConnectionOverview";
import CouldNotMakeRequest from "./components/CouldNotMakeRequest";
import Disconnected from "./components/Disconnected";
import Header from "./components/Header";
import Hosting from "./components/Hosting";
import Loading from "./components/Loading";
import MobileAppLogoButton from "./components/MobileAppLogoButton";
import MultiUserAuth from "./components/MultiUserAuth";
import RemoteConnection from "./components/RemoteConnection";
import RequestCapability from "./components/RequestCapability";
import ScanQRCode from "./components/ScanQRCode";
import Settings from "./components/Settings";
import Start from "./components/Start";
import VerifyCode from "./components/VerifyCode";
import { connectWebSocket, DEFAULT_PORT, getForVersion, removeForVersion, setForVersion, isEmbedded } from "./utils";

function detectMob() {
  const toMatch = [
    /Android/i,
    /webOS/i,
    /iPhone/i,
    /iPad/i,
    /iPod/i,
    /BlackBerry/i,
    /Windows Phone/i,
  ];

  return toMatch.some((toMatchItem) => {
    return navigator.userAgent.match(toMatchItem);
  });
}

const styles = css`
  :host {
    --primary-color: #fff;
    --heading-color: #fff;
    --body-color: #ffffff;
    --success-color: #88f3b4;
    --background-color: #131533;
    --start-color: #a4adff;
    --end-color: #d273ff;
    --gradient: #91e3fd;
  }

  .wrapper {
    font-family: "Bricolage Grotesque", sans-serif;
    position: fixed;
    display: grid;
    place-content: center;
    top: 0;
    left: 0;
    color: var(--body-color);
    height: 100vh;
    width: 100vw;
    z-index: 100;
  }

  .mainlogo {
    position: fixed;
    bottom: 30px;
    right: 30px;
    height: 20px;
    width: 20px;
  }

  * {
    box-sizing: border-box;
  }

  .check-list {
    list-style: none;
    margin: 0;
    padding: 0;
  }

  .check-list li {
    display: flex;
    gap: 10px;
    margin-bottom: 10px;
  }

  .items {
    display: flex;
    flex-direction: column;
    flex-gap: 50px;
    gap: 50px;
  }

  .items--small {
    flex-gap: 20px;
    gap: 20px;
  }

  .button {
    text-decoration: none;
    cursor: pointer;
    border: 0;
    color: var(--background-color);
    background: var(--gradient);
    height: 50px;
    font-weight: 600;
    min-width: 100px;
    padding: 0px 30px;
    border-radius: 8px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    text-align: center;
    font-family: inherit;
    font-size: 15px;
  }

  @media (min-width: 800px) {
    .button {
      min-width: 200px;
      padding: 0px 40px;
      font-size: 17px;
    }
  }

  .heading {
    color: var(--heading-color);
    font-size: 18px;
    font-weight: 600;
    margin: 0;
    margin-bottom: 10px;
  }

  .heading.nomargin {
    margin: 0;
  }

  .body {
    padding: 0;
    margin: 0;
    font-size: 14px;
    line-height: 1.5;
  }

  .buttons {
    display: flex;
    align-items: center;
    justify-content: center;
    flex-gap: 10px;
    gap: 10px;
  }

  .button--full {
    width: 100%;
    display: flex;
  }

  .button--link {
    padding: 0;
    height: auto;
    background: none;
    color: var(--primary-color);
    font-size: inherit;
    min-width: auto;
    text-decoration: none;
  }

  .button--link:hover {
    text-decoration: underline;
  }

  .button--secondary {
    background: var(--background-color);
    border: 1px solid var(--gradient);
    color: var(--primary-color);
  }

  .dialog {
    background-color: var(--background-color);
    z-index: 10;
    border-radius: 8px;
    width: calc(100vw - 10px);
    max-width: 500px;
    max-height: 90vh;
    overflow-y: auto;
  }

  .dialog__header {
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 20px 30px;
    gap: 10px;
    color: var(--primary-color);
    margin-bottom: 20px;
    font-size: 14px;
  }

  .dialog__logo {
    display: flex;
    text-align: center;
    width: 25px;
  }

  .dialog__content {
    min-height: 300px;
    display: grid;
    place-content: center;
    padding-top: 0;
    padding-left: 30px;
    padding-right: 30px;
    padding-bottom: 30px;
  }

  .dialog__connect {
    display: flex;
    justify-content: center;
    align-items: center;
    flex-gap: 50px;
    gap: 50px;
    position: relative;
  }

  .dialog__logo svg {
    width: 100%;
  }

  .dialog__connect-ad4m {
    width: 80px;
    height: 80px;
    background: var(--background-color);
    padding: 20px;
    box-shadow: 0px 4px 7px 0px rgb(0 0 0 / 8%);
    border-radius: 50%;
    position: relative;
  }

  .dialog__connect-ad4m:before {
    content: "";
    position: absolute;
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    z-index: -1;
    margin: -2px;
    border-radius: inherit;
    background: var(--gradient);
  }

  .dialog__connect-app {
    width: 80px;
    height: 80px;
  }

  .dialog__connect-check:before {
    content: "";
    display: block;
    width: 40px;
    border-bottom: 2px dashed var(--body-color);
    position: absolute;
    left: 50%;
    top: 50%;
    transform: translateY(-50%) translateX(-50%);
  }

  .dialog__connect-check svg {
    position: relative;
  }

  .text-center {
    text-align: center;
  }

  .uppercase {
    text-transform: uppercase;
  }

  .input {
    display: flex;
    flex-direction: column;
    flex-gap: 10px;
    gap: 10px;
  }

  .input__label {
    font-size: 12px;
  }

  .input__field {
    border-radius: 8px;
    outline: 0;
    height: 60px;
    color: var(--heading-color);
    background-color: var(--background-color);
    padding: 0px 30px;
    font-size: 20px;
    border: 1px solid var(--body-color);
  }

  .input__field:focus {
    border: 1px solid var(--primary-color);
    box-shadow: 0px 0px 0px 1px var(--primary-color);
  }

  .ad4mConnect__backdrop {
    position: absolute;
    top: 0;
    left: 0;
    height: 100vh;
    width: 100vw;
    background-color: rgba(0, 0, 0, 0.5);
  }

  .ad4mConnect__locked {
    position: fixed;
    top: 0;
    left: 0;
    background: var(--background-color);
    height: 100vh;
    width: 100vw;
    padding: 36px;
    display: flex;
    align-items: center;
    flex-direction: column;
    font-family: "Comfortaa", cursive;
  }

  .lds-ring {
    display: block;
    position: relative;
    width: 80px;
    height: 80px;
    margin: 0 auto;
    margin-top: 24px;
  }
  .lds-ring div {
    box-sizing: border-box;
    display: block;
    position: absolute;
    width: 64px;
    height: 64px;
    margin: 4px;
    border: 4px solid var(--primary-color);
    border-radius: 50%;
    animation: lds-ring 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite;
    border-color: var(--primary-color) transparent transparent transparent;
  }
  .lds-ring div:nth-child(1) {
    animation-delay: -0.45s;
  }
  .lds-ring div:nth-child(2) {
    animation-delay: -0.3s;
  }
  .lds-ring div:nth-child(3) {
    animation-delay: -0.15s;
  }
  @keyframes lds-ring {
    0% {
      transform: rotate(0deg);
    }
    100% {
      transform: rotate(360deg);
    }
  }

  .md-ring {
    display: block;
    position: relative;
    width: 30px;
    height: 30px;
    margin-right: 10px;
  }
  .md-ring div {
    box-sizing: border-box;
    display: block;
    position: absolute;
    width: 24px;
    height: 24px;
    margin: 4px;
    border: 2px solid var(--primary-color);
    border-radius: 50%;
    animation: md-ring 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite;
    border-color: var(--primary-color) transparent transparent transparent;
  }
  .md-ring div:nth-child(1) {
    animation-delay: -0.45s;
  }
  .md-ring div:nth-child(2) {
    animation-delay: -0.3s;
  }
  .md-ring div:nth-child(3) {
    animation-delay: -0.15s;
  }
  @keyframes md-ring {
    0% {
      transform: rotate(0deg);
    }
    100% {
      transform: rotate(360deg);
    }
  }

  .disconnected {
    position: fixed;
    top: 0;
    left: 0;
    width: 100vw;
    padding: 10px 0;
    text-align: center;
    background: red;
  }

  .qr-scanner {
    background: black;
    position: fixed;
    left: 0;
    top: 0;
    width: 100%;
    height: 100%;
    display: grid;
    grid-template-columns: 1fr;
    place-content: center;
  }

  .qr-scanner .stop {
    position: absolute;
    z-index: 10;
    left: 50%;
    bottom: 10px;
    transform: translateX(-50%);
  }

  .qr-scanner video {
    height: 100vh;
    width: 100vw;
    object-fit: cover;
  }
`;

@customElement("ad4m-connect")
export class Ad4mConnectElement extends LitElement {
  static styles = [styles];

  @state()
  private _code = null;

  @state()
  private _isMobile = null;

  @state()
  private _hasClickedDownload = null;

  @state()
  private _isRemote = false;

  @state()
  private _client: Ad4mConnect;

  @state()
  private _isOpen: boolean = false;

  @state()
  private _hostingStep = 0;

  @state()
  private _email = "";

  @state()
  private _passowrd = "";

  @state()
  private _passwordError = null;

  @state()
  private _hostingNotRunningError = null;

  @state()
  private _verifyCodeError = null;

  @state()
  private _isHostingLoading = false;

  @state()
  private _multiUserEmail = "";

  @state()
  private _multiUserPassword = "";

  @state()
  private _multiUserVerificationCode = "";

  @state()
  private _multiUserError: string | null = null;

  @state()
  private _multiUserLoading = false;

  // Timeout reference for auto-submit when 6 digits are entered
  private _codeSubmitTimeout: ReturnType<typeof setTimeout> | null = null;

  @state()
  private _multiUserStep: "email" | "password" | "code" = "email";

  @state()
  private _multiUserVerificationType: "signup" | "login" = "login";

  @state()
  private _localDetected = false;

  @state()
  private _remoteUrl = "";

  @state()
  private _remoteMultiUserDetected: boolean | null = null;

  @state()
  private _remoteDetecting = false;

  @state()
  private _remoteError: string | null = null;

  @state()
  private uiState:
    | "connection_overview"
    | "remote_connection"
    | "settings"
    | "start"
    | "qr"
    | "requestcap"
    | "verifycode"
    | "disconnected"
    | "hosting"
    | "agentlocked"
    | "multiuser_auth" = "connection_overview";

  @property({ type: String, reflect: true })
  appName = null;

  @property({ type: String, reflect: true })
  appDesc = null;

  @property({ type: String, reflect: true })
  appDomain = null;

  @property({ type: String, reflect: true })
  appIconPath = null;

  @property({ type: Boolean, reflect: true })
  mobile = null;

  @property({ type: String, reflect: true })
  capabilities = [];

  @property({ type: Boolean, reflect: true })
  hosting = getForVersion("ad4mhosting") === "true" || false;

  // TODO: localstorage doesnt work here
  @property({ type: String })
  token = getForVersion("ad4mToken") || "";

  // TODO: localstorage doesnt work here
  @property({ type: String, reflect: true })
  port = parseInt(getForVersion("ad4mport")) || DEFAULT_PORT;

  // TODO: localstorage doesnt work here
  @property({ type: String, reflect: true })
  url = getForVersion("ad4murl") || "";

  @property({ type: Boolean, reflect: true })
  multiUser = false;

  @property({ type: String, reflect: true })
  backendUrl = "";

  // Core instance - can be passed in or will be created internally
  @property({ type: Object })
  core?: Ad4mConnect;

  get authState(): AuthStates {
    return this._client.authState;
  }

  get connectionState(): ConnectionStates {
    return this._client.connectionState;
  }

  /**
   * Initialize the Ad4mConnect client without mounting UI to DOM
   * Used for embedded mode where we don't need the visual UI
   * Returns the Ad4mConnect client instance
   */
  public initializeWithoutUI(): Ad4mConnect {
    autoBind(this);

    // Use provided core instance or create a new one
    if (this.core) {
      this._client = this.core;
    } else {
      this._client = new Ad4mConnect({
        appName: this.appName,
        appDesc: this.appDesc,
        appDomain: this.appDomain,
        appUrl: window.location.origin,
        appIconPath: this.appIconPath,
        capabilities: Array.isArray(this.capabilities)
          ? this.capabilities
          : JSON.parse(this.capabilities),
        port: this.port || parseInt(getForVersion("ad4mport")) || DEFAULT_PORT,
        token: this.token || getForVersion("ad4mtoken"),
        url: this.url || getForVersion("ad4murl"),
        hosting: this.hosting,
      });
    }

    // Set up event listeners for state tracking
    this._client.on("configstatechange", this.handleConfigChange);
    this._client.on("authstatechange", this.handleAuthChange);
    this._client.on("connectionstatechange", this.handleConnectionChange);
    
    return this._client;
  }

  private injectFont() {
    const link = document.createElement("link");
    link.setAttribute("rel", "stylesheet");
    link.setAttribute(
      "href",
      `https://fonts.googleapis.com/css2?family=Bricolage+Grotesque:opsz,wght@12..96,200..800&display=swap`
    );
    document.head.appendChild(link);
  }

  connectedCallback() {
    super.connectedCallback();
    autoBind(this);

    this.injectFont();

    this._isMobile = detectMob();

    // Use provided core instance or create a new one
    if (this.core) {
      this._client = this.core;
    } else {
      this._client = new Ad4mConnect({
        appName: this.appName,
        appDesc: this.appDesc,
        appDomain: this.appDomain,
        appUrl: window.location.origin,
        appIconPath: this.appIconPath,
        capabilities: Array.isArray(this.capabilities)
          ? this.capabilities
          : JSON.parse(this.capabilities),
        port: this.port || parseInt(getForVersion("ad4mport")) || DEFAULT_PORT,
        token: this.token || getForVersion("ad4mtoken"),
        url: this.url || getForVersion("ad4murl"),
        hosting: this.hosting,
      });
    }

    // Don't show UI if running in embedded mode - core will handle connection
    if (isEmbedded()) {
      console.log('[Ad4m Connect UI] Running in embedded mode - UI will not be shown');
      this._isOpen = false;
      
      // Still set up event listeners so app can track state
      this._client.on("configstatechange", this.handleConfigChange);
      this._client.on("authstatechange", this.handleAuthChange);
      this._client.on("connectionstatechange", this.handleConnectionChange);
      
      return; // Skip rest of UI initialization
    }

    this._client.on("configstatechange", this.handleConfigChange);
    this._client.on("authstatechange", this.handleAuthChange);
    this._client.on("connectionstatechange", this.handleConnectionChange);

    this.loadFont();

    const storedToken = this.token || getForVersion("ad4mtoken");

    if (storedToken) {
      // If we have a stored token, try to auto-connect without showing UI
      // UI will only show if authentication fails
      // First check if there's a stored URL (from previous connection)
      const storedUrl = getForVersion("ad4murl");
      
      if (storedUrl) {
        // Use the stored URL (preserves local vs remote choice)
        this._client.connect(storedUrl);
      } else {
        // No stored URL - this means first time or cleared storage
        // Default to local connection (user can choose remote from UI if needed)
        // The backendUrl is only used when explicitly selected from UI, not on auto-connect
        this._client.connect();
      }
    } else {
      // No stored token - show connection overview
      this.detectLocal();
      this.changeUIState("connection_overview");
      this._isOpen = true;
    }
  }

  private async checkEmail() {
    try {
      const exist = await this._client.checkEmail(this._email);

      if (exist) {
        this._hostingStep = 1;
      } else {
        this._hostingStep = 2;
      }
    } catch (e) {
      console.log(e);
    }
  }

  private async handleMultiUserEmailSubmit() {
    try {
      this._multiUserLoading = true;
      this._multiUserError = null;

      // Build temporary client to call requestLoginVerification
      const tempClient = this._client.buildTempClient(this.backendUrl);

      // Build AuthInfo object from app properties
      const appInfo = {
        appName: this.appName,
        appDesc: this.appDesc,
        appDomain: this.appDomain,
        appUrl: window.location.origin,
        appIconPath: this.appIconPath,
      };

      const result = await tempClient.agent.requestLoginVerification(this._multiUserEmail, appInfo);

      if (result.success && !result.requiresPassword) {
        // User exists, verification email sent
        this._multiUserStep = "code";
        this._multiUserVerificationType = "login";
      } else if (result.requiresPassword) {
        // Determine if this is login or signup based on whether user exists
        // If isExistingUser is true, show password for login; otherwise for signup
        this._multiUserStep = "password";
        this._multiUserVerificationType = result.isExistingUser ? "login" : "signup";
      } else {
        this._multiUserError = result.message || "Failed to send verification email";
      }
    } catch (e) {
      this._multiUserError = e.message || "Failed to process email. Please try again.";
    } finally {
      this._multiUserLoading = false;
    }
  }

  private async handleMultiUserPasswordSubmit() {
    try {
      this._multiUserLoading = true;
      this._multiUserError = null;

      // Build temporary client
      const tempClient = this._client.buildTempClient(this.backendUrl);

      if (this._multiUserVerificationType === "login") {
        // Existing user - try password login
        try {
          const token = await tempClient.agent.loginUser(this._multiUserEmail, this._multiUserPassword);
          
          // Success! Store token and authenticate
          this._client.setToken(token);
          this._client.setUrl(this.backendUrl);

          // Rebuild client with new token to establish authenticated connection
          await this._client.buildClient();
          await this._client.checkAuth();

          // Clear form and close modal
          this._multiUserEmail = "";
          this._multiUserPassword = "";
          this._multiUserVerificationCode = "";
          this._multiUserStep = "email";
          this._isOpen = false;
          this.changeUIState("connected");
          this.handleAuthChange("authenticated");
        } catch (loginError) {
          this._multiUserError = loginError.message || "Invalid email or password. Please try again.";
        }
      } else {
        // New user - create account
        // Build AuthInfo object from app properties
        const appInfo = {
          appName: this.appName,
          appDesc: this.appDesc,
          appDomain: this.appDomain,
          appUrl: window.location.origin,
          appIconPath: this.appIconPath,
        };

        const result = await tempClient.agent.createUser(this._multiUserEmail, this._multiUserPassword, appInfo);

        if (result.success) {
          // Check if email verification was sent or if we should proceed directly to login
          if (result.error && result.error.includes("SMTP is not configured")) {
            // SMTP not configured - login immediately with password
            try {
              const token = await tempClient.agent.loginUser(this._multiUserEmail, this._multiUserPassword);
              
              // Success! Store token and authenticate
              this._client.setToken(token);
              this._client.setUrl(this.backendUrl);

              // Rebuild client with new token to establish authenticated connection
              await this._client.buildClient();
              await this._client.checkAuth();

              // Clear form and close modal
              this._multiUserEmail = "";
              this._multiUserPassword = "";
              this._multiUserVerificationCode = "";
              this._multiUserStep = "email";
              this._isOpen = false;
              this.changeUIState("connected");
              this.handleAuthChange("authenticated");
            } catch (loginError) {
              this._multiUserError = loginError.message || "Account created but login failed. Please try logging in.";
            }
          } else if (!result.error) {
            // User created, verification email sent
            this._multiUserStep = "code";
            this._multiUserVerificationType = "signup";
          } else {
            // User created but email failed - still allow login
            this._multiUserError = result.error + " You can try logging in with your password.";
          }
        } else {
          this._multiUserError = result.error || "Failed to create account. Please try again.";
        }
      }
    } catch (e) {
      this._multiUserError = e.message || "Failed to process request. Please try again.";
    } finally {
      this._multiUserLoading = false;
    }
  }

  private async handleMultiUserCodeSubmit() {
    // Guard against duplicate calls - if already loading, ignore this request
    if (this._multiUserLoading) {
      return;
    }

    // Cancel any pending auto-submit timeout since we're submitting now
    if (this._codeSubmitTimeout !== null) {
      clearTimeout(this._codeSubmitTimeout);
      this._codeSubmitTimeout = null;
    }

    try {
      this._multiUserLoading = true;
      this._multiUserError = null;

      // Build temporary client to call verifyEmailCode
      const tempClient = this._client.buildTempClient(this.backendUrl);
      const token = await tempClient.agent.verifyEmailCode(
        this._multiUserEmail,
        this._multiUserVerificationCode,
        this._multiUserVerificationType
      );

      // Success! Store token and authenticate
      this._client.setToken(token);
      this._client.setUrl(this.backendUrl);

      // Rebuild client with new token to establish authenticated connection
      await this._client.buildClient();
      await this._client.checkAuth();

      // Clear form and close modal
      this._multiUserEmail = "";
      this._multiUserPassword = "";
      this._multiUserVerificationCode = "";
      this._multiUserStep = "email";
      this._isOpen = false;
      this.changeUIState("connected");
      this.handleAuthChange("authenticated");
    } catch (e) {
      this._multiUserError = "Invalid or expired code. Please try again.";
      this._multiUserVerificationCode = "";
    } finally {
      this._multiUserLoading = false;
    }
  }

  private handleMultiUserBackToEmail() {
    // Cancel any pending auto-submit timeout
    if (this._codeSubmitTimeout !== null) {
      clearTimeout(this._codeSubmitTimeout);
      this._codeSubmitTimeout = null;
    }
    this._multiUserStep = "email";
    this._multiUserPassword = "";
    this._multiUserVerificationCode = "";
    this._multiUserError = null;
  }

  private changeMultiUserEmail(email: string) {
    this._multiUserEmail = email;
    this._multiUserError = null; // Clear error when user types
  }

  private changeMultiUserPassword(password: string) {
    this._multiUserPassword = password;
    this._multiUserError = null; // Clear error when user types
  }

  private changeMultiUserVerificationCode(code: string) {
    // Cancel any pending auto-submit timeout
    if (this._codeSubmitTimeout !== null) {
      clearTimeout(this._codeSubmitTimeout);
      this._codeSubmitTimeout = null;
    }

    this._multiUserVerificationCode = code;
    this._multiUserError = null; // Clear error when user types

    // Auto-submit when 6 digits entered (with debounce to prevent race conditions)
    if (code.length === 6 && !this._multiUserLoading) {
      this._codeSubmitTimeout = setTimeout(() => {
        this._codeSubmitTimeout = null;
        // Only submit if still 6 digits and not already loading
        if (this._multiUserVerificationCode.length === 6 && !this._multiUserLoading) {
          this.handleMultiUserCodeSubmit();
        }
      }, 100);
    }
  }

  private async detectLocal() {
    try {
      await connectWebSocket(`ws://localhost:${this.port}/graphql`, 3000);
      this._localDetected = true;
    } catch (error) {
      console.log("[Ad4m Connect] Local detection failed:", error);
      this._localDetected = false;
    }
  }

  private async verifyAd4mApi(url: string): Promise<void> {
    console.log("[Ad4m Connect] Verifying AD4M API at URL:", url);
    const tempClient = this._client.buildTempClient(url);

    try {
      await tempClient.runtime.info();
      console.log("[Ad4m Connect] AD4M API verified");
    } catch (error) {
      console.error("[Ad4m Connect] Failed to verify AD4M API:", error);
      throw new Error("Server is reachable but doesn't appear to be an AD4M executor. Make sure the URL includes '/graphql'.");
    }
  }

  private async detectRemoteMultiUser(url: string): Promise<boolean> {
    console.log("[Ad4m Connect] Detecting multi-user mode for URL:", url);
    const tempClient = this._client.buildTempClient(url);

    try {
      const multiUserEnabled = await tempClient.runtime.multiUserEnabled();
      console.log("[Ad4m Connect] Multi-user detection result:", multiUserEnabled);
      return multiUserEnabled;
    } catch (error) {
      console.error("[Ad4m Connect] Failed to detect multi-user mode:", error);
      console.error("[Ad4m Connect] Error details:", error.message, error.stack);
      // If multi-user query fails, assume single-user mode
      return false;
    }
  }

  private async handleConnectLocal() {
    try {
      this.changeUIState("requestcap");
      await this._client.connect();
    } catch (error) {
      console.error("Failed to connect to local AD4M:", error);
      this.changeUIState("connection_overview");
    }
  }

  private handleShowRemoteConnection() {
    // Use configured URL in priority order: backendUrl, url, or empty
    this._remoteUrl = this.backendUrl || this.url || "";
    this._remoteMultiUserDetected = null;
    this._remoteError = null;
    this.changeUIState("remote_connection");
  }

  private handleRemoteUrlChange(url: string) {
    this._remoteUrl = url;
    this._remoteError = null;
  }

  private async handleRemoteConnect() {
    if (!this._remoteUrl) {
      this._remoteError = "Please enter a URL";
      return;
    }

    this._remoteDetecting = true;
    this._remoteError = null;
    this._remoteMultiUserDetected = null; // Reset detection state

    try {
      // Step 1: Check if the server is reachable at all
      console.log("[Ad4m Connect] Checking if server is reachable:", this._remoteUrl);
      await connectWebSocket(this._remoteUrl, 5000);
      console.log("[Ad4m Connect] Server is reachable");

      // Step 2: Verify it's actually an AD4M API
      await this.verifyAd4mApi(this._remoteUrl);

      // Step 3: Detect if multi-user is enabled
      const isMultiUser = await this.detectRemoteMultiUser(this._remoteUrl);
      this._remoteMultiUserDetected = isMultiUser;
      this._remoteDetecting = false;
    } catch (error) {
      console.error("[Ad4m Connect] Connection/detection failed:", error);
      this._remoteError = error.message || "Cannot reach server at this URL. Please check the URL and try again.";
      this._remoteMultiUserDetected = null; // Keep as null to not show connection options
      this._remoteDetecting = false;
    }
  }

  private async handleRemoteMultiUserAuth() {
    // Set the backend URL and show multi-user auth
    this.backendUrl = this._remoteUrl;
    this.changeUIState("multiuser_auth");
  }

  private async handleRemoteRequestCapability() {
    try {
      this.changeUIState("requestcap");
      await this._client.connect(this._remoteUrl);
    } catch (error) {
      this._remoteError = error.message || "Failed to connect";
      this.changeUIState("remote_connection");
    }
  }

  private async loginToHosting() {
    try {
      await this._client.loginToHosting(this._email, this._passowrd);

      this.changeUIState("connected");
    } catch (e) {
      if (e.message.includes("Passwords did not match")) {
        this._passwordError = "Passwords did not match";
      } else {
        this._hostingNotRunningError = "Hosting is not running";
      }
    }
  }

  private changeEmail(email: string) {
    this._email = email;
  }

  private changePassword(passowrd: string) {
    this._passowrd = passowrd;
  }

  private async unlockAgent(passcode, holochain = true) {
    await this._client.ad4mClient.agent.unlock(passcode, holochain);
  }

  private async verifyCode(code) {
    try {
      await this._client.verifyCode(code);
    } catch (e) {
      this._verifyCodeError = "Invalid code";
    }
  }

  private changeUrl(url) {
    if (url !== this._client.url) {
      removeForVersion("ad4mtoken");
      this._client.setToken(null);
    }

    this._client.setUrl(url);
  }

  private changePort(port: number) {
    this._client.setPort(port);
  }

  private changeUIState(state) {
    this.uiState = state;
  }

  private setIsHostingRunning(val) {
    this._hostingNotRunningError = val;
  }

  private changeIsRemote(bol: boolean) {
    this._isRemote = bol;
  }

  private changeCode(code) {
    this._code = code;
  }

  private onDownloaded() {
    this._hasClickedDownload = true;
  }

  private handleAuthChange(event: AuthStates) {
    const customEvent = new CustomEvent("authstatechange", {
      detail: event,
    });
    if (event === "locked") {
      this._isOpen = true;
    }
    if (event === "authenticated") {
      this._isOpen = false;
    }
    this.dispatchEvent(customEvent);
    this.requestUpdate();
  }

  private handleConfigChange(name: any, val: string) {
    this[name] = val;
    if (val) {
      setForVersion("ad4m" + name, val);
    } else {
      removeForVersion("ad4m" + name);
    }
    this.requestUpdate();
  }

  private handleConnectionChange(event: ConnectionStates) {
    if (event === "checking_local") {
      this._isOpen = false; // Don't show dialog while checking
      return;
    }

    if (event === "connected") {
      if (this.authState !== "authenticated") {
        this.changeUIState("requestcap");
        this._isOpen = true;
      } else {
        this.changeUIState("connected");
        this._isOpen = false;
      }
    }
    if (event === "disconnected") {
      this._isOpen = true;
    }
    if (event === "not_connected") {
      this._isOpen = true;
      // If multi-user mode is enabled, show multi-user auth instead of start
      if (this.multiUser && this.backendUrl) {
        this.changeUIState("multiuser_auth");
      } else {
        this.changeUIState("start");
      }
    }
    const customEvent = new CustomEvent("connectionstatechange", {
      detail: event,
    });
    this.dispatchEvent(customEvent);
    this.requestUpdate();
  }

  private loadFont() {
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.type = "text/css";
    link.crossOrigin = "anonymous";
    link.href =
      "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;700&display=swap";
    document.head.appendChild(link);
  }

  private async startCamera(e) {
    try {
      window["BarcodeDetector"].getSupportedFormats();
    } catch {
      window["BarcodeDetector"] = BarcodeDetectorPolyfill;
    }

    await this.changeUIState("qr");

    setTimeout(async () => {
      const video = this.shadowRoot.querySelector("video");

      if (!video) return;

      const media = await navigator.mediaDevices.getUserMedia({
        audio: false,
        video: { facingMode: "environment" },
      });

      video.srcObject = media;
      video.autoplay = true;
    }, 100);
  }

  async connect() {
    this._isOpen = true;
    this.requestUpdate();
    const client = await this._client.connect();
    try {
      const status = await client.agent.status();
      if (status.isUnlocked && status.isInitialized) {
        window.location.reload();
      }
    } catch (e) {
      console.warn(e);
    }
    return client;
  }

  getAd4mClient() {
    return this._client.ad4mClient;
  }

  getCore() {
    return this._client;
  }

  async connectRemote(url) {
    try {
      this.changeUrl(url);
      const client = await this._client.connect(url);
      const status = await client.agent.status();
      if (status.isUnlocked && status.isInitialized) {
        return client;
      }

      this.changeUIState("requestcap");
      return client;
    } catch (e) {
      this.changeUIState("requestcap");
      this._isOpen = true;
    }
  }

  async requestCapability(bool) {
    try {
      await this._client.requestCapability(bool);
      this.changeUIState("verifycode");
    } catch (e) {
      console.warn(e);
    }
  }

  async isAuthenticated() {
    return this._client.checkAuth();
  }

  setOpen(val: boolean) {
    this._isOpen = val;
  }

  setHostingStep(step: number) {
    this._hostingStep = step;
  }

  clearState() {
    this.handleConfigChange("port", null);
    this.handleConfigChange("url", null);
    this.handleConfigChange("token", null);
    // this._isOpen = false;
    this.handleConnectionChange("not_connected");
    this.handleAuthChange("unauthenticated");
    this.changeUIState("start");

    this._client.clearState();
  }

  renderViews() {
    if (this.connectionState === "connecting") {
      return Loading();
    }

    if (this.uiState === "connection_overview") {
      return ConnectionOverview({
        localDetected: this._localDetected,
        multiUserConfigured: this.multiUser && !!this.backendUrl,
        backendUrl: this.backendUrl,
        configuredUrl: this.url,
        isMobile: this._isMobile,
        onConnectLocal: this.handleConnectLocal,
        onConnectRemote: this.handleShowRemoteConnection,
        onScanQR: () => { this.startCamera(null); },
        onDownloadAd4m: () => { window.open("https://github.com/coasys/ad4m/releases"); },
      });
    }

    if (this.uiState === "remote_connection") {
      return RemoteConnection({
        initialUrl: this._remoteUrl,
        detecting: this._remoteDetecting,
        multiUserDetected: this._remoteMultiUserDetected,
        error: this._remoteError,
        onBack: () => this.changeUIState("connection_overview"),
        onUrlChange: this.handleRemoteUrlChange,
        onConnect: this.handleRemoteConnect,
        onMultiUserAuth: this.handleRemoteMultiUserAuth,
        onRequestCapability: this.handleRemoteRequestCapability,
      });
    }

    if (this.uiState === "multiuser_auth") {
      return MultiUserAuth({
        email: this._multiUserEmail,
        password: this._multiUserPassword,
        verificationCode: this._multiUserVerificationCode,
        error: this._multiUserError,
        isLoading: this._multiUserLoading,
        backendUrl: this.backendUrl,
        step: this._multiUserStep,
        verificationType: this._multiUserVerificationType,
        changeEmail: this.changeMultiUserEmail,
        changePassword: this.changeMultiUserPassword,
        changeVerificationCode: this.changeMultiUserVerificationCode,
        onEmailSubmit: this.handleMultiUserEmailSubmit,
        onPasswordSubmit: this.handleMultiUserPasswordSubmit,
        onCodeSubmit: this.handleMultiUserCodeSubmit,
        onBackToEmail: this.handleMultiUserBackToEmail,
      });
    }

    if (this.uiState === "hosting") {
      return Hosting({
        email: this._email,
        password: this._passowrd,
        changeEmail: this.changeEmail,
        changePassword: this.changePassword,
        changeState: this.changeUIState,
        step: this._hostingStep,
        setHostingStep: this.setHostingStep,
        login: this.loginToHosting,
        checkEmail: this.checkEmail,
        passwordError: this._passwordError,
        isHostingRunning: this._hostingNotRunningError,
        setIsHostingRunning: this.setIsHostingRunning,
      });
    }

    if (this.uiState === "qr") {
      return ScanQRCode({
        changeState: this.changeUIState,
        onSuccess: (url) => {
          this.changeUrl(url);
          this._client.connect(url);
        },
        uiState: this.uiState,
      });
    }

    if (this.authState === "locked") {
      return AgentLocked({
        unlockAgent: this.unlockAgent,
        reconnect: this.connect,
      });
    }

    if (this.uiState === "settings") {
      return Settings({
        port: this.port,
        changePort: this.changePort,
        isRemote: this._isRemote,
        changeIsRemote: this.changeIsRemote,
        url: this.url,
        changeState: this.changeUIState,
        changeUrl: this.changeUrl,
        connectToPort: this._client.connectToPort,
        connectRemote: this.connectRemote,
        clearState: this.clearState,
      });
    }

    if (this.connectionState === "not_connected") {
      return Start({
        scanQrcode: this.startCamera,
        connect: this.connect,
        isMobile: this._isMobile,
        hasClickedDownload: this._hasClickedDownload,
        onDownloaded: this.onDownloaded,
        changeState: this.changeUIState,
        hosting: this.hosting,
      });
    }

    if (this.connectionState === "connected") {
      if (this.uiState === "verifycode") {
        return VerifyCode({
          code: this._code,
          changeCode: this.changeCode,
          changeState: this.changeUIState,
          verifyCode: this.verifyCode,
          isHosting: this._client.isHosting,
          verifyCodeError: this._verifyCodeError,
        });
      }

      return RequestCapability({
        changeState: this.changeUIState,
        requestCapability: this.requestCapability,
        capabilities: this.capabilities,
        appname: this.appName,
        setOpen: this.setOpen,
        appiconpath: this.appIconPath,
      });
    }

    if (this.connectionState === "disconnected") {
      return Disconnected({
        reconnect: this.connect,
      });
    }

    if (this.connectionState === "port_not_found") {
      return CouldNotMakeRequest();
    }
  }

  mobileView() {
    if (this.mobile) {
      return MobileAppLogoButton({
        openModal: () => {
          this.changeUIState("settings");
          this._isOpen = !this._isOpen;
        },
      });
    }

    return null;
  }

  render() {
    console.log(
      this.authState,
      this.connectionState,
      this.uiState,
      this._isOpen
    );
    if (this._isOpen === false) {
      if (this.authState === "authenticated") {
        return this.mobileView();
      }

      return null;
    }

    return html`
      <div class="wrapper">
        <div class="dialog">
          ${Header()}
          <main class="dialog__content">${this.renderViews()}</main>
          ${this.mobileView()}
        </div>
        <div class="ad4mConnect__backdrop" />
      </div>
    `;
  }
}

export default function Ad4mConnectUI(props: Ad4mConnectOptions) {
  // Create core instance first
  const core = new Ad4mConnect(props);
  
  // Create element and pass the core to it
  const element = new Ad4mConnectElement();
  element.core = core;

  // Set other properties from props (excluding those already in core)
  const uiProps = ['appName', 'appDesc', 'appDomain', 'appIconPath', 'capabilities', 'port', 'token', 'url', 'hosting', 'mobile', 'multiUser', 'backendUrl'];
  Object.entries(props).forEach(([key, value]) => {
    if (uiProps.includes(key)) {
      element[key] = value;
    }
  });

  // Check if running in embedded mode
  if (!isEmbedded()) {
    // Check if we have a token - if so, don't show UI initially
    const storedToken = props.token || getForVersion("ad4mtoken");
    
    if (storedToken) {
      // Has token - will auto-connect, keep UI hidden initially
      // connectedCallback will handle the connection attempt
      element.style.display = 'none';
      document.body.appendChild(element);
      
      // Listen for auth state - show UI only if auth fails
      element.addEventListener('authstatechange', (e: any) => {
        if (e.detail === 'unauthenticated') {
          element.style.display = '';
        }
      });
    } else {
      // No token - show UI immediately for user to connect
      document.body.appendChild(element);
    }
  } else {
    // Embedded mode - no UI needed, just return element with core
    console.log('[Ad4m Connect] Running in embedded mode - UI will not be shown');
    element.initializeWithoutUI();
  }

  return element;
}
