import { BarcodeDetectorPolyfill } from "@undecaf/barcode-detector-polyfill";
import { css, html, LitElement } from "lit";
import { customElement, property } from "lit/decorators.js";

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
import { ConnectState } from "./state";

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

  // New centralized state management
  private state = new ConnectState(this);

  // Core client instance
  private _client: Ad4mConnect;

  // Timeout reference for auto-submit when 6 digits are entered
  private _codeSubmitTimeout: ReturnType<typeof setTimeout> | null = null;

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
   * Initialize the Ad4mConnect client instance
   * Uses provided core instance or creates a new one from properties
   */
  private initializeClient(): void {
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
  }

  /**
   * Set up event listeners for client state changes
   */
  private setupEventListeners(): void {
    this._client.on("configstatechange", this.handleConfigChange);
    this._client.on("authstatechange", this.handleAuthChange);
    this._client.on("connectionstatechange", this.handleConnectionChange);
  }

  /**
   * Build app info object from component properties
   * Used for authentication and authorization requests
   */
  private buildAppInfo() {
    return {
      appName: this.appName,
      appDesc: this.appDesc,
      appDomain: this.appDomain,
      appUrl: window.location.origin,
      appIconPath: this.appIconPath,
    };
  }

  /**
   * Complete the authentication flow after receiving a token
   * Stores token, rebuilds client, and updates UI to authenticated state
   */
  private async completeAuthentication(token: string, url?: string): Promise<void> {
    // Store token and URL
    this._client.setToken(token);
    if (url) {
      this._client.setUrl(url);
    }

    // Rebuild client with new token to establish authenticated connection
    await this._client.buildClient();
    await this._client.checkAuth();

    // Clear form and close modal
    this.state.resetMultiUserForm();
    this.state.close();
    this.changeUIState("connected");
    this.handleAuthChange("authenticated");
  }

  /**
   * Clear any pending auto-submit timeout for verification code
   */
  private clearCodeSubmitTimeout(): void {
    if (this._codeSubmitTimeout !== null) {
      clearTimeout(this._codeSubmitTimeout);
      this._codeSubmitTimeout = null;
    }
  }

  /**
   * Initialize the Ad4mConnect client without mounting UI to DOM
   * Used for embedded mode where we don't need the visual UI
   * Returns the Ad4mConnect client instance
   */
  public initializeWithoutUI(): Ad4mConnect {
    autoBind(this);
    this.initializeClient();
    this.setupEventListeners();
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
    this.state.setMobile(detectMob());

    // Initialize the client
    this.initializeClient();

    // Don't show UI if running in embedded mode - core will handle connection
    if (isEmbedded()) {
      console.log('[Ad4m Connect UI] Running in embedded mode - UI will not be shown');
      this.state.close();
      this.setupEventListeners();
      return; // Skip rest of UI initialization
    }

    // Set up event listeners
    this.setupEventListeners();

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
      this.state.open();
    }
  }

  private async checkEmail() {
    try {
      const exist = await this._client.checkEmail(this.state.forms.email);

      if (exist) {
        this.state.updateForm('hostingStep', 1);
      } else {
        this.state.updateForm('hostingStep', 2);
      }
    } catch (e) {
      console.log(e);
    }
  }

  private async handleMultiUserEmailSubmit() {
    try {
      this.state.setLoading('multiUser', true);
      this.state.clearError('multiUser');

      // Build temporary client to call requestLoginVerification
      const tempClient = this._client.buildTempClient(this.backendUrl);

      const result = await tempClient.agent.requestLoginVerification(this.state.forms.multiUserEmail, this.buildAppInfo());

      if (result.success && !result.requiresPassword) {
        // User exists, verification email sent
        this.state.updateForm('multiUserStep', 'code');
        this.state.updateForm('multiUserVerificationType', 'login');
      } else if (result.requiresPassword) {
        // Determine if this is login or signup based on whether user exists
        // If isExistingUser is true, show password for login; otherwise for signup
        this.state.updateForm('multiUserStep', 'password');
        this.state.updateForm('multiUserVerificationType', result.isExistingUser ? 'login' : 'signup');
      } else {
        this.state.setError('multiUser', result.message || "Failed to send verification email");
      }
    } catch (e) {
      this.state.setError('multiUser', e.message || "Failed to process email. Please try again.");
    } finally {
      this.state.setLoading('multiUser', false);
    }
  }

  private async handleMultiUserPasswordSubmit() {
    try {
      this.state.setLoading('multiUser', true);
      this.state.clearError('multiUser');

      // Build temporary client
      const tempClient = this._client.buildTempClient(this.backendUrl);

      if (this.state.forms.multiUserVerificationType === "login") {
        // Existing user - try password login
        try {
          const token = await tempClient.agent.loginUser(this.state.forms.multiUserEmail, this.state.forms.multiUserPassword);
          await this.completeAuthentication(token, this.backendUrl);
        } catch (loginError) {
          this.state.setError('multiUser', loginError.message || "Invalid email or password. Please try again.");
        }
      } else {
        // New user - create account
        const result = await tempClient.agent.createUser(this.state.forms.multiUserEmail, this.state.forms.multiUserPassword, this.buildAppInfo());

        if (result.success) {
          // Check if email verification was sent or if we should proceed directly to login
          if (result.error && result.error.includes("SMTP is not configured")) {
            // SMTP not configured - login immediately with password
            try {
              const token = await tempClient.agent.loginUser(this.state.forms.multiUserEmail, this.state.forms.multiUserPassword);
              await this.completeAuthentication(token, this.backendUrl);
            } catch (loginError) {
              this.state.setError('multiUser', loginError.message || "Account created but login failed. Please try logging in.");
            }
          } else if (!result.error) {
            // User created, verification email sent
            this.state.updateForm('multiUserStep', 'code');
            this.state.updateForm('multiUserVerificationType', 'signup');
          } else {
            // User created but email failed - still allow login
            this.state.setError('multiUser', result.error + " You can try logging in with your password.");
          }
        } else {
          this.state.setError('multiUser', result.error || "Failed to create account. Please try again.");
        }
      }
    } catch (e) {
      this.state.setError('multiUser', e.message || "Failed to process request. Please try again.");
    } finally {
      this.state.setLoading('multiUser', false);
    }
  }

  private async handleMultiUserCodeSubmit() {
    // Guard against duplicate calls - if already loading, ignore this request
    if (this.state.loading.multiUser) {
      return;
    }

    // Cancel any pending auto-submit timeout since we're submitting now
    this.clearCodeSubmitTimeout();

    try {
      this.state.setLoading('multiUser', true);
      this.state.clearError('multiUser');

      // Build temporary client to call verifyEmailCode
      const tempClient = this._client.buildTempClient(this.backendUrl);
      const token = await tempClient.agent.verifyEmailCode(
        this.state.forms.multiUserEmail,
        this.state.forms.multiUserVerificationCode,
        this.state.forms.multiUserVerificationType
      );

      await this.completeAuthentication(token, this.backendUrl);
    } catch (e) {
      this.state.setError('multiUser', "Invalid or expired code. Please try again.");
      this.state.updateForm('multiUserVerificationCode', '');
    } finally {
      this.state.setLoading('multiUser', false);
    }
  }

  private handleMultiUserBackToEmail() {
    // Cancel any pending auto-submit timeout
    this.clearCodeSubmitTimeout();
    this.state.updateForm('multiUserStep', 'email');
    this.state.updateForm('multiUserPassword', '');
    this.state.updateForm('multiUserVerificationCode', '');
    this.state.clearError('multiUser');
  }

  private changeMultiUserEmail(email: string) {
    this.state.updateForm('multiUserEmail', email);
    this.state.clearError('multiUser'); // Clear error when user types
  }

  private changeMultiUserPassword(password: string) {
    this.state.updateForm('multiUserPassword', password);
    this.state.clearError('multiUser'); // Clear error when user types
  }

  private changeMultiUserVerificationCode(code: string) {
    // Cancel any pending auto-submit timeout
    this.clearCodeSubmitTimeout();

    this.state.updateForm('multiUserVerificationCode', code);
    this.state.clearError('multiUser'); // Clear error when user types

    // Auto-submit when 6 digits entered (with debounce to prevent race conditions)
    if (code.length === 6 && !this.state.loading.multiUser) {
      this._codeSubmitTimeout = setTimeout(() => {
        this._codeSubmitTimeout = null;
        // Only submit if still 6 digits and not already loading
        if (this.state.forms.multiUserVerificationCode.length === 6 && !this.state.loading.multiUser) {
          this.handleMultiUserCodeSubmit();
        }
      }, 100);
    }
  }

  private async detectLocal() {
    try {
      await connectWebSocket(`ws://localhost:${this.port}/graphql`, 3000);
      this.state.setLocalDetected(true);
    } catch (error) {
      console.log("[Ad4m Connect] Local detection failed:", error);
      this.state.setLocalDetected(false);
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
    this.state.updateForm('remoteUrl', this.backendUrl || this.url || '');
    this.state.setRemoteMultiUserDetected(null);
    this.state.clearError('remote');
    this.changeUIState("remote_connection");
  }

  private handleRemoteUrlChange(url: string) {
    this.state.updateForm('remoteUrl', url);
    this.state.clearError('remote');
  }

  private async handleRemoteConnect() {
    if (!this.state.forms.remoteUrl) {
      this.state.setError('remote', "Please enter a URL");
      return;
    }

    this.state.setLoading('remoteDetecting', true);
    this.state.clearError('remote');
    this.state.setRemoteMultiUserDetected(null); // Reset detection state

    try {
      // Step 1: Check if the server is reachable at all
      console.log("[Ad4m Connect] Checking if server is reachable:", this.state.forms.remoteUrl);
      await connectWebSocket(this.state.forms.remoteUrl, 5000);
      console.log("[Ad4m Connect] Server is reachable");

      // Step 2: Verify it's actually an AD4M API
      await this.verifyAd4mApi(this.state.forms.remoteUrl);

      // Step 3: Detect if multi-user is enabled
      const isMultiUser = await this.detectRemoteMultiUser(this.state.forms.remoteUrl);
      this.state.setRemoteMultiUserDetected(isMultiUser);
      this.state.setLoading('remoteDetecting', false);
    } catch (error) {
      console.error("[Ad4m Connect] Connection/detection failed:", error);
      this.state.setError('remote', error.message || "Cannot reach server at this URL. Please check the URL and try again.");
      this.state.setRemoteMultiUserDetected(null); // Keep as null to not show connection options
      this.state.setLoading('remoteDetecting', false);
    }
  }

  private async handleRemoteMultiUserAuth() {
    // Set the backend URL and show multi-user auth
    this.backendUrl = this.state.forms.remoteUrl;
    this.changeUIState("multiuser_auth");
  }

  private async handleRemoteRequestCapability() {
    try {
      this.changeUIState("requestcap");
      await this._client.connect(this.state.forms.remoteUrl);
    } catch (error) {
      this.state.setError('remote', error.message || "Failed to connect");
      this.changeUIState("remote_connection");
    }
  }

  private async loginToHosting() {
    try {
      await this._client.loginToHosting(this.state.forms.email, this.state.forms.password);

      this.changeUIState("connected");
    } catch (e) {
      if (e.message.includes("Passwords did not match")) {
        this.state.setError('password', "Passwords did not match");
      } else {
        this.state.setError('hostingNotRunning', "Hosting is not running");
      }
    }
  }

  private changeEmail(email: string) {
    this.state.updateForm('email', email);
  }

  private changePassword(password: string) {
    this.state.updateForm('password', password);
  }

  private async unlockAgent(passcode, holochain = true) {
    await this._client.ad4mClient.agent.unlock(passcode, holochain);
  }

  private async verifyCode(code) {
    try {
      await this._client.verifyCode(code);
    } catch (e) {
      this.state.setError('verifyCode', "Invalid code");
    }
  }

  private changeCode(code) {
    this.state.updateForm('code', code);
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
    this.state.setView(state);
  }

  private setIsHostingRunning(val) {
    this.state.setError('hostingNotRunning', val);
  }

  private changeIsRemote(bol: boolean) {
    this.state.setRemote(bol);
  }

  private onDownloaded() {
    this.state.setHasClickedDownload(true);
  }

  private handleAuthChange(event: AuthStates) {
    const customEvent = new CustomEvent("authstatechange", {
      detail: event,
    });
    if (event === "locked") {
      this.state.open();
    }
    if (event === "authenticated") {
      this.state.close();
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
      this.state.close(); // Don't show dialog while checking
      return;
    }

    if (event === "connected") {
      if (this.authState !== "authenticated") {
        this.changeUIState("requestcap");
        this.state.open();
      } else {
        this.changeUIState("connected");
        this.state.close();
      }
    }
    if (event === "disconnected") {
      this.state.open();
    }
    if (event === "not_connected") {
      this.state.open();
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
    this.state.open();
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
      this.state.open();
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
    if (val) {
      this.state.open();
    } else {
      this.state.close();
    }
  }

  setHostingStep(step: number) {
    this.state.updateForm('hostingStep', step);
  }

  clearState() {
    this.handleConfigChange("port", null);
    this.handleConfigChange("url", null);
    this.handleConfigChange("token", null);
    // this.state.close();
    this.handleConnectionChange("not_connected");
    this.handleAuthChange("unauthenticated");
    this.changeUIState("start");

    this._client.clearState();
  }

  renderViews() {
    if (this.connectionState === "connecting") {
      return Loading();
    }

    if (this.state.currentView === "connection_overview") {
      return ConnectionOverview({
        localDetected: this.state.detection.localDetected,
        multiUserConfigured: this.multiUser && !!this.backendUrl,
        backendUrl: this.backendUrl,
        configuredUrl: this.url,
        isMobile: this.state.isMobile,
        onConnectLocal: this.handleConnectLocal,
        onConnectRemote: this.handleShowRemoteConnection,
        onScanQR: () => { this.startCamera(null); },
        onDownloadAd4m: () => { window.open("https://github.com/coasys/ad4m/releases"); },
      });
    }

    if (this.state.currentView === "remote_connection") {
      return RemoteConnection({
        initialUrl: this.state.forms.remoteUrl,
        detecting: this.state.loading.remoteDetecting,
        multiUserDetected: this.state.detection.remoteMultiUserDetected,
        error: this.state.errors.remote,
        onBack: () => this.changeUIState("connection_overview"),
        onUrlChange: this.handleRemoteUrlChange,
        onConnect: this.handleRemoteConnect,
        onMultiUserAuth: this.handleRemoteMultiUserAuth,
        onRequestCapability: this.handleRemoteRequestCapability,
      });
    }

    if (this.state.currentView === "multiuser_auth") {
      return MultiUserAuth({
        email: this.state.forms.multiUserEmail,
        password: this.state.forms.multiUserPassword,
        verificationCode: this.state.forms.multiUserVerificationCode,
        error: this.state.errors.multiUser,
        isLoading: this.state.loading.multiUser,
        backendUrl: this.backendUrl,
        step: this.state.forms.multiUserStep,
        verificationType: this.state.forms.multiUserVerificationType,
        changeEmail: this.changeMultiUserEmail,
        changePassword: this.changeMultiUserPassword,
        changeVerificationCode: this.changeMultiUserVerificationCode,
        onEmailSubmit: this.handleMultiUserEmailSubmit,
        onPasswordSubmit: this.handleMultiUserPasswordSubmit,
        onCodeSubmit: this.handleMultiUserCodeSubmit,
        onBackToEmail: this.handleMultiUserBackToEmail,
      });
    }

    if (this.state.currentView === "hosting") {
      return Hosting({
        email: this.state.forms.email,
        password: this.state.forms.password,
        changeEmail: this.changeEmail,
        changePassword: this.changePassword,
        changeState: this.changeUIState,
        step: this.state.forms.hostingStep,
        setHostingStep: this.setHostingStep,
        login: this.loginToHosting,
        checkEmail: this.checkEmail,
        passwordError: this.state.errors.password,
        isHostingRunning: this.state.errors.hostingNotRunning,
        setIsHostingRunning: this.setIsHostingRunning,
      });
    }

    if (this.state.currentView === "qr") {
      return ScanQRCode({
        changeState: this.changeUIState,
        onSuccess: (url) => {
          this.changeUrl(url);
          this._client.connect(url);
        },
        uiState: this.state.currentView,
      });
    }

    if (this.authState === "locked") {
      return AgentLocked({
        unlockAgent: this.unlockAgent,
        reconnect: this.connect,
      });
    }

    if (this.state.currentView === "settings") {
      return Settings({
        port: this.port,
        changePort: this.changePort,
        isRemote: this.state.isRemote,
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
        isMobile: this.state.isMobile,
        hasClickedDownload: this.state.hasClickedDownload,
        onDownloaded: this.onDownloaded,
        changeState: this.changeUIState,
        hosting: this.hosting,
      });
    }

    if (this.connectionState === "connected") {
      if (this.state.currentView === "verifycode") {
        return VerifyCode({
          code: this.state.forms.code,
          changeCode: this.changeCode,
          changeState: this.changeUIState,
          verifyCode: this.verifyCode,
          isHosting: this._client.isHosting,
          verifyCodeError: this.state.errors.verifyCode,
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
          this.state.toggleOpen();
        },
      });
    }

    return null;
  }

  render() {
    console.log(
      this.authState,
      this.connectionState,
      this.state.currentView,
      this.state.isOpen
    );
    if (!this.state.isOpen) {
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
