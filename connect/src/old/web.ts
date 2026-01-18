// import { BarcodeDetectorPolyfill } from "@undecaf/barcode-detector-polyfill";
// import { css, html, LitElement } from "lit";
// import { customElement, property, state } from "lit/decorators.js";

// import Ad4mConnect from "./core";

// import autoBind from "auto-bind";

// import "./components/views/ConnectionOptions";
// import "./components/views/LocalAuthentication";
// // import "./components/views/RemoteConnection";
// // import "./components/views/ScanQRCode";
// // import "./components/views/RequestCapability";
// // import "./components/views/Settings";
// // import "./components/views/Start";
// // import "./components/views/Hosting";
// // import "./components/views/MultiUserAuth";
// import Ad4mLogo from "./components/icons/Ad4mLogo";

// // import VerifyCode from "./old/components/VerifyCode";
// // import { connectWebSocket, DEFAULT_PORT, getForVersion, removeForVersion, setForVersion, isEmbedded } from "./old/utils";
// // import { ConnectState } from "./state";

// type Views = 'connection-options' | 'local-authentication' | 'remote-authentication';

// // TODO: update text color vars when decided on

// const styles = css`
//   @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;700&display=swap');

//   :host {
//     --ac-primary-color: #91e3fd;
//     --ac-primary-color-light: #acebff;
//     --ac-success-color: #5dd27d;
//     --ac-danger-color: #f4367f;
//     --ac-text-color: #fff;
//     --ac-background-color: #191c3fe0;
//     --ac-border-color-dark: #91d4fd2b;
//     --ac-border-color-light: #91d4fd69;
//   }

//   * {
//     box-sizing: border-box;
//   }

//   .wrapper {
//     position: fixed;
//     display: grid;
//     place-content: center;
//     top: 0;
//     left: 0;
//     height: 100vh;
//     width: 100vw;
//     z-index: 100;
//   }

//   .backdrop {
//     position: absolute;
//     top: 0;
//     left: 0;
//     height: 100vh;
//     width: 100vw;
//     background-color: rgba(0, 0, 0, 0.5);
//   }

//   .modal {
//     z-index: 10;
//     background-color: var(--ac-background-color);
//     border: 1px solid var(--ac-border-color-dark);
//     border-radius: 12px;
//     padding: 30px;
//     width: calc(100vw - 10px);
//     max-width: 480px;
//     max-height: 90vh;
//     overflow-y: auto;
//     backdrop-filter: blur(10px);
//     -webkit-backdrop-filter: blur(10px); /* Safari */
//   }

//   .modal-header {
//     display: flex;
//     align-items: center;
//     justify-content: center;
//     color: var(--ac-primary-color);
//     margin: 10px 0 30px 0;
//   }

//   .modal-header > svg {
//     width: 70px;
//     height: 70px;
//   }

//   .modal-content {
//     display: flex;
//     flex-direction: column;
//     justify-content: center;
//   }

//   .settings-button {
//     position: absolute;
//     bottom: 20px;
//     right: 20px;
//     color: var(--ac-primary-color);
//   }
// `;

//   // .spinner {
//   //   display: inline-block;
//   //   width: 48px;
//   //   height: 48px;
//   //   border: 4px solid rgba(145, 227, 253, 0.2);
//   //   border-top-color: var(--ac-primary-color);
//   //   border-radius: 50%;
//   //   animation: spin 1s linear infinite;
//   // }

//   // @keyframes spin {
//   //   to {
//   //     transform: rotate(360deg);
//   //   }
//   // }

//   // .dialog__connect {
//   //   display: flex;
//   //   justify-content: center;
//   //   align-items: center;
//   //   flex-gap: 50px;
//   //   gap: 50px;
//   //   position: relative;
//   // }

//   // .dialog__logo svg {
//   //   width: 100%;
//   // }

//   // .dialog__connect-ad4m {
//   //   width: 80px;
//   //   height: 80px;
//   //   background: var(--ac-modal-background-color);
//   //   padding: 20px;
//   //   box-shadow: 0px 4px 7px 0px rgb(0 0 0 / 8%);
//   //   border-radius: 50%;
//   //   position: relative;
//   // }

//   // .dialog__connect-ad4m:before {
//   //   content: "";
//   //   position: absolute;
//   //   top: 0;
//   //   right: 0;
//   //   bottom: 0;
//   //   left: 0;
//   //   z-index: -1;
//   //   margin: -2px;
//   //   border-radius: inherit;
//   //   background: var(--gradient);
//   // }

//   // .dialog__connect-app {
//   //   width: 80px;
//   //   height: 80px;
//   // }

//   // .dialog__connect-check:before {
//   //   content: "";
//   //   display: block;
//   //   width: 40px;
//   //   border-bottom: 2px dashed var(--body-color);
//   //   position: absolute;
//   //   left: 50%;
//   //   top: 50%;
//   //   transform: translateY(-50%) translateX(-50%);
//   // }

//   // .dialog__connect-check svg {
//   //   position: relative;
//   // }

//   // .text-center {
//   //   text-align: center;
//   // }

//   // .uppercase {
//   //   text-transform: uppercase;
//   // }

//   // .input {
//   //   display: flex;
//   //   flex-direction: column;
//   //   flex-gap: 10px;
//   //   gap: 10px;
//   // }

//   // .input__label {
//   //   font-size: 12px;
//   // }

//   // .input__field {
//   //   border-radius: 8px;
//   //   outline: 0;
//   //   height: 60px;
//   //   color: var(--heading-color);
//   //   background-color: var(--ac-modal-background-color);
//   //   padding: 0px 30px;
//   //   font-size: 20px;
//   //   border: 1px solid var(--body-color);
//   // }

//   // .input__field:focus {
//   //   border: 1px solid var(--ac-primary-color);
//   //   box-shadow: 0px 0px 0px 1px var(--ac-primary-color);
//   // }

//   // .ad4mConnect__backdrop {
//   //   position: absolute;
//   //   top: 0;
//   //   left: 0;
//   //   height: 100vh;
//   //   width: 100vw;
//   //   background-color: rgba(0, 0, 0, 0.5);
//   // }

//   // .ad4mConnect__locked {
//   //   position: fixed;
//   //   top: 0;
//   //   left: 0;
//   //   background: var(--ac-modal-background-color);
//   //   height: 100vh;
//   //   width: 100vw;
//   //   padding: 36px;
//   //   display: flex;
//   //   align-items: center;
//   //   flex-direction: column;
//   //   font-family: "Comfortaa", cursive;
//   // }

//   // .disconnected {
//   //   position: fixed;
//   //   top: 0;
//   //   left: 0;
//   //   width: 100vw;
//   //   padding: 10px 0;
//   //   text-align: center;
//   //   background: red;
//   // }

//   // .qr-scanner {
//   //   background: black;
//   //   position: fixed;
//   //   left: 0;
//   //   top: 0;
//   //   width: 100%;
//   //   height: 100%;
//   //   display: grid;
//   //   grid-template-columns: 1fr;
//   //   place-content: center;
//   // }

//   // .qr-scanner .stop {
//   //   position: absolute;
//   //   z-index: 10;
//   //   left: 50%;
//   //   bottom: 10px;
//   //   transform: translateX(-50%);
//   // }

//   // .qr-scanner video {
//   //   height: 100vh;
//   //   width: 100vw;
//   //   object-fit: cover;
//   // }

// @customElement("ad4m-connect")
// export class Ad4mConnectElement extends LitElement {
//   static styles = [styles];

//   @state() private currentView: Views = "connection-options";
//   @state() private modalOpen = false;
//   @state() private verificationError = false;

//   // @property({ type: String, reflect: true }) appName = null;
//   // @property({ type: String, reflect: true }) appDesc = null;
//   // @property({ type: String, reflect: true }) appDomain = null;
//   // @property({ type: String, reflect: true }) appIconPath = null;
//   // @property({ type: String, reflect: true }) capabilities = [];
//   // @property({ type: Boolean, reflect: true }) hosting = getForVersion("ad4mhosting") === "true" || false;
//   // @property({ type: Number, reflect: true }) port = parseInt(getForVersion("ad4mport")) || DEFAULT_PORT;
//   // @property({ type: String, reflect: true }) remoteUrl = "";
//   // @property({ type: String }) token = getForVersion("ad4mToken") || "";

//   // @state() private modalOpen = false;
//   // @state() private nodeUrl = "";
//   // @state() private isMobile = false;
//   // @state() private currentView: Views = "connection-options";
//   // @state() private authState: AuthStates = "unauthenticated";
//   // @state() private connectionState: ConnectionStates = "disconnected";

//   core: Ad4mConnect;

//   // Timeout reference for auto-submit when 6 digits are entered
//   private _codeSubmitTimeout: ReturnType<typeof setTimeout> | null = null;

//   // get authState(): AuthStates {
//   //   return this.client.authState;
//   // }

//   // get connectionState(): ConnectionStates {
//   //   return this.client.connectionState;
//   // }

//   // private initializeCore(): Ad4mConnect {
//   //   console.log('[Ad4m Connect UI] Initializing Ad4mConnect core');
//   //   return new Ad4mConnect({
//   //     appName: this.appName,
//   //     appDesc: this.appDesc,
//   //     appDomain: this.appDomain,
//   //     appUrl: window.location.origin,
//   //     appIconPath: this.appIconPath,
//   //     capabilities: Array.isArray(this.capabilities)
//   //       ? this.capabilities
//   //       : JSON.parse(this.capabilities),
//   //     port: this.port || parseInt(getForVersion("ad4mport")) || DEFAULT_PORT,
//   //     token: this.token || getForVersion("ad4mtoken"),
//   //     url: this.nodeUrl || getForVersion("ad4murl"),
//   //     hosting: this.hosting,
//   //   });
//   // }

//   // /**
//   //  * Set up event listeners for client state changes
//   //  */
//   // private setupEventListeners(): void {
//   //   this.client.on("configstatechange", this.handleConfigChange);
//   //   this.client.on("authstatechange", this.handleAuthChange);
//   //   this.client.on("connectionstatechange", this.handleConnectionChange);
//   // }

//   /**
//    * Build app info object from component properties
//    * Used for authentication and authorization requests
//    */
//   // private buildAppInfo() {
//   //   return {
//   //     appName: this.appName,
//   //     appDesc: this.appDesc,
//   //     appDomain: this.appDomain,
//   //     appUrl: window.location.origin,
//   //     appIconPath: this.appIconPath,
//   //   };
//   // }

//   /**
//    * Complete the authentication flow after receiving a token
//    * Stores token, rebuilds client, and updates UI to authenticated state
//    */
//   // private async completeAuthentication(token: string, url?: string): Promise<void> {
//   //   // Store token and URL
//   //   this.client.setToken(token);
//   //   if (url) {
//   //     this.client.setUrl(url);
//   //   }

//   //   // Rebuild client with new token to establish authenticated connection
//   //   await this.client.buildClient();
//   //   await this.client.checkAuth();

//   //   // Clear form and close modal
//   //   // this.state.resetMultiUserForm();
//   //   // this.state.close();
//   //   this.modalOpen = false;
//   //   this.changeUIState("connected");
//   //   this.handleAuthChange("authenticated");
//   // }

//   /**
//    * Clear any pending auto-submit timeout for verification code
//    */
//   // private clearCodeSubmitTimeout(): void {
//   //   if (this._codeSubmitTimeout !== null) {
//   //     clearTimeout(this._codeSubmitTimeout);
//   //     this._codeSubmitTimeout = null;
//   //   }
//   // }

//   /**
//    * Initialize the Ad4mConnect client without mounting UI to DOM
//    * Used for embedded mode where we don't need the visual UI
//    * Returns the Ad4mConnect client instance
//    */
//   // public initializeWithoutUI(): Ad4mConnect {
//   //   autoBind(this);
//   //   this.core = this.initializeCore();
//   //   return this.core;
//   // }

//   // getClient() {
//   //   return this.core.ad4mClient;
//   // }

//   // getCore() {
//   //   return this.core;
//   // }

//   // private injectFont() {
//   //   const link = document.createElement("link");
//   //   link.setAttribute("rel", "stylesheet");
//   //   link.setAttribute(
//   //     "href",
//   //     `https://fonts.googleapis.com/css2?family=Bricolage+Grotesque:opsz,wght@12..96,200..800&display=swap`
//   //   );
//   //   document.head.appendChild(link);
//   // }

//   // private loadFont() {
//   //   const link = document.createElement("link");
//   //   link.rel = "stylesheet";
//   //   link.type = "text/css";
//   //   link.crossOrigin = "anonymous";
//   //   link.href =
//   //     "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;700&display=swap";
//   //   document.head.appendChild(link);
//   // }

//   private detectMobile() {
//     const toMatch = [
//       /Android/i,
//       /webOS/i,
//       /iPhone/i,
//       /iPad/i,
//       /iPod/i,
//       /BlackBerry/i,
//       /Windows Phone/i,
//     ];

//     return toMatch.some((toMatchItem) => navigator.userAgent.match(toMatchItem));
//   }

//   connectedCallback() {
//     super.connectedCallback();
//     autoBind(this); // needed if 'this' used in callbacks 

//     console.log('[Ad4m Connect UI] Initializing Ad4m Connect UI component', this.core);

//     if (this.core.token) {
//       this.core.connect()
//     } else {
//       this.currentView = "connection-options";
//       this.modalOpen = true;
//     }

//     // // Don't show UI if running in embedded mode - core will handle connection
//     // if (isEmbedded()) {
//     //   console.log('[Ad4m Connect UI] Running in embedded mode - UI will not be shown');
//     //   return; // Skip rest of UI initialization
//     // }

//     // Set up event listeners for core state changes
//     // this.core.addEventListener('authstatechange', this.handleAuthChange);
//     // this.core.addEventListener('connectionstatechange', this.handleConnectionChange);

//     // const storedToken = this.core.token;

//     // if (storedToken) {
//     //   // If we have a stored token, try to auto-connect without showing UI
//     //   // UI will only show if authentication fails
//     //   // First check if there's a stored URL (from previous connection)
//     //   const storedUrl = getForVersion("ad4murl");
      
//     //   if (storedUrl) {
//     //     // Use the stored URL (preserves local vs remote choice)
//     //     this.core.connect(storedUrl);
//     //   } else {
//     //     // No stored URL - this means first time or cleared storage
//     //     // Default to local connection (user can choose remote from UI if needed)
//     //     this.core.connect();
//     //   }
//     // } else {
//     //   // No stored token - show connection options
//     //   console.log('[Ad4m Connect UI] No stored token - showing connection options');
//     //   this.currentView = "connection-options";
//     //   // this.modalOpen = true;
//     // }
//   }

//   // private async checkEmail() {
//   //   try {
//   //     const exist = await this.client.checkEmail(this.state.forms.email);

//   //     if (exist) {
//   //       this.state.updateForm('hostingStep', 1);
//   //     } else {
//   //       this.state.updateForm('hostingStep', 2);
//   //     }
//   //   } catch (e) {
//   //     console.log(e);
//   //   }
//   // }

//   // private async handleMultiUserEmailSubmit() {
//   //   try {
//   //     this.state.setLoading('multiUser', true);
//   //     this.state.clearError('multiUser');

//   //     // Build temporary client to call requestLoginVerification
//   //     const tempClient = this.client.buildTempClient(this.remoteUrl);

//   //     const result = await tempClient.agent.requestLoginVerification(this.state.forms.multiUserEmail, this.buildAppInfo());

//   //     if (result.success && !result.requiresPassword) {
//   //       // User exists, verification email sent
//   //       this.state.updateForm('multiUserStep', 'code');
//   //       this.state.updateForm('multiUserVerificationType', 'login');
//   //     } else if (result.requiresPassword) {
//   //       // Determine if this is login or signup based on whether user exists
//   //       // If isExistingUser is true, show password for login; otherwise for signup
//   //       this.state.updateForm('multiUserStep', 'password');
//   //       this.state.updateForm('multiUserVerificationType', result.isExistingUser ? 'login' : 'signup');
//   //     } else {
//   //       this.state.setError('multiUser', result.message || "Failed to send verification email");
//   //     }
//   //   } catch (e) {
//   //     this.state.setError('multiUser', e.message || "Failed to process email. Please try again.");
//   //   } finally {
//   //     this.state.setLoading('multiUser', false);
//   //   }
//   // }

//   // private async handleMultiUserPasswordSubmit() {
//   //   try {
//   //     this.state.setLoading('multiUser', true);
//   //     this.state.clearError('multiUser');

//   //     // Build temporary client
//   //     const tempClient = this.client.buildTempClient(this.remoteUrl);

//   //     if (this.state.forms.multiUserVerificationType === "login") {
//   //       // Existing user - try password login
//   //       try {
//   //         const token = await tempClient.agent.loginUser(this.state.forms.multiUserEmail, this.state.forms.multiUserPassword);
//   //         await this.completeAuthentication(token, this.remoteUrl);
//   //       } catch (loginError) {
//   //         this.state.setError('multiUser', loginError.message || "Invalid email or password. Please try again.");
//   //       }
//   //     } else {
//   //       // New user - create account
//   //       const result = await tempClient.agent.createUser(this.state.forms.multiUserEmail, this.state.forms.multiUserPassword, this.buildAppInfo());

//   //       if (result.success) {
//   //         // Check if email verification was sent or if we should proceed directly to login
//   //         if (result.error && result.error.includes("SMTP is not configured")) {
//   //           // SMTP not configured - login immediately with password
//   //           try {
//   //             const token = await tempClient.agent.loginUser(this.state.forms.multiUserEmail, this.state.forms.multiUserPassword);
//   //             await this.completeAuthentication(token, this.remoteUrl);
//   //           } catch (loginError) {
//   //             this.state.setError('multiUser', loginError.message || "Account created but login failed. Please try logging in.");
//   //           }
//   //         } else if (!result.error) {
//   //           // User created, verification email sent
//   //           this.state.updateForm('multiUserStep', 'code');
//   //           this.state.updateForm('multiUserVerificationType', 'signup');
//   //         } else {
//   //           // User created but email failed - still allow login
//   //           this.state.setError('multiUser', result.error + " You can try logging in with your password.");
//   //         }
//   //       } else {
//   //         this.state.setError('multiUser', result.error || "Failed to create account. Please try again.");
//   //       }
//   //     }
//   //   } catch (e) {
//   //     this.state.setError('multiUser', e.message || "Failed to process request. Please try again.");
//   //   } finally {
//   //     this.state.setLoading('multiUser', false);
//   //   }
//   // }

//   // private async handleMultiUserCodeSubmit() {
//   //   // Guard against duplicate calls - if already loading, ignore this request
//   //   if (this.state.loading.multiUser) {
//   //     return;
//   //   }

//   //   // Cancel any pending auto-submit timeout since we're submitting now
//   //   this.clearCodeSubmitTimeout();

//   //   try {
//   //     this.state.setLoading('multiUser', true);
//   //     this.state.clearError('multiUser');

//   //     // Build temporary client to call verifyEmailCode
//   //     const tempClient = this.client.buildTempClient(this.remoteUrl);
//   //     const token = await tempClient.agent.verifyEmailCode(
//   //       this.state.forms.multiUserEmail,
//   //       this.state.forms.multiUserVerificationCode,
//   //       this.state.forms.multiUserVerificationType
//   //     );

//   //     await this.completeAuthentication(token, this.remoteUrl);
//   //   } catch (e) {
//   //     this.state.setError('multiUser', "Invalid or expired code. Please try again.");
//   //     this.state.updateForm('multiUserVerificationCode', '');
//   //   } finally {
//   //     this.state.setLoading('multiUser', false);
//   //   }
//   // }

//   // private handleMultiUserBackToEmail() {
//   //   // Cancel any pending auto-submit timeout
//   //   this.clearCodeSubmitTimeout();
//   //   this.state.updateForm('multiUserStep', 'email');
//   //   this.state.updateForm('multiUserPassword', '');
//   //   this.state.updateForm('multiUserVerificationCode', '');
//   //   this.state.clearError('multiUser');
//   // }

//   // private changeMultiUserEmail(email: string) {
//   //   this.state.updateForm('multiUserEmail', email);
//   //   this.state.clearError('multiUser'); // Clear error when user types
//   // }

//   // private changeMultiUserPassword(password: string) {
//   //   this.state.updateForm('multiUserPassword', password);
//   //   this.state.clearError('multiUser'); // Clear error when user types
//   // }

//   // private changeMultiUserVerificationCode(code: string) {
//   //   // Cancel any pending auto-submit timeout
//   //   this.clearCodeSubmitTimeout();

//   //   this.state.updateForm('multiUserVerificationCode', code);
//   //   this.state.clearError('multiUser'); // Clear error when user types

//   //   // Auto-submit when 6 digits entered (with debounce to prevent race conditions)
//   //   if (code.length === 6 && !this.state.loading.multiUser) {
//   //     this._codeSubmitTimeout = setTimeout(() => {
//   //       this._codeSubmitTimeout = null;
//   //       // Only submit if still 6 digits and not already loading
//   //       if (this.state.forms.multiUserVerificationCode.length === 6 && !this.state.loading.multiUser) {
//   //         this.handleMultiUserCodeSubmit();
//   //       }
//   //     }, 100);
//   //   }
//   // }

//   // private async detectLocal() {
//   //   try {
//   //     await connectWebSocket(`ws://localhost:${this.port}/graphql`, 3000);
//   //     this.state.setLocalDetected(true);
//   //   } catch (error) {
//   //     console.log("[Ad4m Connect] Local detection failed:", error);
//   //     this.state.setLocalDetected(false);
//   //   }
//   // }

//   // private async verifyAd4mApi(url: string): Promise<void> {
//   //   console.log("[Ad4m Connect] Verifying AD4M API at URL:", url);
//   //   const tempClient = this.client.buildTempClient(url);

//   //   try {
//   //     await tempClient.runtime.info();
//   //     console.log("[Ad4m Connect] AD4M API verified");
//   //   } catch (error) {
//   //     console.error("[Ad4m Connect] Failed to verify AD4M API:", error);
//   //     throw new Error("Server is reachable but doesn't appear to be an AD4M executor. Make sure the URL includes '/graphql'.");
//   //   }
//   // }

//   // private async detectRemoteMultiUser(url: string): Promise<boolean> {
//   //   console.log("[Ad4m Connect] Detecting multi-user mode for URL:", url);
//   //   const tempClient = this.client.buildTempClient(url);

//   //   try {
//   //     const multiUserEnabled = await tempClient.runtime.multiUserEnabled();
//   //     console.log("[Ad4m Connect] Multi-user detection result:", multiUserEnabled);
//   //     return multiUserEnabled;
//   //   } catch (error) {
//   //     console.error("[Ad4m Connect] Failed to detect multi-user mode:", error);
//   //     console.error("[Ad4m Connect] Error details:", error.message, error.stack);
//   //     // If multi-user query fails, assume single-user mode
//   //     return false;
//   //   }
//   // }

//   // private async handleConnectLocal() {
//   //   try {
//   //     this.currentView = "request_capability";
//   //     await this.client.connect();
//   //   } catch (error) {
//   //     console.error("Failed to connect to local AD4M:", error);
//   //     this.currentView = "connection-options";
//   //   }
//   // }

//   // private handleShowRemoteConnection() {
//   //   // Use configured URL in priority order: backendUrl, url, or empty
//   //   this.state.updateForm('remoteUrl', this.remoteUrl || '');
//   //   this.state.setRemoteMultiUserDetected(null);
//   //   this.state.clearError('remote');
//   //   this.changeUIState("remote_connection");
//   // }

//   // private handleRemoteUrlChange(url: string) {
//   //   this.state.updateForm('remoteUrl', url);
//   //   this.state.clearError('remote');
//   // }

//   // private async handleRemoteConnect() {
//   //   if (!this.state.forms.remoteUrl) {
//   //     this.state.setError('remote', "Please enter a URL");
//   //     return;
//   //   }

//   //   this.state.setLoading('remoteDetecting', true);
//   //   this.state.clearError('remote');
//   //   this.state.setRemoteMultiUserDetected(null); // Reset detection state

//   //   try {
//   //     // Step 1: Check if the server is reachable at all
//   //     console.log("[Ad4m Connect] Checking if server is reachable:", this.state.forms.remoteUrl);
//   //     await connectWebSocket(this.state.forms.remoteUrl, 5000);
//   //     console.log("[Ad4m Connect] Server is reachable");

//   //     // Step 2: Verify it's actually an AD4M API
//   //     await this.verifyAd4mApi(this.state.forms.remoteUrl);

//   //     // Step 3: Detect if multi-user is enabled
//   //     const isMultiUser = await this.detectRemoteMultiUser(this.state.forms.remoteUrl);
//   //     this.state.setRemoteMultiUserDetected(isMultiUser);
//   //     this.state.setLoading('remoteDetecting', false);
//   //   } catch (error) {
//   //     console.error("[Ad4m Connect] Connection/detection failed:", error);
//   //     this.state.setError('remote', error.message || "Cannot reach server at this URL. Please check the URL and try again.");
//   //     this.state.setRemoteMultiUserDetected(null); // Keep as null to not show connection options
//   //     this.state.setLoading('remoteDetecting', false);
//   //   }
//   // }

//   // private async handleRemoteMultiUserAuth() {
//   //   // Set the backend URL and show multi-user auth
//   //   this.remoteUrl = this.state.forms.remoteUrl;
//   //   this.changeUIState("multiuser_auth");
//   // }

//   // private async handleRemoteRequestCapability() {
//   //   try {
//   //     this.changeUIState("requestcap");
//   //     await this.client.connect(this.state.forms.remoteUrl);
//   //   } catch (error) {
//   //     this.state.setError('remote', error.message || "Failed to connect");
//   //     this.changeUIState("remote_connection");
//   //   }
//   // }

//   // private async loginToHosting() {
//   //   try {
//   //     await this.client.loginToHosting(this.state.forms.email, this.state.forms.password);

//   //     this.changeUIState("connected");
//   //   } catch (e) {
//   //     if (e.message.includes("Passwords did not match")) {
//   //       this.state.setError('password', "Passwords did not match");
//   //     } else {
//   //       this.state.setError('hostingNotRunning', "Hosting is not running");
//   //     }
//   //   }
//   // }

//   // private changeEmail(email: string) {
//   //   this.state.updateForm('email', email);
//   // }

//   // private changePassword(password: string) {
//   //   this.state.updateForm('password', password);
//   // }

//   // private async unlockAgent(passcode, holochain = true) {
//   //   await this.client.ad4mClient.agent.unlock(passcode, holochain);
//   // }

//   // private async verifyCode(code) {
//   //   try {
//   //     await this.client.verifyCode(code);
//   //   } catch (e) {
//   //     this.state.setError('verifyCode', "Invalid code");
//   //   }
//   // }

//   // private changeCode(code) {
//   //   this.state.updateForm('code', code);
//   // }

//   // private changeUrl(url) {
//   //   if (url !== this.client.url) {
//   //     removeForVersion("ad4mtoken");
//   //     this.client.setToken(null);
//   //   }

//   //   this.client.setUrl(url);
//   // }

//   // private changePort(port: number) {
//   //   this.client.setPort(port);
//   // }

//   // private changeUIState(state) {
//   //   this.state.setView(state);
//   // }

//   // private setIsHostingRunning(val) {
//   //   this.state.setError('hostingNotRunning', val);
//   // }

//   // private changeIsRemote(bol: boolean) {
//   //   this.state.setRemote(bol);
//   // }

//   // private onDownloaded() {
//   //   this.state.setHasClickedDownload(true);
//   // }

//   // private handleAuthChange(event: AuthStates) {
//   //   const customEvent = new CustomEvent("authstatechange", {
//   //     detail: event,
//   //   });
//   //   if (event === "locked") {
//   //     this.state.open();
//   //   }
//   //   if (event === "authenticated") {
//   //     this.state.close();
//   //   }
//   //   this.dispatchEvent(customEvent);
//   //   this.requestUpdate();
//   // }

//   // private handleConfigChange(name: any, val: string) {
//   //   this[name] = val;
//   //   if (val) {
//   //     setForVersion("ad4m" + name, val);
//   //   } else {
//   //     removeForVersion("ad4m" + name);
//   //   }
//   //   this.requestUpdate();
//   // }

//   // private handleConnectionChange(event: ConnectionStates) {
//   //   console.log("**** [Ad4m Connect] Connection state changed:", event);
//   //   if (event === "checking_local") {
//   //     this.state.close(); // Don't show dialog while checking
//   //     return;
//   //   }

//   //   if (event === "connected") {
//   //     if (this.authState !== "authenticated") {
//   //       this.changeUIState("requestcap");
//   //       this.state.open();
//   //     } else {
//   //       this.changeUIState("connected");
//   //       this.state.close();
//   //     }
//   //   }
//   //   if (event === "disconnected") {
//   //     this.state.open();
//   //   }
//   //   if (event === "not_connected") {
//   //     this.state.open();
//   //     // If multi-user mode is enabled, show multi-user auth instead of start
//   //     if (this.remoteUrl) {
//   //       this.changeUIState("multiuser_auth");
//   //     } else {
//   //       this.changeUIState("start");
//   //     }
//   //   }
//   //   const customEvent = new CustomEvent("connectionstatechange", {
//   //     detail: event,
//   //   });
//   //   this.dispatchEvent(customEvent);
//   //   this.requestUpdate();
//   // }

//   // private async startCamera(e) {
//   //   try {
//   //     window["BarcodeDetector"].getSupportedFormats();
//   //   } catch {
//   //     window["BarcodeDetector"] = BarcodeDetectorPolyfill;
//   //   }

//   //   await this.changeUIState("qr");

//   //   setTimeout(async () => {
//   //     const video = this.shadowRoot.querySelector("video");

//   //     if (!video) return;

//   //     const media = await navigator.mediaDevices.getUserMedia({
//   //       audio: false,
//   //       video: { facingMode: "environment" },
//   //     });

//   //     video.srcObject = media;
//   //     video.autoplay = true;
//   //   }, 100);
//   // }

//   // async connect() {
//   //   this.state.open();
//   //   this.requestUpdate();
//   //   const client = await this.client.connect();
//   //   try {
//   //     const status = await client.agent.status();
//   //     if (status.isUnlocked && status.isInitialized) {
//   //       window.location.reload();
//   //     }
//   //   } catch (e) {
//   //     console.warn(e);
//   //   }
//   //   return client;
//   // }

//   // async connectRemote(url) {
//   //   try {
//   //     this.changeUrl(url);
//   //     const client = await this.client.connect(url);
//   //     const status = await client.agent.status();
//   //     if (status.isUnlocked && status.isInitialized) {
//   //       return client;
//   //     }

//   //     this.changeUIState("requestcap");
//   //     return client;
//   //   } catch (e) {
//   //     this.changeUIState("requestcap");
//   //     this.state.open();
//   //   }
//   // }

//   // async requestCapability(bool) {
//   //   try {
//   //     await this.client.requestCapability(bool);
//   //     this.changeUIState("verifycode");
//   //   } catch (e) {
//   //     console.warn(e);
//   //   }
//   // }

//   // async isAuthenticated() {
//   //   return this.client.checkAuth();
//   // }

//   // setOpen(val: boolean) {
//   //   if (val) {
//   //     this.state.open();
//   //   } else {
//   //     this.state.close();
//   //   }
//   // }

//   // setHostingStep(step: number) {
//   //   this.state.updateForm('hostingStep', step);
//   // }

//   // clearState() {
//   //   this.handleConfigChange("port", null);
//   //   this.handleConfigChange("url", null);
//   //   this.handleConfigChange("token", null);
//   //   // this.state.close();
//   //   this.handleConnectionChange("not_connected");
//   //   this.handleAuthChange("unauthenticated");
//   //   this.changeUIState("start");

//   //   this.client.clearState();
//   // }

//   private async changePort(event: CustomEvent) {
//     console.log('Port changed to', event.detail.port);
//     this.core.port = event.detail.port
//     this.requestUpdate();
//   }

//   private async changeUrl(event: CustomEvent) {
//     console.log('URL changed to', event.detail.url);
//     this.core.url = event.detail.url
//     this.requestUpdate();
//   }

//   private async connectLocalNode() {
//     console.log('[Ad4m Connect UI] Connecting to local node on port');
//     await this.core.connect();
//     this.currentView = "local-authentication";
//   }

//   private async connectRemoteNode() {
//     console.log('[Ad4m Connect UI] Connecting to remote node at URL');
//     await this.core.connect();
//     this.currentView = "remote-authentication";
//   }

//   // private async requestCapability() {
//   //   console.log('[Ad4m Connect UI] Requesting capability');
//   //   try {
//   //     const res = await this.core.requestCapability(true);
//   //     console.log('[Ad4m Connect UI] Capability requested successfully', res);
//   //     // this.currentView = "verify-code"; // or whatever view shows the code input
//   //   } catch (e) {
//   //     console.error(e);
//   //     // Handle error - maybe show error state
//   //   }
//   // }

//   private async verifyCode(event: CustomEvent) {
//     console.log('[Ad4m Connect UI] Verifying code', event.detail.code);
//     const success = await this.core.verifyCode(event.detail.code);
//     console.log('[Ad4m Connect UI] Code verified successfully: ', success);
//     this.verificationError = !success;
//     if (success) this.modalOpen = false;
//   }

//   private clearVerificationError() {
//     this.verificationError = false;
//   }

//   renderViews() {
//     if (this.currentView === "connection-options") {
//       return html`
//         <connection-options
//           .port=${this.core.port}
//           .url=${this.core.url}
//           @change-port=${this.changePort}
//           @change-url=${this.changeUrl}
//           @connect-local-node=${this.connectLocalNode}
//           @connect-remote-node=${this.connectRemoteNode}
//         ></connection-options>
//       `;
//     }

//     if (this.currentView === "local-authentication") {
//       return html`
//         <local-authentication 
//           .capabilities=${this.core.options.capabilities}
//           .appname=${this.core.options.appInfo.name}
//           .appiconpath=${this.core.options.appInfo.iconPath}
//           .verificationError=${this.verificationError}
//           @back=${() => { this.currentView = "connection-options" }}
//           @request-capability=${() => this.core.requestCapability(true)}
//           @verify-code=${this.verifyCode}
//           @clear-verification-error=${this.clearVerificationError}
//         ></local-authentication>
//       `;
//     }

//     // if (this.connectionState === "connected") {
//     //   if (this.state.currentView === "verifycode") {
//     //     return VerifyCode({
//     //       code: this.state.forms.code,
//     //       changeCode: this.changeCode,
//     //       changeState: this.changeUIState,
//     //       verifyCode: this.verifyCode,
//     //       isHosting: this.client.isHosting,
//     //       verifyCodeError: this.state.errors.verifyCode,
//     //     });
//     //   }

//     //   return html`
//     //     <request-capability
//     //       .capabilities=${this.capabilities}
//     //       .appname=${this.appName}
//     //       .appiconpath=${this.appIconPath}
//     //       @cancel=${() => {
//     //         this.requestCapability(false);
//     //         this.setOpen(false);
//     //       }}
//     //       @authorize=${() => this.requestCapability(true)}
//     //       @settings=${() => this.changeUIState("settings")}
//     //     ></request-capability>
//     //   `;
//     // }

//     // if (this.state.currentView === "remote_connection") {
//     //   return html`
//     //     <remote-connection
//     //       .initialUrl=${this.state.forms.remoteUrl}
//     //       .detecting=${this.state.loading.remoteDetecting}
//     //       .multiUserDetected=${this.state.detection.remoteMultiUserDetected}
//     //       .error=${this.state.errors.remote}
//     //       @back=${() => this.changeUIState("connection_overview")}
//     //       @url-change=${(e: CustomEvent) => this.handleRemoteUrlChange(e.detail.url)}
//     //       @connect=${() => this.handleRemoteConnect()}
//     //       @multi-user-auth=${() => this.handleRemoteMultiUserAuth()}
//     //       @request-capability=${() => this.handleRemoteRequestCapability()}
//     //     ></remote-connection>
//     //   `;
//     // }

//     // if (this.state.currentView === "multiuser_auth") {
//     //   return html`
//     //     <multi-user-auth
//     //       .email=${this.state.forms.multiUserEmail}
//     //       .password=${this.state.forms.multiUserPassword}
//     //       .verificationCode=${this.state.forms.multiUserVerificationCode}
//     //       .error=${this.state.errors.multiUser}
//     //       .isLoading=${this.state.loading.multiUser}
//     //       .backendUrl=${this.remoteUrl}
//     //       .step=${this.state.forms.multiUserStep}
//     //       .verificationType=${this.state.forms.multiUserVerificationType}
//     //       @email-change=${(e: CustomEvent) => this.changeMultiUserEmail(e.detail.email)}
//     //       @password-change=${(e: CustomEvent) => this.changeMultiUserPassword(e.detail.password)}
//     //       @code-change=${(e: CustomEvent) => this.changeMultiUserVerificationCode(e.detail.code)}
//     //       @email-submit=${() => this.handleMultiUserEmailSubmit()}
//     //       @password-submit=${() => this.handleMultiUserPasswordSubmit()}
//     //       @code-submit=${() => this.handleMultiUserCodeSubmit()}
//     //       @back-to-email=${() => this.handleMultiUserBackToEmail()}
//     //     ></multi-user-auth>
//     //   `;
//     // }

//     // if (this.state.currentView === "hosting") {
//     //   return html`
//     //     <hosting-view
//     //       .email=${this.state.forms.email}
//     //       .password=${this.state.forms.password}
//     //       .step=${this.state.forms.hostingStep}
//     //       .passwordError=${this.state.errors.password}
//     //       .isHostingRunning=${!!this.state.errors.hostingNotRunning}
//     //       @back=${() => this.changeUIState("not_connected")}
//     //       @email-change=${(e: CustomEvent) => this.changeEmail(e.detail.email)}
//     //       @password-change=${(e: CustomEvent) => this.changePassword(e.detail.password)}
//     //       @check-email=${() => this.checkEmail()}
//     //       @login=${() => this.loginToHosting()}
//     //       @reset-hosting=${() => this.setHostingStep(0)}
//     //       @clear-running=${() => this.setIsHostingRunning(false)}
//     //     ></hosting-view>
//     //   `;
//     // }

//     // if (this.state.currentView === "qr") {
//     //   return html`
//     //     <scan-qr-code
//     //       .uiState=${this.state.currentView}
//     //       @stop=${() => this.changeUIState("connection_overview")}
//     //       @success=${(e: CustomEvent) => {
//     //         this.changeUrl(e.detail.qrValue);
//     //         this.client.connect(e.detail.qrValue);
//     //       }}
//     //     ></scan-qr-code>
//     //   `;
//     // }

//     // if (this.authState === "locked") {
//     //   return ErrorState({
//     //     type: 'agent-locked',
//     //     onAction: this.connect,
//     //   });
//     // }

//     // if (this.connectionState === "not_connected") {
//     //   return html`
//     //     <start-view
//     //       .isMobile=${this.state.isMobile}
//     //       .hasClickedDownload=${this.state.hasClickedDownload}
//     //       .hosting=${this.hosting}
//     //       @connect=${() => this.connect()}
//     //       @settings=${() => this.changeUIState("settings")}
//     //       @s
//     // can-qr=${() => this.startCamera(null)}
//     //       @hosting=${() => this.changeUIState("hosting")}
//     //       @downloaded=${() => this.onDownloaded()}
//     //     ></start-view>
//     //   `;
//     // }

//     // if (this.connectionState === "disconnected") {
//     //   return ErrorState({ type: 'disconnected', onAction: this.connect });
//     // }

//     // if (this.connectionState === "port_not_found") {
//     //   return ErrorState({ type: 'request-blocked', onAction: () => location.reload() });
//     // }
//   }

//   // settingsButton() {
//   //   if (this.authState === "authenticated") {
//   //     return html`
//   //       <div class="settings-button" @click=${() => {
//   //         this.currentView = "connection-options";
//   //         this.modalOpen = true;
//   //       }}>
//   //         ${Ad4mLogo()}
//   //       </div>
//   //     `;
//   //   }
//   //   return null;
//   // }

//   render() {
//     // Show settings button when authenticated and modal is closed
//     if (!this.modalOpen && this.core.authState === "authenticated") {
//       return html`
//         <div class="settings-button" @click=${() => {
//           this.currentView = "connection-options";
//           this.modalOpen = true;
//         }}>
//           ${Ad4mLogo()}
//         </div>
//       `;
//     }

//     // Render the main modal
//     return html`
//       <div class="wrapper">
//         <div class="modal">
//           <header class="modal-header">
//             ${Ad4mLogo()}
//           </header>
//           <main class="modal-content">
//             ${this.renderViews()}
//           </main>
//         </div>
//         <div class="backdrop" />
//       </div>
//     `;
//   }
// }

// export default function Ad4mConnectUI(core: Ad4mConnect): Ad4mConnectElement {
//   // Create element and inject the core
//   const element = new Ad4mConnectElement();
//   element.core = core;
  
//   if (core.embedded) {
//     // Running in embedded mode - no UI needed
//     console.log('[Ad4m Connect] Running in embedded mode - UI will not be shown');
//   } else {
//     // Not embedded - mount UI to DOM
//     console.log('[Ad4m Connect UI] Mounting UI to DOM');
    
//     // Check if we have a token - if so, don't show UI initially
//     const storedToken = core.token;
    
//     if (storedToken) {
//       // Has token - will auto-connect, keep UI hidden initially
//       // connectedCallback will handle the connection attempt
//       element.style.display = 'none';
//       document.body.appendChild(element);
      
//       // Listen for auth state - show UI only if auth fails
//       core.addEventListener('authstatechange', (e: any) => {
//         if (e.detail === 'unauthenticated') {
//           element.style.display = '';
//         }
//       });
//     } else {
//       // No token - show UI immediately for user to connect
//       document.body.appendChild(element);
//     }
//   }

//   return element;
// }
