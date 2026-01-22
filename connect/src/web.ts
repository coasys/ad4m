import { css, html, LitElement } from "lit";
import { customElement, state } from "lit/decorators.js";
import autoBind from "auto-bind";
import { VerificationRequestResult } from "@coasys/ad4m/lib/src/runtime/RuntimeResolver";
import { connectWebSocket, setLocal } from "./utils";
import Ad4mConnect from "./core";
import { Ad4mLogo } from "./components/icons";

import "./components/views/ConnectionOptions";
import "./components/views/LocalAuthentication";
import "./components/views/RemoteAuthentication";
import "./components/views/CurrentState";

type Views = 'connection-options' | 'local-authentication' | 'remote-authentication' | 'current-state';

const styles = css`
  @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;700&display=swap');

  :host {
    --ac-primary-color: #91e3fd;
    --ac-primary-color-light: #acebff;
    --ac-success-color: #5dd27d;
    --ac-danger-color: #f4367f;
    --ac-text-color: #fff;
    --ac-background-color: #191c3fe0;
    --ac-border-color-dark: #91d4fd2b;
    --ac-border-color-light: #91d4fd69;
  }

  * {
    box-sizing: border-box;
  }

  .wrapper {
    position: fixed;
    display: grid;
    place-content: center;
    top: 0;
    left: 0;
    height: 100vh;
    width: 100vw;
    z-index: 100;
  }

  .backdrop {
    position: absolute;
    top: 0;
    left: 0;
    height: 100vh;
    width: 100vw;
    background-color: rgba(0, 0, 0, 0.5);
  }

  .modal {
    z-index: 10;
    background-color: var(--ac-background-color);
    border: 1px solid var(--ac-border-color-dark);
    border-radius: 12px;
    padding: 30px;
    width: calc(100vw - 10px);
    max-width: 480px;
    max-height: 90vh;
    overflow-y: auto;
    backdrop-filter: blur(10px);
    -webkit-backdrop-filter: blur(10px); /* Safari */
  }

  .modal-header {
    display: flex;
    align-items: center;
    justify-content: center;
    color: var(--ac-primary-color);
    margin: 10px 0 30px 0;
  }

  .modal-header > svg {
    width: 70px;
    height: 70px;
  }

  .modal-content {
    display: flex;
    flex-direction: column;
    justify-content: center;
  }

  .settings-button {
    appearance: none;
    border: none;
    background: transparent;
    padding: 0;
    cursor: pointer;
    position: absolute;
    bottom: 20px;
    right: 20px;
    color: var(--ac-primary-color);
    width: 40px;
    height: 40px;
  }
`;

@customElement("ad4m-connect")
export class Ad4mConnectElement extends LitElement {
  static styles = [styles];

  core: Ad4mConnect;

  // Global state
  @state() modalOpen = false;
  @state() private currentView: Views = "connection-options";

  // Connection options state
  @state() private connectingToRemoteNode = false;
  @state() private remoteNodeError = false;

  // Local authentication state
  @state() private verificationError = false;

  // Remote authentication state
  @state() private remoteAuthLoading = false;
  @state() private remoteAuthState: VerificationRequestResult | null = null;
  @state() private emailCodeError = false;
  @state() private passwordError = false;
  @state() private accountCreationError = false;

  connectedCallback() {
    super.connectedCallback();
    autoBind(this);

    // Set up auth listener before attempting connection
    this.core.addEventListener('authstatechange', (e: any) => {
      if (e.detail === 'unauthenticated') {
        // Token expired or invalid - show connection options
        this.currentView = "connection-options";
        this.modalOpen = true;
      }
      // Trigger re-render to update UI based on new auth state
      this.requestUpdate();
    });

    if (this.core.token) {
      // Try to auto-connect with stored token
      this.core.connect().catch((error) => {
        // Connection failed - show connection options
        console.error('[Ad4m Connect UI] Auto-connect failed:', error);
        this.currentView = "connection-options";
        this.modalOpen = true;
      });
    } else {
      // No token - show connection options
      this.currentView = "connection-options";
      this.modalOpen = true;
    }
  }

  private async changePort(event: CustomEvent) {
    this.core.port = event.detail.port;
    setLocal("ad4m-port", this.core.port.toString());
    this.requestUpdate();
  }

  private async connectLocalNode() {
    // Update URL to local and persist
    this.core.url = `ws://localhost:${this.core.port}/graphql`;
    setLocal("ad4m-url", this.core.url);
    
    try {
      await this.core.connect();
      this.currentView = "local-authentication";
    } catch (error) {
      console.error('[Ad4m Connect UI] Local node connection failed:', error);
      this.currentView = "connection-options";
    }
  }

  private async verifyLocalAd4mCode(event: CustomEvent) {
    const success = await this.core.verifyLocalAd4mCode(event.detail.code);
    this.verificationError = !success;
    if (success) this.modalOpen = false;
  }

  private async connectRemoteNode(e: CustomEvent) {
    this.remoteNodeError = false;
    this.connectingToRemoteNode = true;
    this.core.url = e.detail.remoteUrl;
    // Persist URL immediately so it's remembered even if validation fails
    setLocal("ad4m-url", this.core.url);

    try {
      // Check if the server is reachable
      await connectWebSocket(e.detail.remoteUrl);
      console.log('[Ad4m Connect UI] Remote connection successful');
  
      // Verify it's actually an AD4M API
      const isValidAd4mApi = await this.core.isValidAd4mAPI();
      if (!isValidAd4mApi) throw new Error("Server is reachable but doesn't appear to be an AD4M executor");
      console.log('[Ad4m Connect UI] Remote AD4M API verified');
  
      // TODO: Handle multi-user flow differently if needed
      const isMultiUser = await this.core.isMultiUser();
      console.log('[Ad4m Connect UI] Remote multi-user detected:', isMultiUser);

      // Navigate to remote authentication view
      this.currentView = "remote-authentication";
    } catch (error) {
      console.error('[Ad4m Connect UI] Remote connection failed:', error);
      this.remoteNodeError = true;
    } finally {
      this.connectingToRemoteNode = false;
    }
  }

  private async emailLogin(e: CustomEvent) {
    try {
      this.remoteAuthLoading = true;
      this.remoteAuthState = await this.core.submitEmail(e.detail.email);
    } finally {
      this.remoteAuthLoading = false;
    }
  }

  private async verifyEmailCode(event: CustomEvent) {
    try {
      this.remoteAuthLoading = true;
      const success = await this.core.verifyEmailCode(event.detail.email, event.detail.code);
      this.emailCodeError = !success;
      if (success) this.modalOpen = false;
    } catch (error) {
      this.emailCodeError = true;
    } finally {
      this.remoteAuthLoading = false;
    }
  }

  private async passwordLogin(event: CustomEvent) {
    try {
      this.remoteAuthLoading = true;
      const success = await this.core.loginWithPassword(event.detail.email, event.detail.password);
      this.passwordError = !success;
      if (success) this.modalOpen = false;
    } catch (error) {
      this.passwordError = true;
    } finally {
      this.remoteAuthLoading = false;
    }
  }

  private async createAccount(event: CustomEvent) {
    try {
      this.remoteAuthLoading = true;
      const success = await this.core.createAccount(event.detail.email, event.detail.password);
      this.accountCreationError = !success;
      if (success) this.modalOpen = false;
      // TODO: request verification instead of auto-login when testing complete
      // const result = await this.core.createAccount(event.detail.email, event.detail.password);
      // console.log('*** create account result', result);
      // this.accountCreationError = !result.success;
      // if (result.success) {
      //   await this.emailLogin(new CustomEvent("", { detail: { email: event.detail.email }}));
      // }
    } catch (error) {
      this.accountCreationError = true;
    } finally {
      this.remoteAuthLoading = false;
    }
  }

  private async disconnect() {
    await this.core.disconnect();
    window.location.reload();
  }

  renderViews() {
    if (this.currentView === "connection-options") {
      return html`
        <connection-options
          .port=${this.core.port}
          .remoteUrl=${this.core.options.remoteUrl}
          .connectingToRemoteNode=${this.connectingToRemoteNode}
          .remoteNodeError=${this.remoteNodeError}
          @change-port=${this.changePort}
          @connect-local-node=${this.connectLocalNode}
          @connect-remote-node=${this.connectRemoteNode}
          @clear-remote-node-error=${() => { this.remoteNodeError = false; }}
        ></connection-options>
      `;
    }

    if (this.currentView === "local-authentication") {
      return html`
        <local-authentication 
          .capabilities=${this.core.options.capabilities}
          .appname=${this.core.options.appInfo.name}
          .appiconpath=${this.core.options.appInfo.iconPath}
          .verificationError=${this.verificationError}
          @back=${() => { this.currentView = "connection-options" }}
          @request-capability=${() => this.core.requestCapability(true)}
          @verify-code=${this.verifyLocalAd4mCode}
          @clear-verification-error=${() => { this.verificationError = false; }}
        ></local-authentication>
      `;
    }

    if (this.currentView === "remote-authentication") {
      return html`
        <remote-authentication
          .remoteAuthLoading=${this.remoteAuthLoading}
          .remoteAuthState=${this.remoteAuthState}
          .emailCodeError=${this.emailCodeError}
          .passwordError=${this.passwordError}
          .accountCreationError=${this.accountCreationError}
          @back=${() => { this.currentView = "connection-options" }}
          @email-login=${this.emailLogin}
          @verify-email-code=${this.verifyEmailCode}
          @password-login=${this.passwordLogin}
          @create-account=${this.createAccount}
          @clear-email-code-error=${() => { this.emailCodeError = false; }}
        ></remote-authentication>
      `;
    }

    if (this.currentView === "current-state") {
      return html`
        <current-state
          .url=${this.core.url}
          .port=${this.core.port}
          .authState=${this.core.authState}
          @close=${() => { this.modalOpen = false; }}
          @disconnect=${this.disconnect}
        ></current-state>
      `;
    }
  }

  render() {
    if (this.modalOpen) {
      // Show modal
      return html`
        <div class="wrapper">
          <div class="modal">
            <header class="modal-header">
              ${Ad4mLogo()}
            </header>
            <main class="modal-content">
              ${this.renderViews()}
            </main>
          </div>
          <div class="backdrop" />
        </div>
      `;
    } else if (this.core.authState === "authenticated") {
      // Show settings button when authenticated and modal is closed
      return html`
        <button
          type="button"
          class="settings-button"
          aria-label="Open settings"
          @click=${() => {
            this.currentView = "current-state";
            this.modalOpen = true;
          }}
        >
          ${Ad4mLogo()}
        </button>
      `;
    }

    // Nothing to render
    return null;
  }
}

export default function Ad4mConnectUI(core: Ad4mConnect): Ad4mConnectElement {
  // Create element and inject the core
  const element = new Ad4mConnectElement();
  element.core = core;
  
  if (core.embedded) {
    // Running in embedded mode - no UI needed
    console.log('[Ad4m Connect] Running in embedded mode - UI will not be shown');
  } else {
    // Not embedded - mount UI to DOM
    console.log('[Ad4m Connect UI] Mounting UI to DOM');
    document.body.appendChild(element);
  }

  return element;
}
