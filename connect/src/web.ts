import { css, html, LitElement } from "lit";
import { customElement, state } from "lit/decorators.js";
import autoBind from "auto-bind";
import { VerificationRequestResult } from "@coasys/ad4m/lib/src/runtime/RuntimeResolver";
import { connectWebSocket, setLocal } from "./utils";
import Ad4mConnect from "./core";
import { Ad4mLogo } from "./components/icons";
import { fetchHosts } from "./services/hostIndex";
import type { RemoteHost, UserInfo } from "./types";

import "./components/views/ConnectionOptions";
import "./components/views/LocalAuthentication";
import "./components/views/RemoteAuthentication";
import "./components/views/CurrentState";
import "./components/views/HostBrowser";
import "./components/views/HostDetail";
import "./components/views/LoggedInDashboard";

type Views =
  | 'connection-options'
  | 'local-authentication'
  | 'remote-authentication'
  | 'current-state'
  | 'host-browser'
  | 'host-detail'
  | 'logged-in-dashboard';

const styles = css`
  @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;700&display=swap');

  :host {
    font-family: 'DM Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
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
    z-index: 99999;
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
    width: calc(100vw - 30px);
    max-width: 480px;
    max-height: calc(100vh - 30px);
    overflow-y: auto;
    backdrop-filter: blur(10px);
    -webkit-backdrop-filter: blur(10px); /* Safari */
    display: flex;
    flex-direction: column;
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
    flex: 1 1 auto;
    min-height: 0;
  }

  .settings-button {
    appearance: none;
    border: none;
    background: transparent;
    padding: 0;
    cursor: pointer;
    position: fixed;
    bottom: 10px;
    right: 10px;
    color: var(--ac-primary-color);
    width: 34px;
    height: 34px;
    z-index: 99999;
  }

  .settings-button.low-credit {
    color: var(--ac-danger-color);
    animation: pulse-danger 2s ease-in-out infinite;
  }

  @keyframes pulse-danger {
    0%, 100% { opacity: 1; }
    50% { opacity: 0.4; }
  }
`;

@customElement("ad4m-connect")
export class Ad4mConnectElement extends LitElement {
  static styles = [styles];

  core: Ad4mConnect;

  // Global state
  @state() modalOpen = false;
  @state() private currentView: Views = "connection-options";

  // Local authentication state
  @state() private verificationError = false;

  // Remote authentication state
  @state() private remoteAuthLoading = false;
  @state() private remoteAuthState: VerificationRequestResult | null = null;
  @state() private emailCodeError = false;
  @state() private passwordError = false;
  @state() private accountCreationError = false;

  // Hosting state
  @state() private hosts: RemoteHost[] = [];
  @state() private hostsLoading = false;
  @state() private hostsError: string | null = null;
  @state() private selectedHost: RemoteHost | null = null;
  @state() private userInfo: UserInfo | null = null;
  @state() private lowCredit = false;
  @state() private requestingPayment = false;
  @state() private paymentError: string | null = null;

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

    // Hosting events
    this.core.addEventListener('userinfochange', (e: any) => {
      const incoming = e.detail as UserInfo;
      // Preserve locally-set wallet address until server confirms it via polling
      if (this.userInfo?.hotWalletAddress && !incoming.hotWalletAddress) {
        incoming.hotWalletAddress = this.userInfo.hotWalletAddress;
      }
      this.userInfo = incoming;
      this.lowCredit = this.userInfo.remainingCredits <= 10;
      this.requestUpdate();
    });

    this.core.addEventListener('creditdepleted', () => {
      this.lowCredit = true;
      this.requestUpdate();
    });

    if (this.core.token) {
      // Try to auto-connect with stored token
      this.core.connect().then(() => {
        // If we have a connected host, start credit polling
        if (this.core.connectedHost) {
          this.selectedHost = this.core.connectedHost;
          this.core.startCreditPolling();
        }
      }).catch((error) => {
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

  // --- Hosting handlers ---

  private async browseHosts() {
    this.hostsLoading = true;
    this.hostsError = null;
    this.currentView = "host-browser";

    try {
      this.hosts = await fetchHosts(this.core.hostIndexUrl);
    } catch (error) {
      console.error('[Ad4m Connect UI] Failed to fetch hosts:', error);
      this.hostsError = error instanceof Error ? error.message : "Failed to load hosts";
    } finally {
      this.hostsLoading = false;
    }
  }

  private async retryFetchHosts() {
    this.hostsLoading = true;
    this.hostsError = null;

    try {
      this.hosts = await fetchHosts(this.core.hostIndexUrl);
    } catch (error) {
      console.error('[Ad4m Connect UI] Failed to fetch hosts:', error);
      this.hostsError = error instanceof Error ? error.message : "Failed to load hosts";
    } finally {
      this.hostsLoading = false;
    }
  }

  private selectHost(e: CustomEvent) {
    this.selectedHost = e.detail.host as RemoteHost;
    this.currentView = "host-detail";
  }

  private async proceedToAuth(e: CustomEvent) {
    const host = e.detail.host as RemoteHost;
    this.selectedHost = host;

    const candidateUrl = host.url;

    try {
      // Verify WS reachability before committing URL
      await connectWebSocket(candidateUrl);
      console.log('[Ad4m Connect UI] Host connection successful:', host.name);

      // Verify it's an AD4M API
      const prevUrl = this.core.url;
      this.core.url = candidateUrl;
      const isValid = await this.core.isValidAd4mAPI();
      if (!isValid) {
        this.core.url = prevUrl;
        throw new Error("Server is reachable but doesn't appear to be an AD4M executor");
      }

      // Validation passed — persist
      setLocal("ad4m-url", candidateUrl);

      // Navigate to remote authentication
      this.currentView = "remote-authentication";
    } catch (error) {
      console.error('[Ad4m Connect UI] Host connection failed:', error);
      // Go back to host detail with an error — for now just go back to browser
      this.hostsError = error instanceof Error ? error.message : "Connection failed";
      this.currentView = "host-browser";
    }
  }

  private async handleAuthSuccess() {
    // Called after successful remote auth when a host is selected
    if (this.selectedHost) {
      this.core.setConnectedHost(this.selectedHost);
      this.core.startCreditPolling();
      this.currentView = "logged-in-dashboard";
    } else {
      this.modalOpen = false;
    }
  }

  private async handleRequestTopUp(e: CustomEvent) {
    this.requestingPayment = true;
    this.paymentError = null;

    try {
      const result = await this.core.requestTopUp(e.detail.amountHOT);
      if (!result.success) {
        this.paymentError = result.message;
      }
    } catch (error) {
      this.paymentError = error instanceof Error ? error.message : "Payment request failed";
    } finally {
      this.requestingPayment = false;
    }
  }

  private async handleSetWalletAddress(e: CustomEvent) {
    try {
      await this.core.ad4mClient!.agent.setHotWalletAddress(e.detail.address);
      if (this.userInfo) {
        this.userInfo = { ...this.userInfo, hotWalletAddress: e.detail.address };
      }
    } catch (error) {
      console.error('[Ad4m Connect] Failed to set wallet address:', error);
    }
  }

  // --- Remote authentication handlers (kept for remote-authentication view) ---

  private async connectRemoteNode(e: CustomEvent) {
    // Legacy direct-URL connection (kept for backward compat if needed)
    this.core.url = e.detail.remoteUrl;
    setLocal("ad4m-url", this.core.url);

    try {
      await connectWebSocket(e.detail.remoteUrl);
      const isValidAd4mApi = await this.core.isValidAd4mAPI();
      if (!isValidAd4mApi) throw new Error("Server is reachable but doesn't appear to be an AD4M executor");

      this.currentView = "remote-authentication";
    } catch (error) {
      console.error('[Ad4m Connect UI] Remote connection failed:', error);
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
      if (success) await this.handleAuthSuccess();
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
      if (success) await this.handleAuthSuccess();
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
      if (success) await this.handleAuthSuccess();
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
          .showHosting=${!!this.core.options.hosting}
          @change-port=${this.changePort}
          @connect-local-node=${this.connectLocalNode}
          @browse-hosts=${this.browseHosts}
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

    if (this.currentView === "host-browser") {
      return html`
        <host-browser
          .hosts=${this.hosts}
          .loading=${this.hostsLoading}
          .error=${this.hostsError}
          .lastHostId=${this.core.connectedHost?.id ?? null}
          .defaultUrl=${this.core.options.remoteUrl ?? ''}
          @select-host=${this.selectHost}
          @back=${() => { this.currentView = "connection-options" }}
          @retry=${this.retryFetchHosts}
        ></host-browser>
      `;
    }

    if (this.currentView === "host-detail") {
      return html`
        <host-detail
          .host=${this.selectedHost!}
          @proceed-to-auth=${this.proceedToAuth}
          @back=${() => { this.currentView = "host-browser" }}
        ></host-detail>
      `;
    }

    if (this.currentView === "remote-authentication") {
      return html`
        <remote-authentication
          .host=${this.selectedHost}
          .remoteAuthLoading=${this.remoteAuthLoading}
          .remoteAuthState=${this.remoteAuthState}
          .emailCodeError=${this.emailCodeError}
          .passwordError=${this.passwordError}
          .accountCreationError=${this.accountCreationError}
          @back=${() => {
            this.currentView = this.selectedHost ? "host-detail" : "connection-options";
          }}
          @email-login=${this.emailLogin}
          @verify-email-code=${this.verifyEmailCode}
          @password-login=${this.passwordLogin}
          @create-account=${this.createAccount}
          @clear-email-code-error=${() => { this.emailCodeError = false; }}
        ></remote-authentication>
      `;
    }

    if (this.currentView === "logged-in-dashboard") {
      return html`
        <logged-in-dashboard
          .connectedHost=${this.core.connectedHost}
          .userInfo=${this.userInfo}
          .requestingPayment=${this.requestingPayment}
          .paymentError=${this.paymentError}
          @close=${() => { this.modalOpen = false; }}
          @disconnect=${this.disconnect}
          @request-top-up=${this.handleRequestTopUp}
          @set-wallet-address=${this.handleSetWalletAddress}
        ></logged-in-dashboard>
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
          class="settings-button ${this.lowCredit ? 'low-credit' : ''}"
          aria-label="Open settings"
          @click=${() => {
            // If connected to a remote host, show dashboard; otherwise show current-state
            this.currentView = this.core.connectedHost ? "logged-in-dashboard" : "current-state";
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
