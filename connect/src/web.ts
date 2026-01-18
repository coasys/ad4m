import { css, html, LitElement } from "lit";
import { customElement, state } from "lit/decorators.js";
import autoBind from "auto-bind";

import Ad4mConnect from "./core";
import { Ad4mLogo } from "./components/icons";

import "./components/views/ConnectionOptions";
import "./components/views/LocalAuthentication";


type Views = 'connection-options' | 'local-authentication' | 'remote-authentication';

// TODO: update text color vars when decided on

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
    position: absolute;
    bottom: 20px;
    right: 20px;
    color: var(--ac-primary-color);
  }
`;

@customElement("ad4m-connect")
export class Ad4mConnectElement extends LitElement {
  static styles = [styles];

  @state() private currentView: Views = "connection-options";
  @state() private modalOpen = false;
  @state() private verificationError = false;

  core: Ad4mConnect;

  // Timeout reference for auto-submit when 6 digits are entered
  private _codeSubmitTimeout: ReturnType<typeof setTimeout> | null = null;

  private detectMobile() {
    const toMatch = [
      /Android/i,
      /webOS/i,
      /iPhone/i,
      /iPad/i,
      /iPod/i,
      /BlackBerry/i,
      /Windows Phone/i,
    ];

    return toMatch.some((toMatchItem) => navigator.userAgent.match(toMatchItem));
  }

  connectedCallback() {
    super.connectedCallback();
    autoBind(this); // needed if 'this' used in callbacks 

    console.log('[Ad4m Connect UI] Initializing Ad4m Connect UI component', this.core);

    if (this.core.token) {
      this.core.connect()
    } else {
      this.currentView = "connection-options";
      this.modalOpen = true;
    }
  }

  private async changePort(event: CustomEvent) {
    console.log('Port changed to', event.detail.port);
    this.core.port = event.detail.port
    this.requestUpdate();
  }

  private async changeUrl(event: CustomEvent) {
    console.log('URL changed to', event.detail.url);
    this.core.url = event.detail.url
    this.requestUpdate();
  }

  private async connectLocalNode() {
    console.log('[Ad4m Connect UI] Connecting to local node on port');
    await this.core.connect();
    this.currentView = "local-authentication";
  }

  private async connectRemoteNode() {
    console.log('[Ad4m Connect UI] Connecting to remote node at URL');
    await this.core.connect();
    this.currentView = "remote-authentication";
  }

  private async verifyCode(event: CustomEvent) {
    console.log('[Ad4m Connect UI] Verifying code', event.detail.code);
    const success = await this.core.verifyCode(event.detail.code);
    console.log('[Ad4m Connect UI] Code verified successfully: ', success);
    this.verificationError = !success;
    if (success) this.modalOpen = false;
  }

  private clearVerificationError() {
    this.verificationError = false;
  }

  renderViews() {
    if (this.currentView === "connection-options") {
      return html`
        <connection-options
          .port=${this.core.port}
          .url=${this.core.url}
          @change-port=${this.changePort}
          @change-url=${this.changeUrl}
          @connect-local-node=${this.connectLocalNode}
          @connect-remote-node=${this.connectRemoteNode}
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
          @verify-code=${this.verifyCode}
          @clear-verification-error=${this.clearVerificationError}
        ></local-authentication>
      `;
    }

    // if (this.state.currentView === "multiuser_auth") {
    //   return html`
    //     <multi-user-auth
    //       .email=${this.state.forms.multiUserEmail}
    //       .password=${this.state.forms.multiUserPassword}
    //       .verificationCode=${this.state.forms.multiUserVerificationCode}
    //       .error=${this.state.errors.multiUser}
    //       .isLoading=${this.state.loading.multiUser}
    //       .backendUrl=${this.remoteUrl}
    //       .step=${this.state.forms.multiUserStep}
    //       .verificationType=${this.state.forms.multiUserVerificationType}
    //       @email-change=${(e: CustomEvent) => this.changeMultiUserEmail(e.detail.email)}
    //       @password-change=${(e: CustomEvent) => this.changeMultiUserPassword(e.detail.password)}
    //       @code-change=${(e: CustomEvent) => this.changeMultiUserVerificationCode(e.detail.code)}
    //       @email-submit=${() => this.handleMultiUserEmailSubmit()}
    //       @password-submit=${() => this.handleMultiUserPasswordSubmit()}
    //       @code-submit=${() => this.handleMultiUserCodeSubmit()}
    //       @back-to-email=${() => this.handleMultiUserBackToEmail()}
    //     ></multi-user-auth>
    //   `;
    // }
  }

  render() {
    // Show settings button when authenticated and modal is closed
    if (!this.modalOpen && this.core.authState === "authenticated") {
      return html`
        <div class="settings-button" @click=${() => {
          this.currentView = "connection-options";
          this.modalOpen = true;
        }}>
          ${Ad4mLogo()}
        </div>
      `;
    }

    // Render the main modal
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
    
    // Check if we have a token - if so, don't show UI initially
    const storedToken = core.token;
    
    if (storedToken) {
      // Has token - will auto-connect, keep UI hidden initially
      // connectedCallback will handle the connection attempt
      element.style.display = 'none';
      document.body.appendChild(element);
      
      // Listen for auth state - show UI only if auth fails
      core.addEventListener('authstatechange', (e: any) => {
        if (e.detail === 'unauthenticated') {
          element.style.display = '';
        }
      });
    } else {
      // No token - show UI immediately for user to connect
      document.body.appendChild(element);
    }
  }

  return element;
}
