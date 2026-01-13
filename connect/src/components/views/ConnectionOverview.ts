import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";

@customElement("connection-overview")
export class ConnectionOverview extends LitElement {
  @property({ type: Boolean }) localDetected = false;
  @property({ type: Boolean }) multiUserConfigured = false;
  @property({ type: String }) backendUrl?: string;
  @property({ type: String }) configuredUrl?: string;
  @property({ type: Boolean }) isMobile = false;

  static styles = [
    sharedStyles,
    css`
      .header {
        text-align: center;
      }

      .options {
        display: flex;
        flex-direction: column;
        gap: 24px;
      }

      .option {
        display: flex;
        flex-direction: column;
        align-items: center;
        background: rgba(128, 178, 201, 0.14);
        box-shadow: 0 0 0 1px var(--ac-border-color-light);
        border-radius: 8px;
        padding: 20px;
        gap: 18px;
        transition: all 0.3s ease;
      }

      .option:hover {
        background: rgba(128, 178, 201, 0.2);
        box-shadow: 0 0 0 2px var(--ac-primary-color);
      }

      .option-header {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: -8px;
      }

      .option-header h3 {
        font-weight: 600;
      }

      h3 span {
        font-size: 14px;
        font-weight: 400;
        opacity: 0.8;
        margin-left: 6px;
      }

      .option-header svg {
        width: 24px;
        height: 24px;
        stroke-width: 2;
        flex-shrink: 0;
        color: var(--ac-primary-color);
      }

      .url-display {
        margin: 8px 0 16px 0;
        font-size: 12px;
        opacity: 0.7;
        font-family: monospace;
        word-break: break-all;
      }

      button {
        width: 100%;
      }
    `
  ];

  private handleConnectLocal() {
    this.dispatchEvent(new CustomEvent("connect-local", { bubbles: true, composed: true }));
  }

  private handleConnectRemote() {
    this.dispatchEvent(new CustomEvent("connect-remote", { bubbles: true, composed: true }));
  }

  private handleScanQR() {
    this.dispatchEvent(new CustomEvent("scan-qr", { bubbles: true, composed: true }));
  }

  private handleDownload() {
    this.dispatchEvent(new CustomEvent("download-ad4m", { bubbles: true, composed: true }));
  }

  render() {
    const showLocalOption = !this.isMobile && this.localDetected;
    const showRemoteOption = true;
    const showQROption = this.isMobile;
    const showDownload = !this.localDetected; // && !this.multiUserConfigured;
    const displayUrl = this.backendUrl || this.configuredUrl;

    return html`
      <div class="container">
        <div class="header">
          <h1>Connect AD4M</h1>
          ${!this.localDetected
            ? html`
                <p>
                  No local AD4M detected.
                  ${!this.isMobile
                    ? "Download AD4M or connect to a remote executor."
                    : "Connect to a remote executor or scan a QR code."}
                </p>
              `
            : html`<h3>How would you like to connect?</h3>`}
        </div>

        <div class="options">
          ${showLocalOption
            ? html`
                <div class="option option--primary">
                  <div class="option-header">
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor">
                      <circle cx="12" cy="12" r="10"></circle>
                      <circle cx="12" cy="12" r="3" fill="currentColor"></circle>
                    </svg>
                    <h3>Local Node <span>(Found)</span></h3>
                  </div>
                  <p class="option-description">
                    Connect to your local executor on port 12000
                  </p>
                  <button class="primary" @click=${this.handleConnectLocal}>
                    Connect to Local AD4M
                  </button>
                </div>
              `
            : ""}

          ${showRemoteOption
            ? html`
                <div class="option ${!showLocalOption ? "option--primary" : ""}">
                  <div class="option-header">
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor">
                      <circle cx="12" cy="12" r="10"></circle>
                      <path d="M2 12h20"></path>
                      <path d="M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z"></path>
                    </svg>
                    <h3>Remote Node <span>${displayUrl ? "(Configured)" : ""}</span></h3>
                  </div>
                  ${displayUrl
                    ? html`
                        <p class="option-description">
                          ${this.multiUserConfigured
                            ? "Multi-user backend configured"
                            : "Connect to AD4M node at URL"}
                        </p>
                        <div class="url-display">${displayUrl}</div>
                      `
                    : html`
                        <p class="option-description">
                          Connect to a remote executor or multi-user backend
                        </p>
                      `}
                  <button 
                    class="${showLocalOption ? "secondary" : "primary"}" 
                    @click=${this.handleConnectRemote}
                  >
                    ${displayUrl ? "Connect to Configured Node" : "Enter Remote URL"}
                  </button>
                </div>
              `
            : ""}

          ${showQROption
            ? html`
                <div class="option">
                  <div class="option-header">
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor">
                      <rect x="3" y="3" width="7" height="7"></rect>
                      <rect x="14" y="3" width="7" height="7"></rect>
                      <rect x="14" y="14" width="7" height="7"></rect>
                      <rect x="3" y="14" width="7" height="7"></rect>
                    </svg>
                    <h3>Scan QR Code</h3>
                  </div>
                  <p class="option-description">
                    Scan a QR code from another device
                  </p>
                  <button class="secondary" @click=${this.handleScanQR}>
                    Scan QR Code
                  </button>
                </div>
              `
            : ""}

          ${showDownload
            ? html`
                <div class="option option--download">
                  <div class="option-header">
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor">
                      <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"></path>
                      <polyline points="7 10 12 15 17 10"></polyline>
                      <line x1="12" y1="15" x2="12" y2="3"></line>
                    </svg>
                    <h3>Get AD4M</h3>
                  </div>
                  <p class="option-description">
                    Download and install AD4M on your device
                  </p>
                  <button class="primary" @click=${this.handleDownload}>
                    Download AD4M
                  </button>
                </div>
              `
            : ""}
        </div>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "connection-overview": ConnectionOverview;
  }
}
