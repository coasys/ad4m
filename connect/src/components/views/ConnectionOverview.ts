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
      margin-bottom: 16px;
    }

    h2 {
      margin: 0 0 16px 0;
    }

    .options {
      display: flex;
      flex-direction: column;
      gap: 16px;
    }

    .option {
      background: rgba(145, 227, 253, 0.05);
      border: 1px solid rgba(145, 227, 253, 0.2);
      border-radius: 8px;
      padding: 20px;
      transition: all 0.3s ease;
    }

    .option:hover {
      border-color: rgba(145, 227, 253, 0.4);
      background: rgba(145, 227, 253, 0.08);
    }

    .option--primary {
      border: 2px solid var(--primary-color);
      background: rgba(145, 227, 253, 0.1);
    }

    .option--primary:hover {
      background: rgba(145, 227, 253, 0.15);
    }

    .option--download {
      border-style: dashed;
      opacity: 0.9;
    }

    .option-header {
      display: flex;
      align-items: center;
      gap: 10px;
      margin-bottom: 8px;
    }

    .option-header svg {
      flex-shrink: 0;
      opacity: 0.8;
      width: 24px;
      height: 24px;
      stroke-width: 2;
    }

    .option--primary .option-header svg {
      color: var(--primary-color);
      opacity: 1;
    }

    .option-description {
      margin: 0 0 16px 0;
      font-size: 14px;
      color: rgba(255, 255, 255, 0.7);
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
  `];

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
    const showDownload = !this.localDetected && !this.multiUserConfigured;
    const displayUrl = this.backendUrl || this.configuredUrl;

    return html`
      <div class="container">
        <div class="header">
          <h2>Connect to AD4M</h2>
          ${!this.localDetected
            ? html`
                <p>
                  No local AD4M detected.
                  ${!this.isMobile
                    ? "Download AD4M or connect to a remote executor."
                    : "Connect to a remote executor or scan a QR code."}
                </p>
              `
            : html`<p>Choose how you want to connect:</p>`}
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
                    <h3>Local AD4M</h3>
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
                    <h3>${displayUrl ? "Configured AD4M Node" : "Remote AD4M"}</h3>
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
