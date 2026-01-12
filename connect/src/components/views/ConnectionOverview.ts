import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";

@customElement("connection-overview")
export class ConnectionOverview extends LitElement {
  @property({ type: Boolean }) localDetected = false;
  @property({ type: Boolean }) multiUserConfigured = false;
  @property({ type: String }) backendUrl?: string;
  @property({ type: String }) configuredUrl?: string;
  @property({ type: Boolean }) isMobile = false;

  static styles = css`
    :host {
      display: block;
    }

    .container {
      display: flex;
      flex-direction: column;
      gap: 24px;
    }

    .header {
      text-align: center;
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
      border: 2px solid var(--gradient, #91e3fd);
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
      margin-bottom: 5px;
    }

    .option-header svg {
      flex-shrink: 0;
      opacity: 0.8;
      width: 24px;
      height: 24px;
    }

    .option--primary .option-header svg {
      color: var(--gradient, #91e3fd);
      opacity: 1;
    }

    .url-display {
      margin: 5px 0 15px 0;
      font-size: 12px;
      opacity: 0.7;
      font-family: monospace;
      word-break: break-all;
    }
  `;

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
      <j-flex direction="column" gap="600">
        <j-flex direction="column" gap="300" a="center">
          <j-text variant="heading-lg">Connect to AD4M</j-text>
          ${!this.localDetected
            ? html`
                <j-text variant="body" color="ui-500">
                  No local AD4M detected.
                  ${!this.isMobile
                    ? "Download AD4M or connect to a remote executor."
                    : "Connect to a remote executor or scan a QR code."}
                </j-text>
              `
            : html`
                <j-text variant="body">Choose how you want to connect:</j-text>
              `}
        </j-flex>

        <j-flex direction="column" gap="400">
          ${showLocalOption
            ? html`
                <div class="option option--primary">
                  <div class="option-header">
                    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor">
                      <circle cx="12" cy="12" r="10"></circle>
                      <circle cx="12" cy="12" r="3" fill="currentColor"></circle>
                    </svg>
                    <j-text variant="heading">Local AD4M</j-text>
                  </div>
                  <j-text variant="body-sm" nomargin>
                    Connect to your local executor on port 12000
                  </j-text>
                  <j-box mt="400">
                    <j-button
                      variant="primary"
                      size="lg"
                      full
                      @click=${this.handleConnectLocal}
                    >
                      Connect to Local AD4M
                    </j-button>
                  </j-box>
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
                      <path
                        d="M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z"
                      ></path>
                    </svg>
                    <j-text variant="heading">
                      ${displayUrl ? "Configured AD4M Node" : "Remote AD4M"}
                    </j-text>
                  </div>
                  ${displayUrl
                    ? html`
                        <j-text variant="body-sm" nomargin>
                          ${this.multiUserConfigured
                            ? "Multi-user backend configured"
                            : "Connect to AD4M node at URL"}
                        </j-text>
                        <div class="url-display">${displayUrl}</div>
                      `
                    : html`
                        <j-text variant="body-sm" nomargin>
                          Connect to a remote executor or multi-user backend
                        </j-text>
                      `}
                  <j-box mt="400">
                    <j-button
                      variant="${showLocalOption ? "secondary" : "primary"}"
                      size="lg"
                      full
                      @click=${this.handleConnectRemote}
                    >
                      ${displayUrl ? "Connect to Configured Node" : "Enter Remote URL"}
                    </j-button>
                  </j-box>
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
                    <j-text variant="heading">Scan QR Code</j-text>
                  </div>
                  <j-text variant="body-sm" nomargin>
                    Scan a QR code from another device
                  </j-text>
                  <j-box mt="400">
                    <j-button
                      variant="secondary"
                      size="lg"
                      full
                      @click=${this.handleScanQR}
                    >
                      Scan QR Code
                    </j-button>
                  </j-box>
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
                    <j-text variant="heading">Get AD4M</j-text>
                  </div>
                  <j-text variant="body-sm" nomargin>
                    Download and install AD4M on your device
                  </j-text>
                  <j-box mt="400">
                    <j-button
                      variant="primary"
                      size="lg"
                      full
                      @click=${this.handleDownload}
                    >
                      Download AD4M
                    </j-button>
                  </j-box>
                </div>
              `
            : ""}
        </j-flex>
      </j-flex>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "connection-overview": ConnectionOverview;
  }
}
