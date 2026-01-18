import { LitElement, html, css, PropertyValues } from "lit";
import { customElement, property, state } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";
import { DownloadIcon, LocalIcon, RefreshIcon, RemoteIcon, CheckIcon } from "../icons";
import CrossIcon from "../icons/CrossIcon";
import { connectWebSocket } from "../../utils";

@customElement("connection-options")
export class ConnectionOptions extends LitElement {
  @property({ type: String }) port: number;
  @property({ type: String }) url?: string;

  @state() private loading = true;
  @state() private localNodeDetected = false;
  @state() private newPort = 0;
  @state() private newUrl = "";

  static styles = [
    sharedStyles,
    css`
      .options {
        display: flex;
        flex-direction: column;
        gap: 30px;
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

      .option-header svg {
        width: 24px;
        height: 24px;
        stroke-width: 2;
        flex-shrink: 0;
        color: var(--ac-primary-color);
      }

      .port-input {
        display: flex;
        justify-content: center;
        gap: 8px;
        width: 100%;
      }

      .port-input button {
        width: auto;
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

  private async detectLocalNode() {
    try {
      await connectWebSocket(`ws://localhost:${this.newPort}/graphql`, 3000);
      this.localNodeDetected = true;
    } catch (error) {
      console.log("[Ad4m Connect] Local detection failed:", error);
      this.localNodeDetected = false;
    }
  }

  private async changePort() {
    this.dispatchEvent(new CustomEvent("change-port", { detail: { port: this.newPort }, bubbles: true, composed: true }));
  }

  private async changeUrl() {
    this.dispatchEvent(new CustomEvent("change-url", { detail: { url: this.newUrl }, bubbles: true, composed: true }));
  }

  private async connectLocalNode() {
    this.dispatchEvent(new CustomEvent("connect-local-node", { bubbles: true, composed: true }));
  }

  private async connectRemoteNode() {
    this.dispatchEvent(new CustomEvent("connect-remote-node", { bubbles: true, composed: true }));
  }

  async connectedCallback() {
    super.connectedCallback();
    this.newPort = this.port;
    this.newUrl = this.url || "";
    await this.detectLocalNode();
    this.loading = false;
  }

  willUpdate(changedProps: PropertyValues) {
    if (changedProps.has('port')) this.newPort = this.port;
    if (changedProps.has('url')) this.newUrl = this.url || "";
  }

  render() {
    if (this.loading) return null;

    return html`
      <div class="container">
        <div class="header">
          <h1>Connect AD4M</h1>
          <h3>How would you like to connect?</h3>
        </div>

        <div class="options">
          <div class="box">
            <div class="option-header">
              ${LocalIcon()}
              <h3>Local Node</h3>
            </div>

            ${this.localNodeDetected
              ? html`
                  <div class="state success">
                    ${CheckIcon()}
                    <p>Local node detected on port ${this.port}</p>
                  </div>

                  <button class="primary" @click=${this.connectLocalNode}>
                    Connect to Local Node
                  </button>
                `
              : html`
                  <div class="state danger">
                    ${CrossIcon()}
                    <p>No local node detected on port ${this.port}</p>
                  </div>

                  <p style="margin-bottom: -12px">Download and install AD4M</p>
                  <button class="secondary" @click=${() => window.open("https://github.com/coasys/ad4m/releases")}>
                    ${DownloadIcon()} Download AD4M
                  </button>
                `
            }

            <p style="margin-bottom: -12px">Or try another port</p>
            <div class="port-input">
              <input
                type="number"
                placeholder="Port number..."
                .value=${this.newPort.toString()}
                @input=${(e: Event) => {
                  const input = e.target as HTMLInputElement;
                  this.newPort = parseInt(input.value);
                }}
              />
              <button class="primary" @click=${() => { this.changePort(); this.detectLocalNode(); }}>
                ${RefreshIcon()}
              </button>
            </div>
          </div>

          <div class="box">
            <div class="option-header">
              ${RemoteIcon()}
              <h3>Remote Node</h3>
            </div>

            ${this.url
              ? html`
                  <div class="state success">
                    ${CheckIcon()}
                    <p>Remote configuration detected</p>
                  </div>
                `
              : html`<p style="margin-bottom: -12px">Enter the URL of a remote AD4M node</p>`
            }

            <input
              type="text"
              placeholder="https://ad4m-node:12000/graphql"
              .value=${this.newUrl}
              @input=${(e: Event) => {
                const input = e.target as HTMLInputElement;
                this.newUrl = input.value;
              }}
              style= "font-size: 16px;"
            />

            <button 
              class="primary" 
              @click=${() => { this.changeUrl(); this.connectRemoteNode(); }}
            >
              Connect to Remote Node
            </button>
          </div>
        </div>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "connection-options": ConnectionOptions;
  }
}

// <p style="margin-bottom: -12px">If you have AD4M installed, boot it up and retry</p>
// <button class="primary" @click=${this.detectLocalNode}>
//   ${RefreshIcon()} Retry Connection
// </button>

// import QRCodeIcon from "../icons/QRCodeIcon";
// ${showQRCode
//   ? html`
//       <div class="option">
//         <div class="option-header">
//           ${QRCodeIcon()}
//           <h3>Scan QR Code</h3>
//         </div>
//         <p class="option-description">
//           Scan a QR code from another device
//         </p>
//         <button class="secondary" @click=${this.handleScanQR}>
//           Scan QR Code
//         </button>
//       </div>
//     `
//   : ""
// }