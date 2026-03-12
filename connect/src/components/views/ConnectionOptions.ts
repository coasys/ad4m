import { LitElement, html, css, PropertyValues } from "lit";
import { customElement, property, state } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";
import { DownloadIcon, LocalIcon, RefreshIcon, CheckIcon, GlobeIcon } from "../icons";
import CrossIcon from "../icons/CrossIcon";
import { connectWebSocket } from "../../utils";

@customElement("connection-options")
export class ConnectionOptions extends LitElement {
  @property({ type: Number }) port: number;
  @property({ type: Boolean }) showHosting: boolean = false;

  @state() private loading = true;
  @state() private localNodeDetected = false;
  @state() private newPort = 0;
  @state() private isMobile = false;

  static styles = [
    sharedStyles,
    css`
      .options {
        display: flex;
        flex-direction: column;
        gap: 30px;
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

  private async changePort() {
    this.dispatchEvent(new CustomEvent("change-port", { detail: { port: this.newPort }, bubbles: true, composed: true }));
  }

  private async connectLocalNode() {
    this.dispatchEvent(new CustomEvent("connect-local-node", { bubbles: true, composed: true }));
  }

  private browseHosts() {
    this.dispatchEvent(new CustomEvent("browse-hosts", { bubbles: true, composed: true }));
  }

  private async detectLocalNode() {
    try {
      await connectWebSocket(`ws://localhost:${this.newPort}/graphql`, 3000);
      this.localNodeDetected = true;
    } catch (error) {
      console.log("[Ad4m Connect] Local detection failed:", error);
      this.localNodeDetected = false;
    }
  }

  private refreshPort() {
    this.changePort();
    this.detectLocalNode();
  }

  private checkMobile = () => {
    this.isMobile = window.innerWidth < 800;
  };

  async connectedCallback() {
    super.connectedCallback();
    this.newPort = this.port;
    this.checkMobile();
    window.addEventListener('resize', this.checkMobile);
    await this.detectLocalNode();
    this.loading = false;
  }

  disconnectedCallback() {
    super.disconnectedCallback();
    window.removeEventListener('resize', this.checkMobile);
  }

  willUpdate(changedProps: PropertyValues) {
    if (changedProps.has('port')) this.newPort = this.port;
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
          ${!this.isMobile ? html`
            <div class="box">
              <div class="box-header">
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
                  .value=${this.newPort != null ? this.newPort.toString() : ''}
                  @input=${(e: Event) => {
                    const input = e.target as HTMLInputElement;
                    const next = Number.parseInt(input.value, 10);
                    if (Number.isFinite(next)) this.newPort = next;
                  }}
                  @keydown=${(e: KeyboardEvent) => {
                    if (e.key === 'Enter') this.refreshPort();
                  }}
                />
                <button class="primary" @click=${this.refreshPort}>
                  ${RefreshIcon()}
                </button>
              </div>
            </div>
          ` : ''}

          ${this.showHosting ? html`
            <div class="box">
              <div class="box-header">
                ${GlobeIcon()}
                <h3>Remote Hosts</h3>
              </div>

              <p>Browse hosted AD4M nodes or enter a URL</p>

              <button class="primary" @click=${this.browseHosts}>
                Browse Remote Hosts
              </button>
            </div>
          ` : '' }
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