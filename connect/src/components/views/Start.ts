import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";
import { getForVersion, DEFAULT_PORT } from "../../utils";

@customElement("start-view")
export class StartView extends LitElement {
  @property({ type: Boolean }) isMobile = false;
  @property({ type: Boolean }) hasClickedDownload = false;
  @property({ type: Boolean }) hosting = false;

  static styles = css`
    :host {
      display: block;
    }

    .info-text {
      color: #b9b9b9;
      font-size: 15px;
      text-align: center;
      margin: 30px 0;
      line-height: 1.6;
    }
  `;

  private handleConnect() {
    this.dispatchEvent(new CustomEvent("connect", { bubbles: true, composed: true }));
  }

  private handleSettings() {
    this.dispatchEvent(new CustomEvent("settings", { bubbles: true, composed: true }));
  }

  private handleScanQR() {
    this.dispatchEvent(new CustomEvent("scan-qr", { bubbles: true, composed: true }));
  }

  private handleHosting() {
    this.dispatchEvent(new CustomEvent("hosting", { bubbles: true, composed: true }));
  }

  private handleDownloaded() {
    this.dispatchEvent(new CustomEvent("downloaded", { bubbles: true, composed: true }));
  }

  private handleDownloadClick(e: Event) {
    e.preventDefault();
    const el = e.currentTarget as HTMLLinkElement;
    window.open(el.href, "_blank");
    this.handleDownloaded();
  }

  render() {
    const url = getForVersion("ad4murl") || `http://localhost:${DEFAULT_PORT}`;
    const isLocal = url.includes("localhost");

    return html`
      <j-flex direction="column" gap="600">
        ${!this.hasClickedDownload
          ? html`
              <j-flex direction="column" gap="400" a="center">
                <j-text variant="heading-lg">Could not connect to AD4M</j-text>
                ${isLocal
                  ? html`
                      <div class="info-text">
                        Please make sure you have the ADAM Launcher running on your computer
                        and there are no browser restrictions ("Shields") blocking your
                        connection to ADAM on localhost.
                        <br /><br />
                        (Safari users: please use a different browser for now. Safari is very
                        strict about this and we are working on a solution.)
                      </div>
                    `
                  : html`
                      <div class="info-text">
                        Looks like the remote executor you are trying to connect to is not
                        reachable.
                        <br /><br />
                        Please try connecting to a different remote executor or check your
                        connection settings.
                      </div>
                    `}

                ${this.isMobile
                  ? html`
                      <j-button variant="primary" size="lg" @click=${this.handleScanQR}>
                        Connect with QR
                      </j-button>
                    `
                  : html`
                      <a
                        href="https://github.com/coasys/ad4m/releases"
                        @click=${this.handleDownloadClick}
                        style="text-decoration: none;"
                      >
                        <j-button variant="primary" size="lg">Download AD4M</j-button>
                      </a>
                    `}

                ${this.hosting
                  ? html`
                      <j-text variant="body">Or</j-text>
                      <j-button variant="primary" size="lg" @click=${this.handleHosting}>
                        Use hosted AD4M (alpha)
                      </j-button>
                    `
                  : ""}
              </j-flex>

              <j-flex direction="column" gap="400" a="center">
                <j-text variant="body">or</j-text>
                <j-button variant="secondary" size="lg" @click=${this.handleConnect}>
                  Try reconnecting
                </j-button>
                <j-text variant="body">or</j-text>
                <j-button variant="secondary" size="lg" @click=${this.handleSettings}>
                  Change connection settings
                </j-button>
              </j-flex>
            `
          : html`
              <j-flex direction="column" gap="400" a="center">
                <j-button variant="primary" size="lg" @click=${this.handleConnect}>
                  Connect to ADAM
                </j-button>
                <j-text variant="body">
                  Please connect to ADAM once you have downloaded and setup your ADAM agent
                </j-text>
              </j-flex>
            `}

        <j-flex j="center">
          <a
            href="https://ad4m.dev"
            target="_blank"
            style="text-decoration: none;"
          >
            <j-button variant="secondary">Learn more about ADAM</j-button>
          </a>
        </j-flex>
      </j-flex>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "start-view": StartView;
  }
}
