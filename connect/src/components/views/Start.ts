import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";
import { getForVersion, DEFAULT_PORT } from "../../utils";
import { sharedStyles } from "../../styles/shared-styles";

@customElement("start-view")
export class StartView extends LitElement {
  @property({ type: Boolean }) isMobile = false;
  @property({ type: Boolean }) hasClickedDownload = false;
  @property({ type: Boolean }) hosting = false;

  static styles = [
    sharedStyles,
    css`
    .section {
      display: flex;
      flex-direction: column;
      gap: 16px;
      align-items: center;
    }

    h1 {
      text-align: center;
    }

    .info-text {
      color: #b9b9b9;
      font-size: 15px;
      text-align: center;
      line-height: 1.6;
      padding: 0 10px;
    }

    a {
      text-decoration: none;
    }
  `];

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
      <div class="container">
        ${!this.hasClickedDownload
          ? html`
              <div class="section">
                <h1>Could not connect to AD4M</h1>
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
                      <button class="primary" @click=${this.handleScanQR}>
                        Connect with QR
                      </button>
                    `
                  : html`
                      <a
                        href="https://github.com/coasys/ad4m/releases"
                        @click=${this.handleDownloadClick}
                      >
                        <button class="primary">Download AD4M</button>
                      </a>
                    `}

                ${this.hosting
                  ? html`
                      <p>Or</p>
                      <button class="primary" @click=${this.handleHosting}>
                        Use hosted AD4M (alpha)
                      </button>
                    `
                  : ""}
              </div>

              <div class="section">
                <p>or</p>
                <button class="secondary" @click=${this.handleConnect}>
                  Try reconnecting
                </button>
                <p>or</p>
                <button class="secondary" @click=${this.handleSettings}>
                  Change connection settings
                </button>
              </div>
            `
          : html`
              <div class="section">
                <button class="primary" @click=${this.handleConnect}>
                  Connect to ADAM
                </button>
                <p>
                  Please connect to ADAM once you have downloaded and setup your ADAM agent
                </p>
              </div>
            `}

        <div class="center">
          <a
            href="https://ad4m.dev"
            target="_blank"
          >
            <button class="secondary">Learn more about ADAM</button>
          </a>
        </div>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "start-view": StartView;
  }
}
