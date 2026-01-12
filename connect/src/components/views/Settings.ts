import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";

@customElement("connection-settings")
export class ConnectionSettings extends LitElement {
  @property({ type: Number }) port = 12000;
  @property({ type: String }) url = "";
  @property({ type: Boolean }) isRemote = false;

  static styles = css`
    :host {
      display: block;
    }
  `;

  private handleModeChange(isRemote: boolean) {
    this.dispatchEvent(
      new CustomEvent("mode-change", {
        detail: { isRemote },
        bubbles: true,
        composed: true,
      })
    );
  }

  private handleUrlChange(e: Event) {
    const input = e.target as HTMLInputElement;
    this.dispatchEvent(
      new CustomEvent("url-change", {
        detail: { url: input.value },
        bubbles: true,
        composed: true,
      })
    );
  }

  private handlePortChange(e: Event) {
    const input = e.target as HTMLInputElement;
    this.dispatchEvent(
      new CustomEvent("port-change", {
        detail: { port: parseInt(input.value) },
        bubbles: true,
        composed: true,
      })
    );
  }

  private handleConnect() {
    this.dispatchEvent(new CustomEvent("connect", { bubbles: true, composed: true }));
  }

  private handleBack() {
    this.dispatchEvent(new CustomEvent("back", { bubbles: true, composed: true }));
  }

  private handleClearState() {
    this.dispatchEvent(new CustomEvent("clear-state", { bubbles: true, composed: true }));
  }

  render() {
    return html`
      <j-flex direction="column" gap="500">
        <j-flex direction="column" gap="300" a="center">
          <j-text variant="heading-lg">Connection Settings</j-text>
          <j-text variant="body">Please choose how you want to connect to AD4M</j-text>
        </j-flex>

        <j-flex gap="300">
          <j-button
            variant="${this.isRemote ? "secondary" : "primary"}"
            full
            @click=${() => this.handleModeChange(false)}
          >
            Locally
          </j-button>
          <j-button
            variant="${this.isRemote ? "primary" : "secondary"}"
            full
            @click=${() => this.handleModeChange(true)}
          >
            Remotely
          </j-button>
        </j-flex>

        ${this.isRemote
          ? html`
              <j-box>
                <j-text variant="label" nomargin>URL</j-text>
                <j-input
                  .value=${this.url}
                  @input=${this.handleUrlChange}
                  size="lg"
                ></j-input>
              </j-box>
              <j-button variant="primary" size="lg" full @click=${this.handleConnect}>
                Connect
              </j-button>
            `
          : html`
              <j-box>
                <j-text variant="label" nomargin>PORT</j-text>
                <j-input
                  type="number"
                  .value=${this.port.toString()}
                  @input=${this.handlePortChange}
                  size="lg"
                ></j-input>
              </j-box>
              <j-button variant="primary" size="lg" full @click=${this.handleConnect}>
                Connect
              </j-button>
            `}

        <j-text variant="body" a="center">or</j-text>

        <j-flex gap="300">
          <j-button variant="secondary" size="lg" full @click=${this.handleBack}>
            Back
          </j-button>
          <j-button variant="secondary" size="lg" full @click=${this.handleClearState}>
            Clear state
          </j-button>
        </j-flex>
      </j-flex>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "connection-settings": ConnectionSettings;
  }
}
