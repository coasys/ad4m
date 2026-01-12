import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";

@customElement("connection-settings")
export class ConnectionSettings extends LitElement {
  @property({ type: Number }) port = 12000;
  @property({ type: String }) url = "";
  @property({ type: Boolean }) isRemote = false;

  static styles = [
    sharedStyles,
    css`
    .container {
      gap: 20px;
    }

    .header {
      display: flex;
      flex-direction: column;
      gap: 12px;
      align-items: center;
      margin-bottom: 4px;
    }

    p {
      text-align: center;
    }

    .mode-buttons {
      display: flex;
      gap: 12px;
    }

    input {
      box-sizing: border-box;
    }

    .divider {
      text-align: center;
    }
  `];

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
      <div class="container">
        <div class="header">
          <h1>Connection Settings</h1>
          <p>Please choose how you want to connect to AD4M</p>
        </div>

        <div class="mode-buttons">
          <button
            class="${this.isRemote ? "secondary" : "primary"} full"
            @click=${() => this.handleModeChange(false)}
          >
            Locally
          </button>
          <button
            class="${this.isRemote ? "primary" : "secondary"} full"
            @click=${() => this.handleModeChange(true)}
          >
            Remotely
          </button>
        </div>

        ${this.isRemote
          ? html`
              <div class="form-group">
                <label>URL</label>
                <input
                  type="text"
                  placeholder="Enter remote URL"
                  .value=${this.url}
                  @input=${this.handleUrlChange}
                />
              </div>
              <button class="primary full" @click=${this.handleConnect}>
                Connect
              </button>
            `
          : html`
              <div class="form-group">
                <label>PORT</label>
                <input
                  type="number"
                  placeholder="12000"
                  .value=${this.port.toString()}
                  @input=${this.handlePortChange}
                />
              </div>
              <button class="primary full" @click=${this.handleConnect}>
                Connect
              </button>
            `}

        <p class="divider">or</p>

        <div class="button-row">
          <button class="secondary full" @click=${this.handleBack}>
            Back
          </button>
          <button class="secondary full" @click=${this.handleClearState}>
            Clear state
          </button>
        </div>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "connection-settings": ConnectionSettings;
  }
}
