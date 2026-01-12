import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";
import { capSentence } from "@coasys/ad4m";

@customElement("request-capability")
export class RequestCapability extends LitElement {
  @property({ type: Array }) capabilities: string[] = [];
  @property({ type: String }) appname = "";
  @property({ type: String }) appiconpath = "";

  static styles = css`
    :host {
      display: block;
    }

    .dialog__connect {
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 15px;
      padding: 20px 0;
    }

    .dialog__connect-app,
    .dialog__connect-ad4m {
      width: 60px;
      height: 60px;
      border-radius: 12px;
      object-fit: cover;
    }

    .dialog__connect-check {
      width: 24px;
      height: 24px;
      background: var(--success-color, #10b981);
      border-radius: 50%;
      position: relative;
    }

    .dialog__connect-check::after {
      content: "✓";
      color: white;
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
    }

    .check-list {
      list-style: none;
      padding: 0;
      margin: 20px 0;
    }

    .check-list li {
      display: flex;
      align-items: center;
      gap: 10px;
      padding: 8px 0;
    }

    .check-list svg {
      flex-shrink: 0;
    }
  `;

  private handleCancel() {
    this.dispatchEvent(new CustomEvent("cancel", { bubbles: true, composed: true }));
  }

  private handleAuthorize() {
    this.dispatchEvent(new CustomEvent("authorize", { bubbles: true, composed: true }));
  }

  private handleSettings() {
    this.dispatchEvent(new CustomEvent("settings", { bubbles: true, composed: true }));
  }

  render() {
    return html`
      <j-flex direction="column" gap="600">
        <j-flex direction="column" gap="200" a="center">
          <j-text variant="body">An external application</j-text>
          <j-text variant="heading-lg" nomargin>${this.appname}</j-text>
          <j-text variant="body">wants to access your AD4M data</j-text>
        </j-flex>

        ${this.appiconpath
          ? html`
              <div class="dialog__connect">
                <img class="dialog__connect-app" src=${this.appiconpath} alt="App Logo" />
                <div class="dialog__connect-check"></div>
                <div class="dialog__connect-ad4m">
                  <svg viewBox="0 0 100 100" width="60" height="60">
                    <circle cx="50" cy="50" r="45" fill="var(--j-color-primary-500)" />
                    <text
                      x="50"
                      y="50"
                      text-anchor="middle"
                      dy=".35em"
                      fill="white"
                      font-size="40"
                      font-weight="bold"
                    >
                      A
                    </text>
                  </svg>
                </div>
              </div>
            `
          : ""}

        <j-box>
          <j-text variant="body" weight="600" a="center">
            This will allow the developer to:
          </j-text>
          <ul class="check-list">
            ${this.capabilities.map(
              (cap) => html`
                <li>
                  <svg
                    xmlns="http://www.w3.org/2000/svg"
                    width="20"
                    height="20"
                    fill="var(--j-color-success-500)"
                    viewBox="0 0 16 16"
                  >
                    <path
                      d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zm-3.97-3.03a.75.75 0 0 0-1.08.022L7.477 9.417 5.384 7.323a.75.75 0 0 0-1.06 1.06L6.97 11.03a.75.75 0 0 0 1.079-.02l3.992-4.99a.75.75 0 0 0-.01-1.05z"
                    />
                  </svg>
                  <j-text variant="body">${capSentence(cap)}</j-text>
                </li>
              `
            )}
          </ul>
        </j-box>

        <j-flex gap="300">
          <j-button variant="secondary" size="lg" full @click=${this.handleCancel}>
            Cancel
          </j-button>
          <j-button variant="primary" size="lg" full @click=${this.handleAuthorize}>
            Authorize
          </j-button>
        </j-flex>

        <j-button variant="link" full @click=${this.handleSettings}>
          Connection settings
        </j-button>
      </j-flex>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "request-capability": RequestCapability;
  }
}
