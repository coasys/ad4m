import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";
import { capSentence } from "@coasys/ad4m";
import { sharedStyles } from "../../styles/shared-styles";

@customElement("request-capability")
export class RequestCapability extends LitElement {
  @property({ type: Array }) capabilities: string[] = [];
  @property({ type: String }) appname = "";
  @property({ type: String }) appiconpath = "";

  static styles = [
    sharedStyles,
    css`
    .header {
      display: flex;
      flex-direction: column;
      gap: 8px;
      align-items: center;
    }

    .header p {
      margin: 0;
      font-size: 14px;
      color: rgba(255, 255, 255, 0.8);
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

    .permissions-box {
      background: rgba(145, 227, 253, 0.05);
      border: 1px solid rgba(145, 227, 253, 0.2);
      border-radius: 8px;
      padding: 20px;
    }

    .permissions-title {
      font-weight: 600;
      text-align: center;
      margin-bottom: 16px;
    }

    .check-list {
      list-style: none;
      padding: 0;
      margin: 0;
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

    button.full {
      flex: 1;
    }
  `];

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
      <div class="container">
        <div class="header">
          <p>An external application</p>
          <h1>${this.appname}</h1>
          <p>wants to access your AD4M data</p>
        </div>

        ${this.appiconpath
          ? html`
              <div class="dialog__connect">
                <img class="dialog__connect-app" src=${this.appiconpath} alt="App Logo" />
                <div class="dialog__connect-check"></div>
                <div class="dialog__connect-ad4m">
                  <svg viewBox="0 0 100 100" width="60" height="60">
                    <circle cx="50" cy="50" r="45" fill="var(--ac-primary-color)" />
                    <text
                      x="50"
                      y="50"
                      text-anchor="middle"
                      dy=".35em"
                      fill="black"
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

        <div class="permissions-box">
          <p class="permissions-title">
            This will allow the developer to:
          </p>
          <ul class="check-list">
            ${this.capabilities.map(
              (cap) => html`
                <li>
                  <svg
                    xmlns="http://www.w3.org/2000/svg"
                    width="20"
                    height="20"
                    fill="#10b981"
                    viewBox="0 0 16 16"
                  >
                    <path
                      d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zm-3.97-3.03a.75.75 0 0 0-1.08.022L7.477 9.417 5.384 7.323a.75.75 0 0 0-1.06 1.06L6.97 11.03a.75.75 0 0 0 1.079-.02l3.992-4.99a.75.75 0 0 0-.01-1.05z"
                    />
                  </svg>
                  <p>${capSentence(cap)}</p>
                </li>
              `
            )}
          </ul>
        </div>

        <div class="button-row">
          <button class="secondary full" @click=${this.handleCancel}>
            Cancel
          </button>
          <button class="primary full" @click=${this.handleAuthorize}>
            Authorize
          </button>
        </div>

        <div class="center">
          <button class="link" @click=${this.handleSettings}>
            Connection settings
          </button>
        </div>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "request-capability": RequestCapability;
  }
}
