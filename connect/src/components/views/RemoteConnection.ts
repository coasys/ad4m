import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";

@customElement("remote-connection")
export class RemoteConnection extends LitElement {
  @property({ type: String }) initialUrl?: string;
  @property({ type: Boolean }) detecting = false;
  @property({ type: Boolean }) multiUserDetected: boolean | null = null;
  @property({ type: String }) error: string | null = null;

  static styles = [
    sharedStyles,
    css`

    .header {
      display: flex;
      flex-direction: column;
      gap: 12px;
      text-align: center;
    }

    .button-row {
      justify-content: flex-end;
    }

    button.full {
      width: 100%;
    }

    .remote-info {
      background: rgba(145, 227, 253, 0.05);
      border: 1px solid rgba(145, 227, 253, 0.2);
      border-radius: 8px;
      padding: 15px;
    }

    .remote-info p {
      font-size: 12px;
      margin-bottom: 5px;
    }

    .remote-url {
      font-family: monospace;
      font-size: 12px;
      opacity: 0.8;
      margin-top: 5px;
    }

    .auth-options {
      display: flex;
      flex-direction: column;
      gap: 16px;
    }

    .auth-option {
      background: rgba(145, 227, 253, 0.05);
      border: 1px solid rgba(145, 227, 253, 0.2);
      border-radius: 8px;
      padding: 20px;
      transition: all 0.3s ease;
    }

    .auth-option:hover {
      border-color: rgba(145, 227, 253, 0.4);
      background: rgba(145, 227, 253, 0.08);
    }

    .auth-option--primary {
      border: 2px solid #91e3fd;
      background: rgba(145, 227, 253, 0.1);
    }

    .auth-option--primary:hover {
      background: rgba(145, 227, 253, 0.15);
    }

    .auth-option__header {
      display: flex;
      align-items: center;
      gap: 10px;
      margin-bottom: 8px;
    }

    .auth-option__header h3 {
      font-size: 16px;
      font-weight: 600;
      margin: 0;
      color: #ffffff;
    }

    .auth-option__header svg {
      flex-shrink: 0;
      opacity: 0.8;
      width: 20px;
      height: 20px;
      stroke-width: 2;
    }

    .auth-option--primary .auth-option__header svg {
      color: #91e3fd;
      opacity: 1;
    }

    .auth-option-description {
      margin: 0 0 16px 0;
      font-size: 14px;
      color: rgba(255, 255, 255, 0.7);
    }
  `];

  private handleBack() {
    this.dispatchEvent(new CustomEvent("back", { bubbles: true, composed: true }));
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

  private handleConnect() {
    this.dispatchEvent(new CustomEvent("connect", { bubbles: true, composed: true }));
  }

  private handleMultiUserAuth() {
    this.dispatchEvent(new CustomEvent("multi-user-auth", { bubbles: true, composed: true }));
  }

  private handleRequestCapability() {
    this.dispatchEvent(new CustomEvent("request-capability", { bubbles: true, composed: true }));
  }

  render() {
    const showAuthOptions = this.multiUserDetected !== null && !this.detecting;
    const urlValue = this.initialUrl || "";

    return html`
      <div class="container">
        <div class="header">
          <h2>Connect to Remote AD4M</h2>
          <p>Enter the URL of a remote AD4M executor</p>
        </div>

        ${!showAuthOptions
          ? html`
              <div class="container">
                <div class="form-group">
                  <label>Executor URL</label>
                  <input
                    type="text"
                    placeholder="wss://your-server.com/graphql"
                    .value=${urlValue}
                    @input=${this.handleUrlChange}
                    ?disabled=${this.detecting}
                  />
                </div>

                ${this.error
                  ? html`
                      <div class="error-box">
                        ${this.error}
                      </div>
                    `
                  : ""}

                <div class="button-row">
                  <button
                    class="ghost"
                    @click=${this.handleBack}
                    ?disabled=${this.detecting}
                  >
                    Back
                  </button>
                  <button
                    class="primary"
                    @click=${this.handleConnect}
                    ?disabled=${this.detecting || !urlValue}
                  >
                    ${this.detecting ? "Detecting..." : "Connect"}
                  </button>
                </div>
              </div>
            `
          : html`
              <div class="container">
                <div class="remote-info">
                  <p>Connected to:</p>
                  <div class="remote-url">${urlValue}</div>
                </div>

                <div class="auth-options">
                  ${this.multiUserDetected
                    ? html`
                        <div class="auth-option auth-option--primary">
                          <div class="auth-option__header">
                            <svg viewBox="0 0 24 24" fill="none" stroke="currentColor">
                              <path d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"></path>
                              <circle cx="9" cy="7" r="4"></circle>
                              <path d="M23 21v-2a4 4 0 0 0-3-3.87"></path>
                              <path d="M16 3.13a4 4 0 0 1 0 7.75"></path>
                            </svg>
                            <h3>Multi-User Login</h3>
                          </div>
                          <p class="auth-option-description">
                            This executor supports multi-user authentication. Sign in or create
                            an account.
                          </p>
                          <button
                            class="primary full"
                            @click=${this.handleMultiUserAuth}
                          >
                            Login / Sign Up
                          </button>
                        </div>

                        <div class="auth-option">
                          <div class="auth-option__header">
                            <svg viewBox="0 0 24 24" fill="none" stroke="currentColor">
                              <rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect>
                              <path d="M7 11V7a5 5 0 0 1 10 0v4"></path>
                            </svg>
                            <h3>Capability Request</h3>
                          </div>
                          <p class="auth-option-description">
                            Request access to the main agent with a verification code.
                          </p>
                          <button
                            class="secondary full"
                            @click=${this.handleRequestCapability}
                          >
                            Request Capability
                          </button>
                        </div>
                      `
                    : html`
                        <div class="auth-option auth-option--primary">
                          <div class="auth-option__header">
                            <svg viewBox="0 0 24 24" fill="none" stroke="currentColor">
                              <rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect>
                              <path d="M7 11V7a5 5 0 0 1 10 0v4"></path>
                            </svg>
                            <h3>Single-User Executor</h3>
                          </div>
                          <p class="auth-option-description">
                            This executor uses capability-based authentication. Request access
                            with a verification code.
                          </p>
                          <button
                            class="primary full"
                            @click=${this.handleRequestCapability}
                          >
                            Request Capability
                          </button>
                        </div>
                      `}
                </div>

                <div class="center">
                  <button class="link" @click=${this.handleBack}>
                    ← Back to connection options
                  </button>
                </div>
              </div>
            `}
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "remote-connection": RemoteConnection;
  }
}
