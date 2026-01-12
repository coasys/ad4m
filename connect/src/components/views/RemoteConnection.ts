import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";

@customElement("remote-connection")
export class RemoteConnection extends LitElement {
  @property({ type: String }) initialUrl?: string;
  @property({ type: Boolean }) detecting = false;
  @property({ type: Boolean }) multiUserDetected: boolean | null = null;
  @property({ type: String }) error: string | null = null;

  static styles = css`
    :host {
      display: block;
    }

    .remote-info {
      background: rgba(145, 227, 253, 0.05);
      border: 1px solid rgba(145, 227, 253, 0.2);
      border-radius: 8px;
      padding: 15px;
    }

    .remote-url {
      font-family: monospace;
      font-size: 12px;
      opacity: 0.8;
      margin-top: 5px;
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
      border: 2px solid var(--gradient, #91e3fd);
      background: rgba(145, 227, 253, 0.1);
    }

    .auth-option--primary:hover {
      background: rgba(145, 227, 253, 0.15);
    }

    .auth-option__header {
      display: flex;
      align-items: center;
      gap: 10px;
      margin-bottom: 5px;
    }

    .auth-option__header svg {
      flex-shrink: 0;
      opacity: 0.8;
      width: 20px;
      height: 20px;
    }

    .auth-option--primary .auth-option__header svg {
      color: var(--gradient, #91e3fd);
      opacity: 1;
    }
  `;

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
      <j-flex direction="column" gap="600">
        <j-flex direction="column" gap="300" a="center">
          <j-text variant="heading-lg">Connect to Remote AD4M</j-text>
          <j-text variant="body">Enter the URL of a remote AD4M executor</j-text>
        </j-flex>

        ${!showAuthOptions
          ? html`
              <j-flex direction="column" gap="400">
                <j-box>
                  <j-text variant="label" nomargin>Executor URL</j-text>
                  <j-input
                    placeholder="wss://your-server.com/graphql"
                    .value=${urlValue}
                    @input=${this.handleUrlChange}
                    ?disabled=${this.detecting}
                    size="lg"
                  ></j-input>
                </j-box>

                ${this.error
                  ? html`
                      <j-box p="400" bg="danger-100" border="1" radius="200">
                        <j-text variant="body" color="danger-700">${this.error}</j-text>
                      </j-box>
                    `
                  : ""}

                <j-flex gap="300" j="end">
                  <j-button
                    variant="ghost"
                    @click=${this.handleBack}
                    ?disabled=${this.detecting}
                  >
                    Back
                  </j-button>
                  <j-button
                    variant="primary"
                    @click=${this.handleConnect}
                    ?disabled=${this.detecting || !urlValue}
                  >
                    ${this.detecting ? "Detecting..." : "Connect"}
                  </j-button>
                </j-flex>
              </j-flex>
            `
          : html`
              <j-flex direction="column" gap="400">
                <div class="remote-info">
                  <j-text variant="body-sm" nomargin>Connected to:</j-text>
                  <div class="remote-url">${urlValue}</div>
                </div>

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
                          <j-text variant="heading-sm">Multi-User Login</j-text>
                        </div>
                        <j-text variant="body-sm" nomargin>
                          This executor supports multi-user authentication. Sign in or create
                          an account.
                        </j-text>
                        <j-box mt="400">
                          <j-button
                            variant="primary"
                            size="lg"
                            full
                            @click=${this.handleMultiUserAuth}
                          >
                            Login / Sign Up
                          </j-button>
                        </j-box>
                      </div>

                      <div class="auth-option">
                        <div class="auth-option__header">
                          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor">
                            <rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect>
                            <path d="M7 11V7a5 5 0 0 1 10 0v4"></path>
                          </svg>
                          <j-text variant="heading-sm">Capability Request</j-text>
                        </div>
                        <j-text variant="body-sm" nomargin>
                          Request access to the main agent with a verification code.
                        </j-text>
                        <j-box mt="400">
                          <j-button
                            variant="secondary"
                            size="lg"
                            full
                            @click=${this.handleRequestCapability}
                          >
                            Request Capability
                          </j-button>
                        </j-box>
                      </div>
                    `
                  : html`
                      <div class="auth-option auth-option--primary">
                        <div class="auth-option__header">
                          <svg viewBox="0 0 24 24" fill="none" stroke="currentColor">
                            <rect x="3" y="11" width="18" height="11" rx="2" ry="2"></rect>
                            <path d="M7 11V7a5 5 0 0 1 10 0v4"></path>
                          </svg>
                          <j-text variant="heading-sm">Single-User Executor</j-text>
                        </div>
                        <j-text variant="body-sm" nomargin>
                          This executor uses capability-based authentication. Request access
                          with a verification code.
                        </j-text>
                        <j-box mt="400">
                          <j-button
                            variant="primary"
                            size="lg"
                            full
                            @click=${this.handleRequestCapability}
                          >
                            Request Capability
                          </j-button>
                        </j-box>
                      </div>
                    `}

                <j-flex j="center">
                  <j-button variant="link" @click=${this.handleBack}>
                    ← Back to connection options
                  </j-button>
                </j-flex>
              </j-flex>
            `}
      </j-flex>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "remote-connection": RemoteConnection;
  }
}
