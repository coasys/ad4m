import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";

@customElement("hosting-view")
export class HostingView extends LitElement {
  @property({ type: Number }) step = 0;
  @property({ type: String }) email = "";
  @property({ type: String }) password = "";
  @property({ type: String }) passwordError = "";
  @property({ type: Boolean }) isHostingRunning: boolean | null = null;

  static styles = css`
    :host {
      display: block;
    }

    .error {
      color: var(--j-color-danger-500, #ef4444);
      font-size: 14px;
      margin-top: 8px;
    }

    a {
      color: var(--j-color-primary-500);
      text-decoration: underline;
    }
  `;

  private handleBack() {
    this.dispatchEvent(new CustomEvent("back", { bubbles: true, composed: true }));
  }

  private handleEmailChange(e: Event) {
    const input = e.target as HTMLInputElement;
    this.dispatchEvent(
      new CustomEvent("email-change", {
        detail: { email: input.value },
        bubbles: true,
        composed: true,
      })
    );
  }

  private handlePasswordChange(e: Event) {
    const input = e.target as HTMLInputElement;
    this.dispatchEvent(
      new CustomEvent("password-change", {
        detail: { password: input.value },
        bubbles: true,
        composed: true,
      })
    );
  }

  private handleCheckEmail() {
    this.dispatchEvent(new CustomEvent("check-email", { bubbles: true, composed: true }));
  }

  private handleLogin() {
    this.dispatchEvent(new CustomEvent("login", { bubbles: true, composed: true }));
  }

  private handleResetHosting() {
    this.dispatchEvent(new CustomEvent("reset-hosting", { bubbles: true, composed: true }));
  }

  private handleClearRunning() {
    this.dispatchEvent(new CustomEvent("clear-running", { bubbles: true, composed: true }));
  }

  render() {
    if (this.isHostingRunning) {
      return html`
        <j-flex direction="column" gap="500" a="center">
          <j-text variant="body" a="center">
            Hosted executor does not seem to be running. Please check the logs in your
            <a href="https://hosting.ad4m.dev/dashboard" target="_blank"
              >ADAM hosting dashboard</a
            >
            and potentially restart your executor there.
          </j-text>
          <j-button variant="secondary" size="lg" full @click=${this.handleClearRunning}>
            Back
          </j-button>
        </j-flex>
      `;
    }

    if (this.step === 0) {
      return html`
        <j-flex direction="column" gap="500">
          <j-box>
            <j-text variant="label" nomargin>EMAIL</j-text>
            <j-input
              .value=${this.email}
              @input=${this.handleEmailChange}
              size="lg"
            ></j-input>
          </j-box>
          <j-flex gap="300">
            <j-button variant="secondary" size="lg" full @click=${this.handleBack}>
              Back
            </j-button>
            <j-button variant="primary" size="lg" full @click=${this.handleCheckEmail}>
              Next
            </j-button>
          </j-flex>
        </j-flex>
      `;
    }

    if (this.step === 1) {
      return html`
        <j-flex direction="column" gap="500">
          <j-box>
            <j-text variant="label" nomargin>PASSWORD</j-text>
            <j-input
              type="password"
              .value=${this.password}
              @input=${this.handlePasswordChange}
              size="lg"
              error=${this.passwordError ? true : false}
            ></j-input>
            ${this.passwordError
              ? html`<div class="error">${this.passwordError}</div>`
              : ""}
          </j-box>
          <j-flex gap="300">
            <j-button variant="secondary" size="lg" full @click=${this.handleBack}>
              Back
            </j-button>
            <j-button variant="primary" size="lg" full @click=${this.handleLogin}>
              Login
            </j-button>
          </j-flex>
        </j-flex>
      `;
    }

    if (this.step === 2) {
      return html`
        <j-flex direction="column" gap="500" a="center">
          <j-text variant="body" a="center">
            Email is not registered. Please follow the
            <a href="https://hosting.ad4m.dev/" target="_blank">link</a> to register.
          </j-text>
          <j-button variant="secondary" size="lg" full @click=${this.handleResetHosting}>
            Back
          </j-button>
        </j-flex>
      `;
    }

    return html``;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "hosting-view": HostingView;
  }
}