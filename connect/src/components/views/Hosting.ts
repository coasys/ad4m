import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";

@customElement("hosting-view")
export class HostingView extends LitElement {
  @property({ type: Number }) step = 0;
  @property({ type: String }) email = "";
  @property({ type: String }) password = "";
  @property({ type: String }) passwordError = "";
  @property({ type: Boolean }) isHostingRunning: boolean | null = null;

  static styles = [
    sharedStyles,
    css`
    .container {
      gap: 20px;
    }

    .section {
      display: flex;
      flex-direction: column;
      gap: 16px;
      align-items: center;
    }

    p {
      text-align: center;
    }

    .form-group {
      width: 100%;
    }

    input.error {
      border-color: #ef4444;
      background: rgba(239, 68, 68, 0.05);
    }

    .error-text {
      color: #ef4444;
      font-size: 14px;
      margin-top: 4px;
    }

    a {
      color: var(--ac-primary-color);
      text-decoration: underline;
    }

    a:hover {
      filter: brightness(1.2);
    }
  `];

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
        <div class="container">
          <div class="section">
            <p>
              Hosted executor does not seem to be running. Please check the logs in your
              <a href="https://hosting.ad4m.dev/dashboard" target="_blank"
                >ADAM hosting dashboard</a
              >
              and potentially restart your executor there.
            </p>
            <button class="secondary full" @click=${this.handleClearRunning}>
              Back
            </button>
          </div>
        </div>
      `;
    }

    if (this.step === 0) {
      return html`
        <div class="container">
          <div class="form-group">
            <label>EMAIL</label>
            <input
              type="email"
              placeholder="Enter your email"
              .value=${this.email}
              @input=${this.handleEmailChange}
            />
          </div>
          <div class="button-row">
            <button class="secondary full" @click=${this.handleBack}>
              Back
            </button>
            <button class="primary full" @click=${this.handleCheckEmail}>
              Next
            </button>
          </div>
        </div>
      `;
    }

    if (this.step === 1) {
      return html`
        <div class="container">
          <div class="form-group">
            <label>PASSWORD</label>
            <input
              type="password"
              placeholder="Enter your password"
              class="${this.passwordError ? "error" : ""}"
              .value=${this.password}
              @input=${this.handlePasswordChange}
            />
            ${this.passwordError
              ? html`<div class="error-text">${this.passwordError}</div>`
              : ""}
          </div>
          <div class="button-row">
            <button class="secondary full" @click=${this.handleBack}>
              Back
            </button>
            <button class="primary full" @click=${this.handleLogin}>
              Login
            </button>
          </div>
        </div>
      `;
    }

    if (this.step === 2) {
      return html`
        <div class="container">
          <div class="section">
            <p>
              Email is not registered. Please follow the
              <a href="https://hosting.ad4m.dev/" target="_blank">link</a> to register.
            </p>
            <button class="secondary full" @click=${this.handleResetHosting}>
              Back
            </button>
          </div>
        </div>
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