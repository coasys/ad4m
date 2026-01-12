import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";

@customElement("multi-user-auth")
export class MultiUserAuth extends LitElement {
  @property({ type: String }) email = "";
  @property({ type: String }) password = "";
  @property({ type: String }) verificationCode = "";
  @property({ type: String }) error: string | null = null;
  @property({ type: Boolean }) isLoading = false;
  @property({ type: String }) backendUrl?: string;
  @property({ type: String }) step: "email" | "password" | "code" = "email";
  @property({ type: String }) verificationType: "signup" | "login" = "signup";

  static styles = [
    sharedStyles,
    css`
    .header {
      display: flex;
      flex-direction: column;
      gap: 12px;
      align-items: center;
      text-align: center;
    }

    p.small {
      font-size: 12px;
      opacity: 0.7;
    }

    p strong {
      color: var(--primary-color);
      font-weight: 600;
    }

    .backend-url {
      margin-top: 10px;
      font-size: 12px;
      opacity: 0.7;
      font-family: monospace;
    }

    input.code-input {
      text-align: center;
      font-size: 32px;
      letter-spacing: 8px;
      font-weight: 600;
    }

    button.link {
      padding: 8px 16px;
    }

    .help-text {
      font-size: 12px;
      opacity: 0.8;
      text-align: center;
    }
  `];

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

  private handleCodeChange(e: Event) {
    const input = e.target as HTMLInputElement;
    const cleaned = input.value.replace(/\D/g, "");
    this.dispatchEvent(
      new CustomEvent("code-change", {
        detail: { code: cleaned },
        bubbles: true,
        composed: true,
      })
    );
  }

  private handleEmailSubmit() {
    this.dispatchEvent(new CustomEvent("email-submit", { bubbles: true, composed: true }));
  }

  private handlePasswordSubmit() {
    this.dispatchEvent(
      new CustomEvent("password-submit", { bubbles: true, composed: true })
    );
  }

  private handleCodeSubmit() {
    this.dispatchEvent(new CustomEvent("code-submit", { bubbles: true, composed: true }));
  }

  private handleBackToEmail() {
    this.dispatchEvent(new CustomEvent("back-to-email", { bubbles: true, composed: true }));
  }

  private handleKeyPress(e: KeyboardEvent, callback: () => void, condition: boolean) {
    if (e.key === "Enter" && condition && !this.isLoading) {
      callback();
    }
  }

  render() {
    // Email step
    if (this.step === "email") {
      return html`
        <div class="container">
          <div class="header">
            <h1>Sign in or Sign up</h1>
            <p>Enter your email to continue</p>
            ${this.backendUrl
              ? html`<div class="backend-url">${this.backendUrl}</div>`
              : ""}
          </div>

          ${this.error
            ? html`
                <div class="error-box">
                  ${this.error}
                </div>
              `
            : ""}

          <div class="form-group">
            <label>Email</label>
            <input
              type="email"
              placeholder="your@email.com"
              .value=${this.email}
              @input=${this.handleEmailChange}
              @keypress=${(e: KeyboardEvent) =>
                this.handleKeyPress(e, () => this.handleEmailSubmit(), !!this.email)}
              ?disabled=${this.isLoading}
              autofocus
            />
          </div>

          <button
            class="primary full"
            @click=${this.handleEmailSubmit}
            ?disabled=${this.isLoading || !this.email}
          >
            ${this.isLoading ? "Checking..." : "Continue"}
          </button>
        </div>
      `;
    }

    // Password step
    if (this.step === "password") {
      return html`
        <div class="container">
          <div class="header">
            <h1>
              ${this.verificationType === "signup"
                ? "Create your account"
                : "Sign in to your account"}
            </h1>
            <p>
              Email: <strong>${this.email}</strong>
            </p>
            <p>
              ${this.verificationType === "signup"
                ? "Choose a password to create your account"
                : "Enter your password to sign in"}
            </p>
          </div>

          ${this.error
            ? html`
                <div class="error-box">
                  ${this.error}
                </div>
              `
            : ""}

          <div class="form-group">
            <label>Password</label>
            <input
              type="password"
              placeholder="${this.verificationType === "signup"
                ? "Enter a strong password"
                : "Enter your password"}"
              .value=${this.password}
              @input=${this.handlePasswordChange}
              @keypress=${(e: KeyboardEvent) =>
                this.handleKeyPress(e, () => this.handlePasswordSubmit(), !!this.password)}
              ?disabled=${this.isLoading}
              autofocus
            />
          </div>

          <button
            class="primary full"
            @click=${this.handlePasswordSubmit}
            ?disabled=${this.isLoading || !this.password}
          >
            ${this.verificationType === "signup"
              ? this.isLoading
                ? "Creating account..."
                : "Create Account"
              : this.isLoading
              ? "Signing in..."
              : "Sign In"}
          </button>

          <div class="center">
            <button
              class="link"
              @click=${this.handleBackToEmail}
              ?disabled=${this.isLoading}
            >
              ← Back
            </button>
          </div>
        </div>
      `;
    }

    // Code verification step
    if (this.step === "code") {
      return html`
        <div class="container">
          <div class="header">
            <h1>Check your email</h1>
            <p>
              We've sent a 6-digit verification code to <strong>${this.email}</strong>
            </p>
            <p class="small">
              The code will expire in 15 minutes
            </p>
          </div>

          ${this.error
            ? html`
                <div class="error-box">
                  ${this.error}
                </div>
              `
            : ""}

          <div class="form-group">
            <label>Verification Code</label>
            <input
              type="text"
              inputmode="numeric"
              pattern="[0-9]*"
              maxlength="6"
              class="code-input"
              placeholder="000000"
              .value=${this.verificationCode}
              @input=${this.handleCodeChange}
              ?disabled=${this.isLoading}
              autofocus
            />
          </div>

          <button
            class="primary full"
            @click=${this.handleCodeSubmit}
            ?disabled=${this.isLoading || this.verificationCode.length !== 6}
          >
            ${this.isLoading ? "Verifying..." : "Verify Code"}
          </button>

          <div class="header">
            <p class="help-text">
              Didn't receive the email? Check your spam folder or
              <button
                class="link"
                @click=${this.handleBackToEmail}
                ?disabled=${this.isLoading}
              >
                try again
              </button>
            </p>
          </div>
        </div>
      `;
    }

    return html``;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "multi-user-auth": MultiUserAuth;
  }
}
