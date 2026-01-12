import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";

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

  static styles = css`
    :host {
      display: block;
    }

    .backend-url {
      margin-top: 10px;
      font-size: 12px;
      opacity: 0.7;
      font-family: monospace;
    }

    .code-input {
      text-align: center;
      font-size: 32px;
      letter-spacing: 8px;
      font-weight: 600;
    }

    .help-text {
      font-size: 12px;
      opacity: 0.8;
    }
  `;

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
        <j-flex direction="column" gap="600">
          <j-flex direction="column" gap="300" a="center">
            <j-text variant="heading-lg">Sign in or Sign up</j-text>
            <j-text variant="body">Enter your email to continue</j-text>
            ${this.backendUrl
              ? html`<div class="backend-url">${this.backendUrl}</div>`
              : ""}
          </j-flex>

          ${this.error
            ? html`
                <j-box p="400" bg="danger-100" border="1" radius="200">
                  <j-text variant="body" color="danger-700">${this.error}</j-text>
                </j-box>
              `
            : ""}

          <j-box>
            <j-text variant="label" nomargin>Email</j-text>
            <j-input
              type="email"
              placeholder="your@email.com"
              .value=${this.email}
              @input=${this.handleEmailChange}
              @keypress=${(e: KeyboardEvent) =>
                this.handleKeyPress(e, () => this.handleEmailSubmit(), !!this.email)}
              ?disabled=${this.isLoading}
              autofocus
              size="lg"
            ></j-input>
          </j-box>

          <j-button
            variant="primary"
            size="lg"
            full
            @click=${this.handleEmailSubmit}
            ?disabled=${this.isLoading || !this.email}
          >
            ${this.isLoading ? "Checking..." : "Continue"}
          </j-button>
        </j-flex>
      `;
    }

    // Password step
    if (this.step === "password") {
      return html`
        <j-flex direction="column" gap="600">
          <j-flex direction="column" gap="300" a="center">
            <j-text variant="heading-lg">
              ${this.verificationType === "signup"
                ? "Create your account"
                : "Sign in to your account"}
            </j-text>
            <j-text variant="body">
              Email: <strong>${this.email}</strong>
            </j-text>
            <j-text variant="body">
              ${this.verificationType === "signup"
                ? "Choose a password to create your account"
                : "Enter your password to sign in"}
            </j-text>
          </j-flex>

          ${this.error
            ? html`
                <j-box p="400" bg="danger-100" border="1" radius="200">
                  <j-text variant="body" color="danger-700">${this.error}</j-text>
                </j-box>
              `
            : ""}

          <j-box>
            <j-text variant="label" nomargin>Password</j-text>
            <j-input
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
              size="lg"
            ></j-input>
          </j-box>

          <j-button
            variant="primary"
            size="lg"
            full
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
          </j-button>

          <j-flex j="center">
            <j-button
              variant="link"
              @click=${this.handleBackToEmail}
              ?disabled=${this.isLoading}
            >
              ← Back
            </j-button>
          </j-flex>
        </j-flex>
      `;
    }

    // Code verification step
    if (this.step === "code") {
      return html`
        <j-flex direction="column" gap="600">
          <j-flex direction="column" gap="300" a="center">
            <j-text variant="heading-lg">Check your email</j-text>
            <j-text variant="body">
              We've sent a 6-digit verification code to <strong>${this.email}</strong>
            </j-text>
            <j-text variant="body-sm" style="opacity: 0.7;">
              The code will expire in 15 minutes
            </j-text>
          </j-flex>

          ${this.error
            ? html`
                <j-box p="400" bg="danger-100" border="1" radius="200">
                  <j-text variant="body" color="danger-700">${this.error}</j-text>
                </j-box>
              `
            : ""}

          <j-box>
            <j-text variant="label" nomargin>Verification Code</j-text>
            <j-input
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
              size="lg"
            ></j-input>
          </j-box>

          <j-button
            variant="primary"
            size="lg"
            full
            @click=${this.handleCodeSubmit}
            ?disabled=${this.isLoading || this.verificationCode.length !== 6}
          >
            ${this.isLoading ? "Verifying..." : "Verify Code"}
          </j-button>

          <j-flex direction="column" gap="200" a="center">
            <j-text variant="body-sm" class="help-text" a="center">
              Didn't receive the email? Check your spam folder or
              <j-button
                variant="link"
                @click=${this.handleBackToEmail}
                ?disabled=${this.isLoading}
              >
                try again
              </j-button>
            </j-text>
          </j-flex>
        </j-flex>
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
