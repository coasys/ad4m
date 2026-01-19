import { LitElement, html, css } from "lit";
import { customElement, property, state } from "lit/decorators.js";
import { VerificationRequestResult } from "@coasys/ad4m";
import { sharedStyles } from "../../styles/shared-styles";
import { ArrowLeftIcon, CrossIcon } from "../icons";

@customElement("remote-authentication")
export class RemoteAuthentication extends LitElement {
  @property({ type: Object }) remoteAuthState: VerificationRequestResult | null = null;
  @property({ type: Boolean }) remoteAuthLoading: boolean = false;
  @property({ type: Boolean }) emailCodeError: boolean = false;
  @property({ type: Boolean }) passwordError: boolean = false;
  @property({ type: Boolean }) accountCreationError: boolean = false;

  @state() private email = "";
  @state() private password = "";
  @state() private emailSecurityCode: string = "";

  static styles = [
    sharedStyles,
    css`
      .input-row {
        display: flex;
        justify-content: center;
      }

      .input-row input {
        width: 200px;
      }

      .login-button {
        display: flex;
        justify-content: center;
      }

      .login-button button {
        width: 200px;
      }

      .security-code {
        width: 100%;
        display: flex;
        justify-content: center;
        margin-bottom: 20px;
      }

      .security-code input {
        width: 194px;
        font-size: 36px;
        letter-spacing: 8px;
        height: 60px;
        font-family: system-ui, -apple-system, sans-serif;
        font-variant-numeric: tabular-nums;
        padding: 0 0 0 16px;
      }

      .password-input {
        display: flex;
        justify-content: center;
        margin-bottom: 20px;
      }

      .password-input input {
        width: 200px;
      }

      .state {
        justify-content: center;
      }
    `
  ];

  private back() {
    this.dispatchEvent(new CustomEvent("back", { bubbles: true, composed: true }));
  }

  private emailLogin() {
    this.dispatchEvent(new CustomEvent("email-login", { detail: { email: this.email }, bubbles: true, composed: true }));
  }

  private verifyEmailCode() {
    this.dispatchEvent(new CustomEvent("verify-email-code", { detail: { email: this.email, code: this.emailSecurityCode }, bubbles: true, composed: true }));
  }

  private passwordLogin() {
    this.dispatchEvent(new CustomEvent("password-login", { detail: { email: this.email, password: this.password }, bubbles: true, composed: true }));
  }

  private createAccount() {
    this.dispatchEvent(new CustomEvent("create-account", { detail: { email: this.email, password: this.password }, bubbles: true, composed: true }));
  }

  private onEmailSecurityCodeChange(e: Event) {
    // Clear any previous error when user starts typing
    this.dispatchEvent(new CustomEvent("clear-email-code-error", { bubbles: true, composed: true }));

    // Allow only digits and limit to 6 characters
    const input = e.target as HTMLInputElement;
    const cleaned = input.value.replace(/\D/g, '').slice(0, 6);
    input.value = cleaned;
    this.emailSecurityCode = cleaned;

    // verifyEmailCode(email: string, code: string, verificationType: string): Promise<string>;

    // Auto-verify when 6 digits entered
    if (this.emailSecurityCode.length === 6) this.verifyEmailCode();
  }

  // if (success) {
  //   if (requiresPassword) {
  //     if (isExistingUser) {
  //       // User exists, show password for login
  //     } else {
  //       // New user, show password for signup
  //     }
  //   } else {
  //     // User exists, verification email sent (display code entry)
  //   }
  // } else {
  //   // Show error message
  // }

  // <h3>Enter your email to access <br/> the remote AD4M node</h3>

  render() {
    const showEmailInput = !this.remoteAuthState;
    const showEmailCodeInput = this.remoteAuthState && this.remoteAuthState.success && !this.remoteAuthState.requiresPassword;
    const showLoginPasswordInput = this.remoteAuthState && this.remoteAuthState.requiresPassword && this.remoteAuthState.isExistingUser;
    const showSignUpPasswordInput = this.remoteAuthState && this.remoteAuthState.requiresPassword && !this.remoteAuthState.isExistingUser;
    const showEmailSubmitError = this.remoteAuthState && !this.remoteAuthState.success && !this.remoteAuthState.requiresPassword;

    return html`
      <div class="container">
        <div class="back-button" @click=${this.back}>
          ${ArrowLeftIcon()}
        </div>

        <div class="header">
          <h1>Login to remote node</h1>
        </div>

        ${showEmailInput ? 
          html`
            <div class="input-row">
              <input
                type="email"
                placeholder="email@example.com"
                .value=${this.email || ""}
                @input=${(e: Event) => {
                  const input = e.target as HTMLInputElement;
                  this.email = input.value;
                }}
              />
            </div>

            <div class="login-button">
              <button class="primary" @click=${this.emailLogin} .disabled=${this.remoteAuthLoading}>
                ${this.remoteAuthLoading ? "Loading..." : "Login"}
              </button>
            </div>
          `
          : ``
        }

        ${showEmailSubmitError ? 
          html`
            <div class="state danger">
              ${CrossIcon()}
              <p>${this.remoteAuthState.message || "Failed to process email. Please try again."}</p>
            </div>

            <div class="login-button">
              <button class="primary" @click=${this.back}>
                Back
              </button>
            </div>
          `
          : ``
        }

        ${showEmailCodeInput ? 
          html`
            <div class="security-code">
              <input
                type="text"
                maxlength="6"
                inputmode="numeric"
                pattern="[0-9]{6}"
                placeholder="000000"
                .value=${this.emailSecurityCode || ""}
                @input=${this.onEmailSecurityCodeChange}
              />
            </div>

            ${this.emailCodeError
              ? html`
                  <div class="state danger" style="margin-top: -32px; margin-bottom: 20px;">
                    ${CrossIcon()}
                    <p>Verification failed. Please try again.</p>
                  </div>
                `
              : ''
            }
          `
          : ``
        }

        ${showLoginPasswordInput ? 
          html`
            <div class="password-input">
              <input
                type="password"
                placeholder="Password..."
                .value=${this.password || ""}
                @input=${(e: Event) => {
                  const input = e.target as HTMLInputElement;
                  this.password = input.value;
                }}
              />
            </div>

            <div class="login-button">
              <button class="primary" @click=${this.passwordLogin} .disabled=${this.remoteAuthLoading}>
                ${this.remoteAuthLoading ? "Loading..." : "Login"}
              </button>
            </div>

            ${this.passwordError
              ? html`
                  <div class="state danger" style="margin-top: -32px; margin-bottom: 20px;">
                    ${CrossIcon()}
                    <p>Incorrect password. Please try again.</p>
                  </div>
                `
              : ''
            }
          `
          : ``
        }

        ${showSignUpPasswordInput ? 
          html`
            <div class="password-input">
              <input
                type="password"
                placeholder="Password..."
                .value=${this.password || ""}
                @input=${(e: Event) => {
                  const input = e.target as HTMLInputElement;
                  this.password = input.value;
                }}
              />
            </div>

            <div class="login-button">
              <button class="primary" @click=${this.createAccount} .disabled=${this.remoteAuthLoading}>
                ${this.remoteAuthLoading ? "Loading..." : "Create Account"}
              </button>
            </div>

            ${this.accountCreationError
              ? html`
                  <div class="state danger" style="margin-top: -32px; margin-bottom: 20px;">
                    ${CrossIcon()}
                    <p>Failed to create account. Please try again.</p>
                  </div>
                `
              : ''
            }
          `
          : ``
        }

      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "remote-authentication": RemoteAuthentication;
  }
}
