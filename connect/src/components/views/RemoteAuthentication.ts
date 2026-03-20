import { LitElement, html, css } from "lit";
import { customElement, property, state } from "lit/decorators.js";
import { VerificationRequestResult } from "@coasys/ad4m";
import { sharedStyles } from "../../styles/shared-styles";
import { CheckIcon, CrossIcon } from "../icons";
import type { RemoteHost } from "../../types";

@customElement("remote-authentication")
export class RemoteAuthentication extends LitElement {
  @property({ type: Object }) remoteAuthState: VerificationRequestResult | null = null;
  @property({ type: Boolean }) remoteAuthLoading: boolean = false;
  @property({ type: Boolean }) emailCodeError: boolean = false;
  @property({ type: Boolean }) passwordError: boolean = false;
  @property({ type: Boolean }) accountCreationError: boolean = false;
  @property({ type: Object }) host: RemoteHost | null = null;

  @state() private email = "";
  @state() private password = "";
  @state() private emailSecurityCode: string = "";
  @state() private confirmPassword: string = "";

  static styles = [
    sharedStyles,
    css`
      .header h1 {
        font-size: 32px;
        margin-bottom: 0;
      }
  
      .input-row {
        display: flex;
        justify-content: center;
      }

      .input-row input {
        width: 280px;
      }

      .login-button {
        display: flex;
        justify-content: center;
      }

      .security-code {
        width: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 12px;
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

      .state {
        justify-content: center;
      }
    `
  ];

  private emailLogin() {
    this.dispatchEvent(new CustomEvent("email-login", { detail: { email: this.email.trim() }, bubbles: true, composed: true }));
  }

  private tryAgain() {
    this.dispatchEvent(new CustomEvent("reset-auth-state", { bubbles: true, composed: true }));
  }

  private verifyEmailCode() {
    this.dispatchEvent(new CustomEvent("verify-email-code", { detail: { email: this.email.trim(), code: this.emailSecurityCode }, bubbles: true, composed: true }));
  }

  private passwordLogin() {
    this.dispatchEvent(new CustomEvent("password-login", { detail: { email: this.email.trim(), password: this.password }, bubbles: true, composed: true }));
  }

  private createAccount() {
    this.dispatchEvent(new CustomEvent("create-account", { detail: { email: this.email.trim(), password: this.password }, bubbles: true, composed: true }));
  }

  private onEmailSecurityCodeChange(e: Event) {
    // Clear any previous error when user starts typing
    this.dispatchEvent(new CustomEvent("clear-email-code-error", { bubbles: true, composed: true }));

    // Allow only digits and limit to 6 characters
    const input = e.target as HTMLInputElement;
    const cleaned = input.value.replace(/\D/g, '').slice(0, 6);
    input.value = cleaned;
    this.emailSecurityCode = cleaned;

    // Auto-verify when 6 digits entered (only if not already verifying)
    if (this.emailSecurityCode.length === 6 && !this.remoteAuthLoading) {
      this.verifyEmailCode();
    }
  }

  private get passwordsMatch(): boolean {
    return this.password.length > 0 && this.password === this.confirmPassword;
  }

  private isValidEmail(): boolean {
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return emailRegex.test(this.email.trim());
  }

  render() {
    const showEmailInput = !this.remoteAuthState;
    const showEmailCodeInput = this.remoteAuthState && this.remoteAuthState.success && !this.remoteAuthState.requiresPassword;
    const showLoginPasswordInput = this.remoteAuthState && this.remoteAuthState.requiresPassword && this.remoteAuthState.isExistingUser;
    const showSignUpPasswordInput = this.remoteAuthState && this.remoteAuthState.requiresPassword && !this.remoteAuthState.isExistingUser;
    const showEmailSubmitError = this.remoteAuthState && !this.remoteAuthState.success && !this.remoteAuthState.requiresPassword;

    return html`
      <div class="container">
        <div class="header">
          <h1 style="font-size: 28px;">${showLoginPasswordInput ? 'Login' : showSignUpPasswordInput ? 'Register' : 'Login or Register'}</h1>
          ${this.host ? html`<p style="font-size: 15px; color: rgba(255,255,255,0.4); margin-top: 10px;">${this.host.url}</p>` : ''}
        </div>

        ${showEmailInput ?
          html`
            <h3>Enter your email to login to ${this.host ? html`<strong>${this.host.name}</strong>` : 'the remote node'}</h3>

            <div class="input-row">
              <input
                type="email"
                placeholder="email@example.com"
                .value=${this.email || ""}
                @input=${(e: Event) => {
                  const input = e.target as HTMLInputElement;
                  this.email = input.value;
                }}
                @keydown=${(e: KeyboardEvent) => {
                  if (e.key === 'Enter' && !this.remoteAuthLoading && this.isValidEmail()) {
                    this.emailLogin();
                  }
                }}
              />
            </div>

            <div class="login-button" style="margin-top: 10px;">
              <button class="primary" @click=${this.emailLogin} .disabled=${this.remoteAuthLoading || !this.isValidEmail()}>
                ${this.remoteAuthLoading ? "Loading..." : "Continue"}
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
            <div class="login-button" style="margin-top: 10px;">
              <button class="primary" @click=${this.tryAgain}>Try again</button>
            </div>
          `
          : ``
        }

        ${showEmailCodeInput ? 
          html`
            <div class="state success" style="margin-bottom: -12px">
              ${CheckIcon()}
              <p>Email verification code sent</p>
            </div>

            <h3>Check your emails and paste the code below</h3>

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
              ${this.remoteAuthLoading ? html`<div class="spinner"></div>` : ''}
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
            <div class="state success" style="margin-bottom: -12px">
              ${CheckIcon()}
              <p>Account found</p>
            </div>
                      
            <h3>Enter your password to login</h3>

            <div class="input-row">
              <input
                type="password"
                placeholder="Password..."
                .value=${this.password || ""}
                @input=${(e: Event) => {
                  const input = e.target as HTMLInputElement;
                  this.password = input.value;
                }}
                @keydown=${(e: KeyboardEvent) => {
                  if (e.key === 'Enter' && !this.remoteAuthLoading && this.password.length) {
                    this.passwordLogin();
                  }
                }}
              />
            </div>

            <div class="login-button">
              <button class="primary" @click=${this.passwordLogin} .disabled=${this.remoteAuthLoading || !this.password.length}>
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
            <div class="state success">
              ${CheckIcon()}
              <p>Creating a new account for <strong>${this.email}</strong></p>
            </div>

            <h3>Choose a password</h3>

            <div class="input-row">
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

            <div class="input-row" style="margin-top: 8px;">
              <input
                type="password"
                placeholder="Confirm password..."
                .value=${this.confirmPassword || ""}
                @input=${(e: Event) => {
                  const input = e.target as HTMLInputElement;
                  this.confirmPassword = input.value;
                }}
                @keydown=${(e: KeyboardEvent) => {
                  if (e.key === 'Enter' && !this.remoteAuthLoading && this.passwordsMatch) {
                    this.createAccount();
                  }
                }}
              />
            </div>

            ${this.confirmPassword.length > 0 && !this.passwordsMatch
              ? html`<p style="font-size: 13px; color: var(--ac-danger-color); text-align: center;">Passwords do not match</p>`
              : ''
            }

            <div class="login-button">
              <button class="primary" @click=${this.createAccount} .disabled=${this.remoteAuthLoading || !this.passwordsMatch}>
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
