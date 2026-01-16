import { LitElement, html, css } from "lit";
import { customElement, property, state } from "lit/decorators.js";
import { capSentence } from "@coasys/ad4m";
import { sharedStyles } from "../../styles/shared-styles";
import { Ad4mLogo, ArrowLeftRightIcon, CheckIcon } from "../icons";

@customElement("local-authentication")
export class LocalAuthentication extends LitElement {
  @property({ type: Array }) capabilities: string[] = [];
  @property({ type: String }) appname = "";
  @property({ type: String }) appiconpath = "";

  @state() private requestSent = false;
  @state() private securityCode: string = "";

  static styles = [
    sharedStyles,
    css`
      h1 {
        font-size: 34px;
      }

      .connection-icons {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 20px;
        margin: 20px 0;
      }

      .app-image {
        width: 100px;
        height: 100px;
        border-radius: 16px;
        object-fit: cover;
      }

      .app-initial {
        width: 100px;
        height: 100px;
        border-radius: 50%;
        border: 2px solid var(--ac-border-color-light);
      }

      .arrow-icon svg {
        width: 34px;
        height: 34px;
        color: white;
        opacity: 0.5;
      }

      .ad4m-logo svg {
        width: 100px;
        height: 100px;
        color: var(--ac-primary-color);
      }

      .box {
        list-style: none;
        margin: 0 0 20px 0;
      }

      .box li {
        display: flex;
        align-items: center;
        gap: 20px;
      }

      .box p {
        text-align: left;
      }

      .box svg {
        width: 26px;
        height: 26px;
        color: var(--ac-success-color);
        flex-shrink: 0;
      }

      .security-code {
        width: 100%;
        display: flex;
        justify-content: center;
        margin-bottom: 20px;
      }

      .security-code input {
        width: 202px;
        font-size: 36px;
        letter-spacing: 8px;
        height: 60px;
        font-family: system-ui, -apple-system, sans-serif;
        font-variant-numeric: tabular-nums;
        padding: 0 0 0 16px;
      }
    `
  ];

  private cancel() {
    this.dispatchEvent(new CustomEvent("cancel", { bubbles: true, composed: true }));
  }

  private back() {
    this.requestSent = false;
    this.securityCode = "";
  }

  private requestCapability() {
    this.dispatchEvent(new CustomEvent("request-capability", { bubbles: true, composed: true }));
    this.requestSent = true;
  }

  private verifyCode() {
    this.dispatchEvent(new CustomEvent("verify-code", { detail: { code: this.securityCode }, bubbles: true, composed: true }));
  }

  render() {
    return html`
      <div class="container">
        <div class="header">
          <h1>${this.appname}</h1>
          <h3>wants to access your AD4M data</h3>
        </div>

        <div class="connection-icons">
          ${this.appiconpath
            ? html`<img class="app-image" src=${this.appiconpath} alt="App Logo" />`
            : html`<div class="app-initial">${this.appname.charAt(0).toUpperCase()}</div>`
          }
          <div class="arrow-icon">${ArrowLeftRightIcon()}</div>
          <div class="ad4m-logo">${Ad4mLogo()}</div>
        </div>

        ${!this.requestSent
          ? html`
            <p>This will allow ${this.appname} to</p>
          
            <ul class="box">
              ${this.capabilities.map((cap) => html`<li>${CheckIcon()}<p>${capSentence(cap)}</p></li>`)}
            </ul>

            <div class="row">
              <button class="secondary full" @click=${this.cancel}>
                Cancel
              </button>
              <button class="primary full" @click=${this.requestCapability}>
                Authorize
              </button>
            </div>
          `
          : html`
            <p>Enter the security code from your AD4M launcher</p>

            <div class="security-code">
              <input
                type="text"
                maxlength="6"
                inputmode="numeric"
                pattern="[0-9]{6}"
                placeholder="000000"
                .value=${this.securityCode || ""}
                @input=${(e: Event) => {
                  const input = e.target as HTMLInputElement;
                  this.securityCode = input.value.replace(/\D/g, '').slice(0, 6);
                }}
              />
            </div>

            <div class="row">
              <button class="secondary full" @click=${this.back}>
                Back
              </button>
              <button class="primary full" @click=${this.verifyCode}>
                Connect
              </button>
            </div>
          `
        }
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "local-authentication": LocalAuthentication;
  }
}
