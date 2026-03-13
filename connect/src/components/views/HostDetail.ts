import { LitElement, html, css, TemplateResult } from "lit";
import { customElement, property } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";
import { ArrowLeftIcon } from "../icons";
import { getInitials, getHue } from "../../utils";
import type { RemoteHost } from "../../types";

@customElement("host-detail")
export class HostDetail extends LitElement {
  @property({ type: Object }) host!: RemoteHost;

  static styles = [
    sharedStyles,
    css`
      .profile {
        display: flex;
        flex-direction: column;
        align-items: center;
        gap: 10px;
      }

      .profile-pic {
        width: 72px;
        height: 72px;
        border-radius: 50%;
        object-fit: cover;
        background: rgba(128, 178, 201, 0.2);
      }

      .profile-pic.fallback {
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 28px;
        font-weight: 600;
        color: #fff;
      }

      .profile-name {
        font-size: 22px;
        font-weight: 700;
        color: #ffffff;
        margin: 0;
      }

      .profile-location {
        font-size: 14px;
        color: rgba(255, 255, 255, 0.6);
        margin: 0;
      }

      .profile-description {
        font-size: 14px;
        color: rgba(255, 255, 255, 0.5);
        margin: 0;
        text-align: center;
        line-height: 1.4;
      }

      .compute-specs {
        font-size: 14px;
        color: rgba(255, 255, 255, 0.8);
        margin: 0;
      }

      .models {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 6px;
        margin-top: 4px;
      }

      .model-chip {
        font-size: 13px;
        padding: 4px 12px;
        border-radius: 12px;
        background: rgba(145, 227, 253, 0.15);
        color: var(--ac-primary-color);
      }

      .rates-section {
        width: 100%;
      }

      .rates-section h3 {
        text-align: left;
        font-size: 14px;
        font-weight: 600;
        color: rgba(255, 255, 255, 0.6);
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-bottom: 10px;
      }

      .rates-table {
        width: 100%;
        border-collapse: collapse;
      }

      .rates-table tr {
        border-bottom: 1px solid var(--ac-border-color-dark);
      }

      .rates-table tr:last-child {
        border-bottom: none;
      }

      .rates-table td {
        padding: 8px 0;
        font-size: 14px;
      }

      .rates-table td:first-child {
        color: rgba(255, 255, 255, 0.8);
      }

      .rates-table td:last-child {
        text-align: right;
        color: var(--ac-primary-color);
        font-family: monospace;
        font-size: 13px;
      }

      button.connect-button {
        width: 100%;
      }

      button.back-button {
        all: unset;
        cursor: pointer;
      }
    `
  ];

  private back() {
    this.dispatchEvent(new CustomEvent("back", { bubbles: true, composed: true }));
  }

  private proceedToAuth() {
    this.dispatchEvent(new CustomEvent("proceed-to-auth", { detail: { host: this.host }, bubbles: true, composed: true }));
  }

  private renderAvatar(): TemplateResult {
    if (this.host.profilePicUrl) {
      return html`<img class="profile-pic" src=${this.host.profilePicUrl} alt="" />`;
    }
    const hue = getHue(this.host.id || this.host.name);
    const initials = getInitials(this.host.name);
    return html`<div class="profile-pic fallback" style="background:hsl(${hue},60%,35%)">${initials}</div>`;
  }

  private formatPrice(price: number): string {
    if (price === 0) return "0.00";
    // Show enough decimal places to be meaningful
    if (price >= 0.01) return price.toFixed(2);
    if (price >= 0.0001) return price.toFixed(4);
    if (price >= 0.000001) return price.toFixed(6);
    return price.toExponential(2);
  }

  render() {
    if (!this.host) return null;

    return html`
      <div class="container">
        <button class="back-button" aria-label="Go back" @click=${this.back}>
          ${ArrowLeftIcon()}
        </button>

        <div class="profile">
          ${this.renderAvatar()}
          <p class="profile-name">${this.host.name}</p>
          <p class="profile-location">${this.host.location}</p>
          ${this.host.description ? html`<p class="profile-description">${this.host.description}</p>` : ''}

          ${this.host.aiModels.length > 0 ? html`
            <div class="models">
              ${this.host.aiModels.map(m => html`<span class="model-chip">${m}</span>`)}
            </div>
          ` : ''}
        </div>

        ${this.host.rates.length > 0 ? html`
          <div class="box">
            <div class="rates-section">
              <h3>Pricing</h3>
              <table class="rates-table">
                ${this.host.rates.map(rate => html`
                  <tr>
                    <td>${rate.description}</td>
                    <td>${this.formatPrice(rate.priceInHOT)} HOT</td>
                  </tr>
                `)}
              </table>
            </div>
          </div>
        ` : ''}

        ${this.host.computeSpecs ? html`
          <div class="box">
            <div class="rates-section">
              <h3>Compute</h3>
              <p class="compute-specs">${this.host.computeSpecs}</p>
            </div>
          </div>
        ` : ''}

        <button class="primary connect-button" @click=${this.proceedToAuth}>
          Connect to ${this.host.name}
        </button>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "host-detail": HostDetail;
  }
}
