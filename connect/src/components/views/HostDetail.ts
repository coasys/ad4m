import { LitElement, html, css, TemplateResult } from "lit";
import { customElement, property } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";
import { MapPinIcon } from "../icons";
import { renderHostAvatar } from "../shared/avatar";
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
        width: 100px;
        height: 100px;
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
        display: inline-flex;
        align-items: center;
        gap: 6px;
        font-size: 14px;
        color: rgba(255, 255, 255, 0.6);
        margin: 0;
      }

      .profile-location svg {
        opacity: 0.7;
      }

      .profile-description {
        font-size: 14px;
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
    `
  ];

  private back() {
    this.dispatchEvent(new CustomEvent("back", { bubbles: true, composed: true }));
  }

  private proceedToAuth() {
    this.dispatchEvent(new CustomEvent("proceed-to-auth", { detail: { host: this.host }, bubbles: true, composed: true }));
  }

  private renderAvatar(): TemplateResult {
    return renderHostAvatar(this.host, "profile-pic");
  }

  private rateLabel(description: string): string {
    if (description === "link write") return "Link write";
    if (description === "embedding per token") return "Embedding (per token)";
    return description;
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
        <div class="profile">
          ${this.renderAvatar()}
          <p class="profile-name">${this.host.name}</p>
          <p class="profile-location">${MapPinIcon()}${this.host.location}</p>
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
                ${this.host.rates.filter(r => !r.description.endsWith("per token")).map(rate => html`
                  <tr>
                    <td>${this.rateLabel(rate.description)}</td>
                    <td>${this.formatPrice(rate.priceInHOT)} HOT</td>
                  </tr>
                `)}
                ${this.host.rates.filter(r => r.description.endsWith("per token")).length > 0 ? html`
                  <tr><td colspan="2" style="padding-top:12px;color:rgba(255,255,255,0.5);font-size:12px;text-transform:uppercase;letter-spacing:0.5px;">AI Models (per token)</td></tr>
                  ${this.host.rates.filter(r => r.description.endsWith("per token")).map(rate => html`
                    <tr>
                      <td>${rate.description.replace(" per token", "")}</td>
                      <td>${this.formatPrice(rate.priceInHOT)} HOT</td>
                    </tr>
                  `)}
                ` : ''}
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
