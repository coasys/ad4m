import { LitElement, html, css } from "lit";
import { customElement, property, state } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";
import { ArrowLeftIcon } from "../icons";
import { renderHostAvatar } from "../shared/avatar";
import type { RemoteHost } from "../../types";

@customElement("host-browser")
export class HostBrowser extends LitElement {
  @property({ type: Array }) hosts: RemoteHost[] = [];
  @property({ type: Boolean }) loading: boolean = false;
  @property({ type: String }) error: string | null = null;
  @property({ type: String }) lastHostId: string | null = null;
  @property({ type: String }) defaultUrl: string = "";

  @state() private manualUrl = "";
  @state() private manualUrlError: string | null = null;
  private defaultApplied = false;

  static styles = [
    sharedStyles,
    css`
      :host {
        display: flex;
        flex-direction: column;
        min-height: 0;
        flex: 1 1 auto;
      }

      .container {
        flex: 1 1 auto;
        min-height: 0;
        overflow: hidden;
      }

      .host-list {
        display: flex;
        flex-direction: column;
        gap: 12px;
        max-height: min(500px, calc(100vh - 380px));
        overflow-y: auto;
        padding: 2px;
      }

      .host-card {
        display: flex;
        align-items: center;
        gap: 12px;
        padding: 14px;
        border-radius: 8px;
        background: rgba(128, 178, 201, 0.10);
        box-shadow: 0 0 0 1px var(--ac-border-color-light);
        cursor: pointer;
        transition: all 0.2s ease;
      }

      .host-card:hover,
      .host-card:focus-visible {
        background: rgba(128, 178, 201, 0.20);
        box-shadow: 0 0 0 1px var(--ac-primary-color);
        outline: none;
      }

      .host-card.pinned {
        box-shadow: 0 0 0 1px var(--ac-primary-color);
        background: rgba(128, 178, 201, 0.16);
      }

      .host-avatar {
        width: 40px;
        height: 40px;
        border-radius: 50%;
        flex-shrink: 0;
        object-fit: cover;
        background: rgba(128, 178, 201, 0.2);
      }

      .host-avatar.fallback {
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 16px;
        font-weight: 600;
        color: #fff;
      }

      .host-info {
        flex: 1;
        min-width: 0;
      }

      .host-name {
        font-size: 15px;
        font-weight: 600;
        color: #ffffff;
        margin: 0;
        text-align: left;
      }

      .host-location {
        font-size: 13px;
        color: rgba(255, 255, 255, 0.6);
        margin: 2px 0 0 0;
        text-align: left;
      }

      .host-meta {
        display: flex;
        flex-wrap: wrap;
        gap: 4px;
        margin-top: 6px;
      }

      .model-chip {
        font-size: 11px;
        padding: 2px 8px;
        border-radius: 10px;
        background: rgba(145, 227, 253, 0.15);
        color: var(--ac-primary-color);
        white-space: nowrap;
      }

      .rates-preview {
        font-size: 12px;
        color: rgba(255, 255, 255, 0.5);
        margin-top: 4px;
        text-align: left;
      }

      .spinner {
        display: flex;
        justify-content: center;
        padding: 40px 0;
      }

      .spinner::after {
        content: "";
        width: 32px;
        height: 32px;
        border: 3px solid var(--ac-border-color-light);
        border-top-color: var(--ac-primary-color);
        border-radius: 50%;
        animation: spin 0.8s linear infinite;
      }

      @keyframes spin {
        to { transform: rotate(360deg); }
      }

      .divider {
        display: flex;
        align-items: center;
        gap: 12px;
        margin: 8px 0;
      }

      .divider::before,
      .divider::after {
        content: "";
        flex: 1;
        height: 1px;
        background: var(--ac-border-color-light);
      }

      .divider span {
        font-size: 13px;
        color: rgba(255, 255, 255, 0.4);
        white-space: nowrap;
      }

      .manual-entry {
        display: flex;
        gap: 8px;
        width: 100%;
        padding: 2px;
      }

      .manual-entry input {
        flex: 1;
      }

      .manual-entry button {
        width: auto;
        flex-shrink: 0;
      }

      .pinned-label {
        font-size: 11px;
        color: var(--ac-primary-color);
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-weight: 600;
      }

      button.back-button {
        all: unset;
        cursor: pointer;
      }
    `
  ];

  willUpdate(changedProps: import("lit").PropertyValues) {
    // Pre-fill manual URL from defaultUrl on first render only
    if (changedProps.has('defaultUrl') && this.defaultUrl && !this.defaultApplied) {
      this.manualUrl = this.defaultUrl;
      this.defaultApplied = true;
    }
  }

  private back() {
    this.dispatchEvent(new CustomEvent("back", { bubbles: true, composed: true }));
  }

  private selectHost(host: RemoteHost) {
    this.dispatchEvent(new CustomEvent("select-host", { detail: { host }, bubbles: true, composed: true }));
  }

  private connectManualUrl() {
    const url = this.manualUrl.trim();
    if (!url) return;

    let parsedUrl: URL;
    try {
      parsedUrl = new URL(url);
    } catch {
      this.manualUrlError = "Invalid URL format";
      return;
    }

    if (parsedUrl.protocol !== 'ws:' && parsedUrl.protocol !== 'wss:') {
      this.manualUrlError = "URL must use ws:// or wss:// protocol";
      return;
    }

    this.manualUrlError = null;

    const host: RemoteHost = {
      id: `manual-${Date.now()}`,
      name: parsedUrl.hostname,
      profilePicUrl: "",
      location: "Custom URL",
      url,
      rates: [],
      aiModels: [],
    };

    this.selectHost(host);
  }

  private renderAvatar(host: RemoteHost) {
    return renderHostAvatar(host, "host-avatar");
  }

  private retry() {
    this.dispatchEvent(new CustomEvent("retry", { bubbles: true, composed: true }));
  }

  private formatPrice(price: number): string {
    if (price === 0) return "0.00";
    if (price >= 0.01) return price.toFixed(2);
    if (price >= 0.0001) return price.toFixed(4);
    if (price >= 0.000001) return price.toFixed(6);
    return price.toExponential(2);
  }

  private getLinkPrice(rates: RemoteHost["rates"]): number | null {
    const r = rates.find(r => r.description === "link write");
    return r ? r.priceInHOT : null;
  }

  private getAvgTokenPrice(rates: RemoteHost["rates"]): number | null {
    const tokenRates = rates.filter(r => r.description.endsWith("per token"));
    if (tokenRates.length === 0) return null;
    return tokenRates.reduce((sum, r) => sum + r.priceInHOT, 0) / tokenRates.length;
  }

  private getSortedHosts(): RemoteHost[] {
    if (!this.lastHostId) return this.hosts;
    const pinned = this.hosts.filter(h => h.id === this.lastHostId);
    const rest = this.hosts.filter(h => h.id !== this.lastHostId);
    return [...pinned, ...rest];
  }

  render() {
    return html`
      <div class="container">
        <button class="back-button" @click=${this.back} aria-label="Go back">
          ${ArrowLeftIcon()}
        </button>

        <div class="header">
          <h1>Remote Node</h1>
          <h3>Choose a host or enter a URL</h3>
        </div>

        ${this.loading ? html`
          <div class="spinner"></div>
        ` : this.error ? html`
          <div class="box">
            <p style="color: var(--ac-danger-color)">${this.error}</p>
            <button class="primary" @click=${this.retry}>Retry</button>
          </div>
        ` : html`
          <div class="host-list">
            ${this.getSortedHosts().map(host => html`
              <div
                class="host-card ${host.id === this.lastHostId ? 'pinned' : ''}"
                tabindex="0"
                role="button"
                aria-label="Connect to ${host.name}"
                @click=${() => this.selectHost(host)}
                @keydown=${(e: KeyboardEvent) => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); this.selectHost(host); } }}
              >
                ${this.renderAvatar(host)}
                <div class="host-info">
                  <div style="display:flex;align-items:center;gap:8px;">
                    <p class="host-name">${host.name}</p>
                    ${host.id === this.lastHostId ? html`<span class="pinned-label">Last used</span>` : ''}
                  </div>
                  <p class="host-location">${host.location}</p>
                  <div class="host-meta">
                    ${host.aiModels.map(model => html`<span class="model-chip">${model}</span>`)}
                  </div>
                  ${host.rates.length > 0 ? html`
                    <p class="rates-preview">
                      ${this.getLinkPrice(host.rates) != null ? html`Link: ${this.formatPrice(this.getLinkPrice(host.rates)!)} HOT` : ''}${this.getLinkPrice(host.rates) != null && this.getAvgTokenPrice(host.rates) != null ? ' · ' : ''}${this.getAvgTokenPrice(host.rates) != null ? html`Token: ~${this.formatPrice(this.getAvgTokenPrice(host.rates)!)} HOT` : ''}
                    </p>
                  ` : ''}
                </div>
              </div>
            `)}
          </div>
        `}

        <div class="divider"><span>or enter URL directly</span></div>

        <div class="manual-entry">
          <input
            type="text"
            placeholder="wss://your-host.example/graphql"
            .value=${this.manualUrl}
            @input=${(e: Event) => { this.manualUrl = (e.target as HTMLInputElement).value; this.manualUrlError = null; }}
            @keydown=${(e: KeyboardEvent) => { if (e.key === 'Enter') this.connectManualUrl(); }}
            style="font-size: 14px;"
          />
          <button
            class="primary"
            ?disabled=${!this.manualUrl.trim()}
            @click=${this.connectManualUrl}
          >
            Connect
          </button>
        </div>
        ${this.manualUrlError ? html`<p class="state danger" style="margin-top: 6px; font-size: 13px;">${this.manualUrlError}</p>` : ''}
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "host-browser": HostBrowser;
  }
}
