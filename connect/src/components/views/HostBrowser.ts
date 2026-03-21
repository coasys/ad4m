import { LitElement, html, css } from "lit";
import { customElement, property, state } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";
import { MapPinIcon } from "../icons";
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
        gap: 16px;
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
        width: 60px;
        height: 60px;
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
        display: inline-flex;
        align-items: center;
        gap: 4px;
        font-size: 13px;
        color: rgba(255, 255, 255, 0.6);
        margin: 2px 0 0 0;
        text-align: left;
      }

      .host-location svg {
        opacity: 0.7;
      }

      .host-meta {
        display: flex;
        flex-wrap: wrap;
        gap: 4px;
        margin-top: 6px;
      }

      .model-chip {
        font-size: 12px;
        padding: 3px 8px;
        border-radius: 10px;
        background: rgba(145, 227, 253, 0.15);
        color: var(--ac-primary-color);
        white-space: nowrap;
      }

      .rates-preview {
        font-size: 12px;
        color: rgba(255, 255, 255, 0.5);
        margin-top: 6px;
        text-align: left;
      }

      .spinner-container {
        display: flex;
        justify-content: center;
        padding: 40px 0;
      }

      .spinner-container .spinner {
        width: 32px;
        height: 32px;
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

      .trust-notice {
        display: flex;
        gap: 10px;
        padding: 12px 14px;
        border-radius: 8px;
        background: rgba(255, 200, 50, 0.08);
        border: 1px solid rgba(255, 200, 50, 0.25);
        margin-bottom: 12px;
        font-size: 13px;
        line-height: 1.45;
        color: rgba(255, 255, 255, 0.75);
      }

      .trust-notice svg {
        flex-shrink: 0;
        margin-top: 1px;
      }

      .pinned-label {
        font-size: 11px;
        color: var(--ac-primary-color);
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-weight: 600;
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
    const r = rates.find(r => r.description.trim().toLowerCase() === "link write");
    return r ? r.priceInHOT : null;
  }

  private getAvgTokenPrice(rates: RemoteHost["rates"]): number | null {
    const tokenRates = rates.filter(r => r.description.trim().toLowerCase().endsWith("per token"));
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
        <div class="header">
          <h1>Remote Node</h1>
          <h3>Choose a host or enter a URL</h3>
        </div>

        <div class="trust-notice">
          <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="rgba(255, 200, 50, 0.9)" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
            <path d="M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z"/>
            <line x1="12" y1="9" x2="12" y2="13"/>
            <line x1="12" y1="17" x2="12.01" y2="17"/>
          </svg>
          <span>By connecting to a remote host, you are trusting them to run AD4M on your behalf. Your data will be stored on their machine and they will have access to your agent's operations. Only connect to hosts you trust.</span>
        </div>

        ${this.loading ? html`
          <div class="spinner-container"><div class="spinner"></div></div>
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
                  <p class="host-location">${MapPinIcon()}${host.location}</p>
                  <div class="host-meta">
                    ${host.aiModels.map(model => html`<span class="model-chip">${model}</span>`)}
                  </div>
                  ${host.rates.length > 0 ? (() => {
                    const linkPrice = this.getLinkPrice(host.rates);
                    const avgTokenPrice = this.getAvgTokenPrice(host.rates);
                    return html`
                    <p class="rates-preview">
                      ${linkPrice != null ? html`Link: ${this.formatPrice(linkPrice)} wHOT` : ''}${linkPrice != null && avgTokenPrice != null ? ' · ' : ''}${avgTokenPrice != null ? html`Token: ~${this.formatPrice(avgTokenPrice)} wHOT` : ''}
                    </p>
                  `; })() : ''}
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
