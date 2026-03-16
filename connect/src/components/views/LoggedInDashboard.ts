import { LitElement, html, css } from "lit";
import { customElement, property, state } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";
import { CrossIcon, CreditIcon, WalletIcon } from "../icons";
import type { RemoteHost, UserInfo } from "../../types";

@customElement("logged-in-dashboard")
export class LoggedInDashboard extends LitElement {
  @property({ type: Object }) connectedHost: RemoteHost | null = null;
  @property({ type: Object }) userInfo: UserInfo | null = null;
  @property({ type: Boolean }) requestingPayment: boolean = false;
  @property({ type: String }) paymentError: string | null = null;

  @state() private walletInput = "";
  @state() private editingWallet = false;
  @state() private paymentSuccess = false;

  static styles = [
    sharedStyles,
    css`
      .dashboard-header {
        text-align: center;
      }

      .host-badge {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 8px;
        font-size: 14px;
        color: rgba(255, 255, 255, 0.6);
        margin-top: 4px;
      }

      .credit-display {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 10px;
        padding: 16px;
        border-radius: 8px;
        background: rgba(128, 178, 201, 0.14);
        box-shadow: 0 0 0 1px var(--ac-border-color-light);
      }

      .credit-display svg {
        width: 24px;
        height: 24px;
        color: var(--ac-primary-color);
        flex-shrink: 0;
      }

      .credit-amount {
        font-size: 28px;
        font-weight: 700;
        color: #ffffff;
      }

      .credit-label {
        font-size: 14px;
        color: rgba(255, 255, 255, 0.5);
      }

      .credit-display.depleted {
        background: rgba(244, 54, 127, 0.12);
        box-shadow: 0 0 0 1px var(--ac-danger-color);
      }

      .credit-display.depleted .credit-amount {
        color: var(--ac-danger-color);
      }

      .depleted-banner {
        text-align: center;
        padding: 10px;
        border-radius: 8px;
        background: rgba(244, 54, 127, 0.15);
        color: var(--ac-danger-color);
        font-size: 14px;
        font-weight: 600;
      }

      .wallet-section {
        display: flex;
        flex-direction: column;
        gap: 8px;
      }

      .wallet-section label {
        display: flex;
        align-items: center;
        gap: 6px;
        font-size: 13px;
        color: rgba(255, 255, 255, 0.6);
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-weight: 600;
      }

      .wallet-section label svg {
        width: 16px;
        height: 16px;
        color: var(--ac-primary-color);
      }

      .wallet-row {
        display: flex;
        gap: 8px;
      }

      .wallet-row input {
        flex: 1;
      }

      .wallet-row button {
        width: auto;
        flex-shrink: 0;
      }

      .wallet-display {
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 0 16px;
        height: 48px;
        border-radius: 8px;
        background: #00091e5c;
        box-shadow: 0 0 0 1px var(--ac-border-color-light);
        font-size: 14px;
        color: var(--ac-primary-color);
        font-family: monospace;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        flex: 1;
      }

      .topup-buttons {
        display: flex;
        gap: 8px;
      }

      .topup-buttons button {
        flex: 1;
        font-size: 14px;
        padding: 0 12px;
        height: 42px;
      }

      .success-message {
        text-align: center;
        padding: 10px;
        border-radius: 8px;
        background: rgba(93, 210, 125, 0.15);
        color: var(--ac-success-color);
        font-size: 14px;
        font-weight: 600;
      }

      .error-message {
        text-align: center;
        padding: 10px;
        border-radius: 8px;
        background: rgba(244, 54, 127, 0.15);
        color: var(--ac-danger-color);
        font-size: 14px;
      }

      .email-display {
        font-size: 14px;
        color: rgba(255, 255, 255, 0.5);
        text-align: center;
      }

      button.full {
        width: 100%;
      }
    `
  ];

  private close() {
    this.dispatchEvent(new CustomEvent("close", { bubbles: true, composed: true }));
  }

  private disconnect() {
    this.dispatchEvent(new CustomEvent("disconnect", { bubbles: true, composed: true }));
  }

  private setWalletAddress() {
    const address = this.walletInput.trim();
    if (!address) return;
    this.editingWallet = false;
    this.dispatchEvent(new CustomEvent("set-wallet-address", { detail: { address }, bubbles: true, composed: true }));
  }

  private requestTopUp(amount: number) {
    this.paymentSuccess = false;
    this.dispatchEvent(new CustomEvent("request-top-up", { detail: { amountHOT: amount }, bubbles: true, composed: true }));
  }

  private get hasWallet(): boolean {
    return !!this.userInfo?.hotWalletAddress;
  }

  private get isDepleted(): boolean {
    return this.userInfo != null && this.userInfo.remainingCredits <= 0;
  }

  updated(changed: Map<string, unknown>) {
    // Show success message when payment completes
    if (changed.has('requestingPayment') && !this.requestingPayment && !this.paymentError) {
      const wasRequesting = changed.get('requestingPayment') as boolean;
      if (wasRequesting) {
        this.paymentSuccess = true;
        setTimeout(() => { this.paymentSuccess = false; }, 4000);
      }
    }
    // Initialize wallet input from userInfo
    if (changed.has('userInfo') && this.userInfo?.hotWalletAddress && !this.walletInput) {
      this.walletInput = this.userInfo.hotWalletAddress;
    }
  }

  render() {
    const credits = this.userInfo?.remainingCredits ?? 0;

    return html`
      <div class="container">
        <div class="close-button" @click=${this.close}>
          ${CrossIcon()}
        </div>

        <div class="dashboard-header">
          <h1>Dashboard</h1>
          ${this.connectedHost ? html`
            <div class="host-badge">
              Connected to <strong>${this.connectedHost.name}</strong> · ${this.connectedHost.location}
            </div>
          ` : ''}
          ${this.userInfo?.email ? html`
            <p class="email-display">${this.userInfo.email}</p>
          ` : ''}
        </div>

        <!-- Credit balance -->
        <div class="credit-display ${this.isDepleted ? 'depleted' : ''}">
          ${CreditIcon()}
          <span class="credit-amount">${credits.toFixed(2)}</span>
          <span class="credit-label">HOT</span>
        </div>

        ${this.isDepleted ? html`
          <div class="depleted-banner">Credits depleted — top up to continue using this host</div>
        ` : ''}

        <!-- Wallet address -->
        <div class="wallet-section">
          <label>${WalletIcon()} mHOT Wallet Address</label>
          ${this.hasWallet && !this.editingWallet ? html`
            <div class="wallet-row">
              <div class="wallet-display">${this.userInfo!.hotWalletAddress}</div>
              <button class="secondary" @click=${() => { this.editingWallet = true; this.walletInput = this.userInfo!.hotWalletAddress || ''; }}>
                Change
              </button>
            </div>
          ` : html`
            <div class="wallet-row">
              <input
                type="text"
                placeholder="Enter your mHOT wallet address"
                .value=${this.walletInput}
                @input=${(e: Event) => { this.walletInput = (e.target as HTMLInputElement).value; }}
                @keydown=${(e: KeyboardEvent) => { if (e.key === 'Enter') this.setWalletAddress(); }}
                style="font-size: 14px;"
              />
              <button
                class="primary"
                ?disabled=${!this.walletInput.trim()}
                @click=${this.setWalletAddress}
              >
                Save
              </button>
            </div>
          `}
        </div>

        <!-- Top-up buttons -->
        <div class="topup-buttons">
          ${[10, 50, 100].map(amount => html`
            <button
              class="secondary"
              ?disabled=${!this.hasWallet || this.requestingPayment}
              @click=${() => this.requestTopUp(amount)}
            >
              ${this.requestingPayment ? '...' : `${amount} HOT`}
            </button>
          `)}
        </div>

        ${!this.hasWallet ? html`
          <p style="font-size:13px;color:rgba(255,255,255,0.4);text-align:center;">
            Set your wallet address above to enable top-ups
          </p>
        ` : ''}

        ${this.paymentSuccess ? html`
          <div class="success-message">Payment request sent to Unit app</div>
        ` : ''}

        ${this.paymentError ? html`
          <div class="error-message">${this.paymentError}</div>
        ` : ''}

        <button class="danger full" @click=${this.disconnect}>
          Disconnect
        </button>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "logged-in-dashboard": LoggedInDashboard;
  }
}
