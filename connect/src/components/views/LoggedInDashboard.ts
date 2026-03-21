import { LitElement, html, css } from "lit";
import { customElement, property, state } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";
import { CheckIcon, CrossIcon, CreditIcon, MapPinIcon, WalletIcon } from "../icons";
import type { RemoteHost, UserInfo } from "../../types";

@customElement("logged-in-dashboard")
export class LoggedInDashboard extends LitElement {
  @property({ type: Object }) connectedHost: RemoteHost | null = null;
  @property({ type: Object }) userInfo: UserInfo | null = null;
  @property({ type: Boolean }) requestingPayment: boolean = false;
  @property({ type: String }) paymentError: string | null = null;

  @state() private walletInput = "";
  @state() private editingWallet = false;
  @state() private walletSaveSuccess = false;
  @state() private topUpAmount = "";
  @state() private awaitingApproval = false;
  @state() private creditsUpdated = false;
  private previousCredits: number | null = null;

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
        gap: 5px;
        font-size: 14px;
        color: rgba(255, 255, 255, 0.6);
        margin-bottom: 8px;
      }

      .host-badge strong {
        color: #ffffff;
      }

      .host-badge svg {
        opacity: 0.7;
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

      .credit-display.free-access {
        background: rgba(93, 210, 125, 0.14);
        box-shadow: 0 0 0 1px var(--ac-success-color);
      }

      .credit-display.free-access .credit-amount {
        color: var(--ac-success-color);
        font-size: 20px;
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
        color: var(--ac-primary-color-light);
        font-family: monospace;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        flex: 1;
      }

      .topup-section {
        display: flex;
        flex-direction: column;
        gap: 10px;
      }

      .topup-section label {
        display: flex;
        align-items: center;
        gap: 6px;
        font-size: 13px;
        color: rgba(255, 255, 255, 0.6);
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-weight: 600;
      }

      .topup-section label svg {
        width: 16px;
        height: 16px;
        color: var(--ac-primary-color);
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

      .pending-message {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 8px;
        text-align: center;
        padding: 10px;
        border-radius: 8px;
        background: rgba(128, 178, 201, 0.14);
        box-shadow: 0 0 0 1px var(--ac-border-color-light);
        color: rgba(255, 255, 255, 0.8);
        font-size: 14px;
      }

      .pending-message .spinner {
        width: 16px;
        height: 16px;
        flex-shrink: 0;
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

      .success-message svg {
        width: 16px;
        height: 16px;
        flex-shrink: 0;
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

      .footer-actions {
        display: flex;
        justify-content: center;
        gap: 8px;
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
    this.walletSaveSuccess = true;
    setTimeout(() => { this.walletSaveSuccess = false; }, 3000);
    this.dispatchEvent(new CustomEvent("set-wallet-address", { detail: { address }, bubbles: true, composed: true }));
  }

  private requestTopUp(amount: number) {
    this.creditsUpdated = false;
    this.dispatchEvent(new CustomEvent("request-top-up", { detail: { amountHOT: amount }, bubbles: true, composed: true }));
  }

  private truncateMiddle(str: string, startChars = 8, endChars = 6): string {
    if (str.length <= startChars + endChars + 3) return str;
    return `${str.slice(0, startChars)}…${str.slice(-endChars)}`;
  }

  private get hasWallet(): boolean {
    return !!this.userInfo?.hotWalletAddress;
  }

  private get isFreeAccess(): boolean {
    return !!this.userInfo?.freeAccess;
  }

  private get isDepleted(): boolean {
    return this.userInfo != null && !this.userInfo.freeAccess && this.userInfo.remainingCredits <= 0;
  }

  private get topUpDisabled(): boolean {
    return !this.hasWallet || this.requestingPayment || this.awaitingApproval;
  }

  updated(changed: Map<string, unknown>) {
    // When requestingPayment goes from true to false (request sent), enter awaiting approval
    if (changed.has('requestingPayment') && !this.requestingPayment && !this.paymentError) {
      const wasRequesting = changed.get('requestingPayment') as boolean;
      if (wasRequesting) {
        this.awaitingApproval = true;
        this.previousCredits = this.userInfo?.remainingCredits ?? null;
      }
    }
    // Detect credit change while awaiting approval
    if (changed.has('userInfo') && this.awaitingApproval && this.previousCredits !== null) {
      const newCredits = this.userInfo?.remainingCredits ?? 0;
      if (newCredits !== this.previousCredits) {
        this.awaitingApproval = false;
        this.creditsUpdated = true;
        this.previousCredits = null;
        setTimeout(() => { this.creditsUpdated = false; }, 4000);
      }
    }
    // Clear awaiting on error
    if (changed.has('paymentError') && this.paymentError) {
      this.awaitingApproval = false;
      this.previousCredits = null;
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
              Connected to <strong>${this.connectedHost.name}</strong>
            </div>
            <div class="host-badge">
              ${MapPinIcon()} ${this.connectedHost.location}
            </div>
          ` : ''}
          ${this.userInfo?.email ? html`
            <p class="email-display">${this.userInfo.email}</p>
          ` : ''}
        </div>

        <!-- Credit balance -->
        ${this.isFreeAccess ? html`
          <div class="credit-display free-access">
            ${CreditIcon()}
            <span class="credit-amount">Free Access</span>
          </div>
        ` : html`
          <div class="credit-display">
            ${CreditIcon()}
            <span class="credit-amount">${credits.toFixed(2)}</span>
            <span class="credit-label">HOT</span>
          </div>

          ${this.isDepleted ? html`
            <div class="depleted-banner">Credits depleted — top up to continue using this host</div>
          ` : ''}

          ${!this.hasWallet ? html`
            <p style="font-size:15px;color:rgba(255,255,255,0.5);text-align:center;">
              Set your wallet address to enable top-ups
            </p>
          ` : ''}

          <!-- Wallet address -->
          <div class="wallet-section">
            <label>${WalletIcon()} wHOT Wallet Address</label>
            ${this.hasWallet && !this.editingWallet ? html`
              <div class="wallet-row">
                <div class="wallet-display" title=${this.userInfo!.hotWalletAddress}>${this.truncateMiddle(this.userInfo!.hotWalletAddress || '')}</div>
                <button class="secondary" @click=${() => { this.editingWallet = true; this.walletInput = this.userInfo!.hotWalletAddress || ''; }}>
                  Change
                </button>
              </div>
              ${this.walletSaveSuccess ? html`
                <div class="success-message" style="display:flex;align-items:center;justify-content:center;gap:6px;">
                  ${CheckIcon()} Wallet saved
                </div>
              ` : ''}
            ` : html`
              <div class="wallet-row">
                <input
                  type="text"
                  placeholder="Enter your wHOT wallet address"
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

          <!-- Top-up -->
          <div class="topup-section">
            <label>${CreditIcon()} Top Up Credits</label>
            <div class="wallet-row">
              <input
                type="number"
                min="1"
                placeholder="Amount in wHOT"
                .value=${this.topUpAmount}
                @input=${(e: Event) => { this.topUpAmount = (e.target as HTMLInputElement).value; }}
                @keydown=${(e: KeyboardEvent) => { if (e.key === 'Enter' && this.topUpAmount && Number(this.topUpAmount) > 0) this.requestTopUp(Number(this.topUpAmount)); }}
                style="font-size: 14px;"
              />
              <button
                class="primary"
                ?disabled=${this.topUpDisabled || !this.topUpAmount || Number(this.topUpAmount) <= 0}
                @click=${() => this.requestTopUp(Number(this.topUpAmount))}
              >
                ${this.requestingPayment ? '...' : 'Top up'}
              </button>
            </div>
            <div class="topup-buttons">
              ${[100, 500, 1000].map(amount => html`
                <button
                  class="secondary"
                  ?disabled=${this.topUpDisabled}
                  @click=${() => { this.topUpAmount = String(amount); this.requestTopUp(amount); }}
                >
                  ${amount} wHOT
                </button>
              `)}
            </div>
          </div>

          ${this.awaitingApproval ? html`
            <div class="pending-message">
              <div class="spinner"></div>
              Open Unyt app and approve the transaction
            </div>
          ` : ''}

          ${this.creditsUpdated ? html`
            <div class="success-message" style="display:flex;align-items:center;justify-content:center;gap:6px;">
              ${CheckIcon()} Credits updated!
            </div>
          ` : ''}

          ${this.paymentError ? html`
            <div class="error-message">${this.paymentError}</div>
          ` : ''}
        `}

        <div class="footer-actions">
          <button class="primary" ?disabled=${this.isDepleted && !this.isFreeAccess} @click=${this.close}>
            Use app
          </button>
          <button class="danger-secondary" @click=${this.disconnect}>
            Disconnect
          </button>
        </div>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "logged-in-dashboard": LoggedInDashboard;
  }
}
