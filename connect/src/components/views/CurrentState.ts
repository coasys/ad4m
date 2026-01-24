import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";
import { sharedStyles } from "../../styles/shared-styles";
import { CrossIcon, LocalIcon, RemoteIcon } from "../icons";
import type { AuthStates } from "../../types";

@customElement("current-state")
export class CurrentState extends LitElement {
  @property({ type: String }) url: string = "";
  @property({ type: Number }) port: number = 12000;
  @property({ type: String }) authState: AuthStates = "unauthenticated";

  static styles = [
    sharedStyles,
    css`
      .box {
        gap: 15px;
      }

      .box-header {
        margin-bottom: 6px;
      }

      .detail-value {
        color: var(--ac-primary-color);
      }

      .auth-status {
        padding: 6px 10px;
        border-radius: 8px;
        font-size: 14px;
        font-weight: 600;
        text-transform: uppercase;
      }

      .auth-status.authenticated {
        background: rgba(93, 210, 125, 0.2);
        color: var(--ac-success-color);
      }

      .auth-status.locked {
        background: rgba(255, 193, 7, 0.2);
        color: #ffc107;
      }

      .auth-status.unauthenticated {
        background: rgba(244, 54, 127, 0.2);
        color: var(--ac-danger-color);
      }
    `
  ];

  private get connectionType(): 'local' | 'remote' {
    return this.url.includes('localhost') || this.url.includes('127.0.0.1') 
      ? 'local' 
      : 'remote';
  }

  private close() {
    this.dispatchEvent(new CustomEvent("close", { bubbles: true, composed: true }));
  }

  private disconnect() {
    this.dispatchEvent(new CustomEvent("disconnect", { bubbles: true, composed: true }));
  }

  render() {
    const isLocal = this.connectionType === 'local';

    return html`
      <div class="container">
        <div class="close-button" @click=${this.close}>
          ${CrossIcon()}
        </div>

        <div class="header">
          <h1>AD4M Connected</h1>
          <h3>Your current connection details:</h3>
        </div>

        <div class="box">
          <div class="box-header">
            ${isLocal ? LocalIcon() : RemoteIcon()}
            <h3>${isLocal ? 'Local Node' : 'Remote Node'}</h3>
          </div>

          ${isLocal 
            ? html`
                <div class="row">
                  <span>Port:</span>
                  <span class="detail-value">${this.port}</span>
                </div>
                <div class="row">
                  <span>URL:</span>
                  <span class="detail-value">${this.url}</span>
                </div>
              `
            : html`
                <div class="row">
                  <span>URL:</span>
                  <span class="detail-value">${this.url}</span>
                </div>
              `
          }

          <div class="row">
            <span>Status:</span>
            <span class="auth-status ${this.authState}">${this.authState}</span>
          </div>
        </div>

        <div class="row center">
          <button class="danger" @click=${this.disconnect}>
            Disconnect
          </button>
        </div>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "current-state": CurrentState;
  }
}
