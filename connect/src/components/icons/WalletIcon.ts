import { html } from "lit";

export default function WalletIcon() {
  return html`
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
      <rect x="2" y="6" width="20" height="14" rx="2"></rect>
      <path d="M2 10h20"></path>
      <circle cx="16" cy="14" r="1" fill="currentColor"></circle>
    </svg>
  `;
}
