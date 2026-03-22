import { html } from "lit";

export default function CreditIcon() {
  return html`
    <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
      <circle cx="12" cy="12" r="10"></circle>
      <path d="M12 6v12"></path>
      <path d="M15.5 9.5a3 3 0 0 0-3-2.5H11a3 3 0 0 0 0 6h2a3 3 0 0 1 0 6h-1.5a3 3 0 0 1-3-2.5"></path>
    </svg>
  `;
}
