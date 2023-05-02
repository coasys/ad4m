import { html } from "lit";

export default function Disconnected({ reconnect }) {
  return html`
    <div class="items items--small">
      <div class="text-center">
        <h1 class="heading">Could not connect to AD4M</h1>
      </div>
      <div class="text-center">
        <button class="button" @click=${() => reconnect()}>Try again</button>
      </div>
    </div>
  `;
}
