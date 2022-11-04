import { html } from "lit";

export default function Disconnected({ connectToPort }) {
  return html`
    <div class="items items--small">
      <div class="text-center">
        <h1 class="heading">Disconnected</h1>
      </div>
      <div class="text-center">
        <button class="button" @click=${() => connectToPort()}>
          Reconnect
        </button>
      </div>
    </div>
  `;
}
