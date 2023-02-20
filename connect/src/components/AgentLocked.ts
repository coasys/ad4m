import { html } from "lit";

export default function AgentLocked({ unlockAgent, reconnect }) {
  return html`
    <div class="text-center">
      <div class="items">
        <div>
          <h1 class="heading">Agent locked</h1>
          <p class="body">
            Your agent is locked, please unlock it & press reconnect.
          </p>
        </div>
        <button class="button" @click=${reconnect}>Reconnect</button>
      </div>
    </div>
  `;
}
