import { html } from "lit";

export default function AgentLocked({ unlockAgent, connectToPort }) {
  return html`
    <div class="text-center">
      <div class="items">
        <div>
          <h1 class="heading">Agent locked</h1>
          <p class="body">
            Your agent is locked, please unlock it & refresh the page to
            continue.
          </p>
        </div>
        <button class="button" @click=${() => connectToPort()}>Connect</button>
      </div>
    </div>
  `;
}
