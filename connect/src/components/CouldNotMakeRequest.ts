import { html } from "lit";

export default function CouldNotMakeRequest() {
  return html`
    <div class="items">
      <div class="text-center">
        <h1 class="heading">Request to AD4M blocked</h1>
        <div class="body">
          Its possible your browser is blocking requests to your local machine.
          Please remove any browser shields/protection that may be running for
          this page. Alternatively allow requests to localhost in your browser
          settings.
        </div>
      </div>

      <div class="buttons">
        <button
          class="button button--full button--secondary"
          @click=${() => location.reload()}
        >
          Try again
        </button>
      </div>
    </div>
  `;
}
