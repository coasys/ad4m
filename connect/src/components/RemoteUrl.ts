import { html } from "lit";

export default function RemoveUrl({
  url,
  connectRemote,
  changeState,
  changeUrl,
}) {
  return html`
    <div class="items items--small">
      <div class="text-center">
        <h1 class="heading">Connect to a remote host</h1>
        <p class="body">Please enter the url you want to connect to</p>
      </div>
      <div class="input">
        <label class="input__label">URL</label>
        <input
          class="input__field"
          value=${url}
          @input=${(e: any) => changeUrl(e.target.value)}
        />
      </div>

      <div class="buttons">
        <button
          class="button button--full button--secondary"
          @click=${() => changeState("start")}
        >
          Back
        </button>
        <button class="button button--full" @click=${() => connectRemote(url)}>
          Continue
        </button>
      </div>
    </div>
  `;
}
