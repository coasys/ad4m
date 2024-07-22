import { html } from "lit";

export default function Request({ code, changeState, verifyCode, changeCode, isHosting }) {
  return html`
    <div class="items">
      <div class="text-center">
        <div class="heading">${isHosting ? "Please check your registered email for hosting" : "Please check Ad4m"}</div>
        <p class="body">
          ${isHosting ? "Please check your registered email for hosting" : "Please check Ad4m"}, confirm the request there and enter the 6-digit
          security code below.
        </p>
      </div>
      <div class="input">
        <label class="input__label">Security code</label>
        <input
          type="tel"
          placeholder="XXXXXX"
          class="input__field"
          value=${code}
          @change=${(e: any) => changeCode(e.target.value)}
        />
      </div>
      <div class="buttons">
        <button
          class="button button--full button--secondary"
          @click=${() => changeState("start")}
        >
          Back
        </button>
        <button class="button button--full" @click=${() => verifyCode(code)}>
          Continue
        </button>
      </div>
    </div>
  `;
}
