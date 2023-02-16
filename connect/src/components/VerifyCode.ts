import { html } from "lit";

export default function Request({ code, changeState, verifyCode, changeCode }) {
  return html`
    <div class="items">
      <div class="text-center">
        <div class="heading">Please check Ad4m</div>
        <p class="body">
          Please check AD4M, confirm the request there and enter the 6-digit
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
          Cancel
        </button>
        <button class="button button--full" @click=${() => verifyCode(code)}>
          Continue
        </button>
      </div>
    </div>
  `;
}
