import { html } from "lit";
import Logo from "./Logo";
import { capSentence } from "@coasys/ad4m";

export default function CapNotMatchedFirst({
  capabilities,
  appname,
  appiconpath,
  changeState,
  setOpen,
  requestCapability,
}) {
  return html`
    <div class="items">
      <div class="text-center">
        <p class="body">An external application</p>
        <h1 class="heading nomargin">${appname}</h1>
        <p class="body">wants to access your AD4M data</p>
      </div>
      ${appiconpath &&
      html`<div class="dialog__connect">
        <img class="dialog__connect-app" src=${appiconpath} alt="Logo" />
        <div class="dialog__connect-check"></div>
        <div class="dialog__connect-ad4m">${Logo()}</div>
      </div>`}
      <div>
        <p class="text-center"><b>This will allow the developer to:</b></p>
        <ul class="check-list">
          ${capabilities.map(
            (cap) => html`<li>
              <span>
                <svg
                  xmlns="http://www.w3.org/2000/svg"
                  width="20"
                  height="20"
                  fill="var(--success-color)"
                  viewBox="0 0 16 16"
                >
                  <path
                    d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zm-3.97-3.03a.75.75 0 0 0-1.08.022L7.477 9.417 5.384 7.323a.75.75 0 0 0-1.06 1.06L6.97 11.03a.75.75 0 0 0 1.079-.02l3.992-4.99a.75.75 0 0 0-.01-1.05z"
                  />
                </svg>
              </span>
              <span>${capSentence(cap)}</span>
            </li>`
          )}
        </ul>
      </div>
      <div class="buttons">
        <button
          class="button button--secondary button--full"
          @click=${() => setOpen(false)}
        >
          Cancel
        </button>
        <button
          class="button button--full"
          @click=${() => requestCapability(true)}
        >
          Authorize
        </button>
      </div>
      <button
        class="button button--full button--link"
        @click=${() => changeState("settings")}
      >
        Connection settings
      </button>
    </div>
  `;
}
