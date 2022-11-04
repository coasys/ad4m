import { html } from "lit";
import Logo from "./Logo";

export default function CapNotMatchedFirst({
  capabilities,
  appname,
  appiconpath,
  changeState,
  requestCapability,
}) {
  return html`
    <div class="items">
      <div class="text-center">
        <h1 class="heading">Authorize ${appname}</h1>
        <p class="body">${appname} want you to grant it these capabilities</p>
        <div>
          ${JSON.parse(capabilities).map(
            (e) =>
              html`<li>${e.can} => ${e.with.domain}.${e.with.pointers}</li>`
          )}
        </div>
      </div>
      ${appiconpath &&
      html`<div class="dialog__connect">
        <img class="dialog__connect-app" src=${appiconpath} alt="Logo" />
        <div class="dialog__connect-check">
          <svg
            xmlns="http://www.w3.org/2000/svg"
            width="30"
            height="30"
            fill="var(--success-color)"
            viewBox="0 0 16 16"
          >
            <path
              d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zm-3.97-3.03a.75.75 0 0 0-1.08.022L7.477 9.417 5.384 7.323a.75.75 0 0 0-1.06 1.06L6.97 11.03a.75.75 0 0 0 1.079-.02l3.992-4.99a.75.75 0 0 0-.01-1.05z"
            />
          </svg>
        </div>
        <div class="dialog__connect-ad4m">${Logo()}</div>
      </div>`}
      <div class="buttons">
        <button
          class="button button--full button--secondary"
          @click=${() => changeState(null)}
        >
          Close
        </button>
        <button
          class="button button--full"
          @click=${() => requestCapability(true)}
        >
          Authorize
        </button>
      </div>
    </div>
  `;
}
