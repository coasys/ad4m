import { html } from "lit";

export default function Settings({
  port,
  url,
  isRemote,
  connectToPort,
  changeIsRemote,
  connectRemote,
  changeState,
  changeUrl,
  changePort,
  clearState,
}) {
  const proxyClasses = `button ${
    isRemote ? "button--primary" : "button--secondary"
  }`;

  const localClasses = `button ${
    isRemote ? "button--secondary" : "button--primary"
  }`;

  return html`
    <div class="items items--small">
      <div class="text-center">
        <h1 class="heading">Connection Settings</h1>
        <p class="body">Please choose how you want to connect to AD4M</p>
      </div>
      <div style="display: flex; gap: 10px;">
        <button class=${localClasses} @click=${() => changeIsRemote(false)}>
          Locally
        </button>
        <button class=${proxyClasses} @click=${() => changeIsRemote(true)}>
          Remotely
        </button>
      </div>
      ${isRemote ? html`
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
            class="button button--full"
            @click=${() => connectRemote(url)}
          >
            Connect
          </button>
        </div>
      ` : html`
        <div class="input">
          <label class="input__label">PORT</label>
          <input
            class="input__field"
            value=${port}
            @input=${(e: any) => changePort(parseInt(e.target.value))}
          />
        </div>

        <div class="buttons">
          <button
            class="button button--full"
            @click=${() => connectToPort(port)}
          >
            Connect
          </button>
        </div>
      `}

      <div class="text-center">or</div>
      <div class="buttons">
        <button
          class="button button--full button--secondary"
          @click=${() => changeState("start")}
        >
          Back
        </button>
        <button 
          class="button button--full button--secondary"
          @click=${clearState}
        >
          Clear state
        </button>
      </div
    </div>
  `;
}
