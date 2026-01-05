import { html } from "lit";

type RemoteConnectionProps = {
  initialUrl?: string;
  detecting: boolean;
  multiUserDetected: boolean | null;
  error: string | null;
  onBack: () => void;
  onUrlChange: (url: string) => void;
  onConnect: () => void;
  onMultiUserAuth: () => void;
  onRequestCapability: () => void;
};

export default function RemoteConnection({
  initialUrl,
  detecting,
  multiUserDetected,
  error,
  onBack,
  onUrlChange,
  onConnect,
  onMultiUserAuth,
  onRequestCapability,
}: RemoteConnectionProps) {
  const showAuthOptions = multiUserDetected !== null && !detecting;
  const urlValue = initialUrl || "";

  return html`
    <div class="items">
      <div class="text-center">
        <h3 class="heading">Connect to Remote AD4M</h3>
        <p class="body">Enter the URL of a remote AD4M executor</p>
      </div>

      ${!showAuthOptions
        ? html`
            <div class="items items--small">
              <div class="input">
                <label class="input__label" for="remote-url">
                  Executor URL
                </label>
                <input
                  id="remote-url"
                  type="text"
                  class="input__field"
                  placeholder="wss://your-server.com/graphql"
                  .value=${urlValue}
                  @input=${(e: Event) =>
                    onUrlChange((e.target as HTMLInputElement).value)}
                  ?disabled=${detecting}
                />
              </div>

              ${error
                ? html`
                    <div class="error-message">
                      <p>${error}</p>
                    </div>
                  `
                : ""}

              <div class="buttons">
                <button
                  class="button button--secondary"
                  @click=${onBack}
                  ?disabled=${detecting}
                >
                  Back
                </button>
                <button
                  class="button"
                  @click=${onConnect}
                  ?disabled=${detecting || !urlValue}
                >
                  ${detecting ? "Detecting..." : "Connect"}
                </button>
              </div>
            </div>
          `
        : html`
            <div class="items items--small">
              <div class="remote-info">
                <p class="body" style="margin: 0 0 5px 0; font-size: 13px;">
                  Connected to:
                </p>
                <p
                  class="body"
                  style="margin: 0; font-family: monospace; font-size: 12px; opacity: 0.8;"
                >
                  ${urlValue}
                </p>
              </div>

              ${multiUserDetected
                ? html`
                    <div class="auth-option auth-option--primary">
                      <div class="auth-option__header">
                        <svg
                          width="20"
                          height="20"
                          viewBox="0 0 24 24"
                          fill="none"
                          stroke="currentColor"
                        >
                          <path
                            d="M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2"
                          ></path>
                          <circle cx="9" cy="7" r="4"></circle>
                          <path d="M23 21v-2a4 4 0 0 0-3-3.87"></path>
                          <path d="M16 3.13a4 4 0 0 1 0 7.75"></path>
                        </svg>
                        <h4 class="heading nomargin" style="font-size: 16px;">
                          Multi-User Login
                        </h4>
                      </div>
                      <p
                        class="body"
                        style="margin: 10px 0 15px 0; font-size: 13px;"
                      >
                        This executor supports multi-user authentication. Sign
                        in or create an account.
                      </p>
                      <button class="button button--full" @click=${onMultiUserAuth}>
                        Login / Sign Up
                      </button>
                    </div>

                    <div class="auth-option">
                      <div class="auth-option__header">
                        <svg
                          width="20"
                          height="20"
                          viewBox="0 0 24 24"
                          fill="none"
                          stroke="currentColor"
                        >
                          <rect
                            x="3"
                            y="11"
                            width="18"
                            height="11"
                            rx="2"
                            ry="2"
                          ></rect>
                          <path d="M7 11V7a5 5 0 0 1 10 0v4"></path>
                        </svg>
                        <h4 class="heading nomargin" style="font-size: 16px;">
                          Capability Request
                        </h4>
                      </div>
                      <p
                        class="body"
                        style="margin: 10px 0 15px 0; font-size: 13px;"
                      >
                        Request access to the main agent with a verification
                        code.
                      </p>
                      <button
                        class="button button--secondary button--full"
                        @click=${onRequestCapability}
                      >
                        Request Capability
                      </button>
                    </div>
                  `
                : html`
                    <div class="auth-option auth-option--primary">
                      <div class="auth-option__header">
                        <svg
                          width="20"
                          height="20"
                          viewBox="0 0 24 24"
                          fill="none"
                          stroke="currentColor"
                        >
                          <rect
                            x="3"
                            y="11"
                            width="18"
                            height="11"
                            rx="2"
                            ry="2"
                          ></rect>
                          <path d="M7 11V7a5 5 0 0 1 10 0v4"></path>
                        </svg>
                        <h4 class="heading nomargin" style="font-size: 16px;">
                          Single-User Executor
                        </h4>
                      </div>
                      <p
                        class="body"
                        style="margin: 10px 0 15px 0; font-size: 13px;"
                      >
                        This executor uses capability-based authentication.
                        Request access with a verification code.
                      </p>
                      <button
                        class="button button--full"
                        @click=${onRequestCapability}
                      >
                        Request Capability
                      </button>
                    </div>
                  `}

              <div class="buttons">
                <button
                  class="button button--link"
                  @click=${onBack}
                  style="margin: 0 auto;"
                >
                  ← Back to connection options
                </button>
              </div>
            </div>
          `}
    </div>

    <style>
      .remote-info {
        background: rgba(145, 227, 253, 0.05);
        border: 1px solid rgba(145, 227, 253, 0.2);
        border-radius: 8px;
        padding: 15px;
      }

      .auth-option {
        background: rgba(145, 227, 253, 0.05);
        border: 1px solid rgba(145, 227, 253, 0.2);
        border-radius: 8px;
        padding: 20px;
        transition: all 0.3s ease;
      }

      .auth-option:hover {
        border-color: rgba(145, 227, 253, 0.4);
        background: rgba(145, 227, 253, 0.08);
      }

      .auth-option--primary {
        border: 2px solid var(--gradient);
        background: rgba(145, 227, 253, 0.1);
      }

      .auth-option--primary:hover {
        background: rgba(145, 227, 253, 0.15);
      }

      .auth-option__header {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 5px;
      }

      .auth-option__header svg {
        flex-shrink: 0;
        opacity: 0.8;
      }

      .auth-option--primary .auth-option__header svg {
        color: var(--gradient);
        opacity: 1;
      }

      .error-message {
        background-color: rgba(255, 0, 0, 0.1);
        border: 1px solid rgba(255, 0, 0, 0.3);
        border-radius: 8px;
        padding: 15px;
        color: #ff6b6b;
        text-align: center;
      }

      .error-message p {
        margin: 0;
      }
    </style>
  `;
}
