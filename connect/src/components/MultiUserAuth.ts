import { html } from "lit";

type MultiUserAuthProps = {
  email: string;
  password: string;
  error: string | null;
  isLoading: boolean;
  changeEmail: (email: string) => void;
  changePassword: (password: string) => void;
  onLogin: () => void;
  onSignup: () => void;
  activeTab: "login" | "signup";
  setActiveTab: (tab: "login" | "signup") => void;
};

export default function MultiUserAuth({
  email,
  password,
  error,
  isLoading,
  changeEmail,
  changePassword,
  onLogin,
  onSignup,
  activeTab,
  setActiveTab,
}: MultiUserAuthProps) {
  return html`
    <div class="items">
      <div class="text-center">
        <h3 class="heading">Welcome to ${window.location.hostname}</h3>
        <p class="body">Please sign in or create an account to continue</p>
      </div>

      <!-- Tab Switcher -->
      <div class="tab-switcher">
        <button
          class="tab-button ${activeTab === "login" ? "active" : ""}"
          @click=${() => setActiveTab("login")}
        >
          Login
        </button>
        <button
          class="tab-button ${activeTab === "signup" ? "active" : ""}"
          @click=${() => setActiveTab("signup")}
        >
          Sign Up
        </button>
      </div>

      ${error
        ? html`
            <div class="error-message">
              <p>${error}</p>
            </div>
          `
        : ""}

      <!-- Login/Signup Form -->
      <div class="items items--small">
        <div class="input">
          <label class="input__label" for="email">Email</label>
          <input
            id="email"
            type="email"
            class="input__field"
            placeholder="your@email.com"
            .value=${email}
            @input=${(e: Event) =>
              changeEmail((e.target as HTMLInputElement).value)}
            ?disabled=${isLoading}
          />
        </div>

        <div class="input">
          <label class="input__label" for="password">Password</label>
          <input
            id="password"
            type="password"
            class="input__field"
            placeholder="Enter password"
            .value=${password}
            @input=${(e: Event) =>
              changePassword((e.target as HTMLInputElement).value)}
            @keypress=${(e: KeyboardEvent) => {
              if (e.key === "Enter") {
                if (activeTab === "login") {
                  onLogin();
                } else {
                  onSignup();
                }
              }
            }}
            ?disabled=${isLoading}
          />
        </div>
      </div>

      <div class="buttons">
        ${activeTab === "login"
          ? html`
              <button
                class="button button--full"
                @click=${onLogin}
                ?disabled=${isLoading || !email || !password}
              >
                ${isLoading ? "Logging in..." : "Login"}
              </button>
            `
          : html`
              <button
                class="button button--full"
                @click=${onSignup}
                ?disabled=${isLoading || !email || !password}
              >
                ${isLoading ? "Creating account..." : "Sign Up"}
              </button>
            `}
      </div>

      ${activeTab === "login"
        ? html`
            <div class="text-center">
              <p class="body">
                Don't have an account?
                <button
                  class="button button--link"
                  @click=${() => setActiveTab("signup")}
                >
                  Sign up
                </button>
              </p>
            </div>
          `
        : html`
            <div class="text-center">
              <p class="body">
                Already have an account?
                <button
                  class="button button--link"
                  @click=${() => setActiveTab("login")}
                >
                  Log in
                </button>
              </p>
            </div>
          `}
    </div>

    <style>
      .tab-switcher {
        display: flex;
        gap: 10px;
        justify-content: center;
        border-bottom: 1px solid var(--body-color);
        padding-bottom: 10px;
      }

      .tab-button {
        background: none;
        border: none;
        color: var(--body-color);
        padding: 10px 20px;
        cursor: pointer;
        font-size: 16px;
        font-weight: 500;
        border-bottom: 2px solid transparent;
        transition: all 0.3s ease;
      }

      .tab-button.active {
        color: var(--gradient);
        border-bottom-color: var(--gradient);
      }

      .tab-button:hover {
        color: var(--primary-color);
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
