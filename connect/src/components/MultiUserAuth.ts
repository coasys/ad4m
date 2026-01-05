import { html } from "lit";

type MultiUserAuthProps = {
  email: string;
  password: string;
  verificationCode: string;
  error: string | null;
  isLoading: boolean;
  backendUrl?: string;
  step: "email" | "password" | "code";
  verificationType: "signup" | "login";
  changeEmail: (email: string) => void;
  changePassword: (password: string) => void;
  changeVerificationCode: (code: string) => void;
  onEmailSubmit: () => void;
  onPasswordSubmit: () => void;
  onCodeSubmit: () => void;
  onBackToEmail: () => void;
};

export default function MultiUserAuth({
  email,
  password,
  verificationCode,
  error,
  isLoading,
  backendUrl,
  step,
  verificationType,
  changeEmail,
  changePassword,
  changeVerificationCode,
  onEmailSubmit,
  onPasswordSubmit,
  onCodeSubmit,
  onBackToEmail,
}: MultiUserAuthProps) {
  // Email step - unified entry point
  if (step === "email") {
    return html`
      <div class="items">
        <div class="text-center">
          <h3 class="heading">Sign in or Sign up</h3>
          <p class="body">Enter your email to continue</p>
          ${backendUrl
            ? html`
                <p
                  class="body"
                  style="margin-top: 10px; font-size: 12px; opacity: 0.7; font-family: monospace;"
                >
                  ${backendUrl}
                </p>
              `
            : ""}
        </div>

        ${error
          ? html`
              <div class="error-message">
                <p>${error}</p>
              </div>
            `
          : ""}

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
              @keypress=${(e: KeyboardEvent) => {
                if (e.key === "Enter" && email && !isLoading) {
                  onEmailSubmit();
                }
              }}
              ?disabled=${isLoading}
              autofocus
            />
          </div>
        </div>

        <div class="buttons">
          <button
            class="button button--full"
            @click=${onEmailSubmit}
            ?disabled=${isLoading || !email}
          >
            ${isLoading ? "Checking..." : "Continue"}
          </button>
        </div>
      </div>

      <style>
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

  // Password step - for new users only
  if (step === "password") {
    return html`
      <div class="items">
        <div class="text-center">
          <h3 class="heading">Create your account</h3>
          <p class="body">Email: <strong>${email}</strong></p>
          <p class="body" style="margin-top: 10px;">
            Choose a password to create your account
          </p>
        </div>

        ${error
          ? html`
              <div class="error-message">
                <p>${error}</p>
              </div>
            `
          : ""}

        <div class="items items--small">
          <div class="input">
            <label class="input__label" for="password">Password</label>
            <input
              id="password"
              type="password"
              class="input__field"
              placeholder="Enter a strong password"
              .value=${password}
              @input=${(e: Event) =>
                changePassword((e.target as HTMLInputElement).value)}
              @keypress=${(e: KeyboardEvent) => {
                if (e.key === "Enter" && password && !isLoading) {
                  onPasswordSubmit();
                }
              }}
              ?disabled=${isLoading}
              autofocus
            />
          </div>
        </div>

        <div class="buttons">
          <button
            class="button button--full"
            @click=${onPasswordSubmit}
            ?disabled=${isLoading || !password}
          >
            ${isLoading ? "Creating account..." : "Create Account"}
          </button>
        </div>

        <div class="text-center">
          <button
            class="button button--link"
            @click=${onBackToEmail}
            ?disabled=${isLoading}
          >
            ← Back
          </button>
        </div>
      </div>

      <style>
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

  // Code verification step
  if (step === "code") {
    return html`
      <div class="items">
        <div class="text-center">
          <h3 class="heading">Check your email</h3>
          <p class="body">
            We've sent a 6-digit verification code to
            <strong>${email}</strong>
          </p>
          <p class="body" style="margin-top: 10px; font-size: 12px; opacity: 0.7;">
            The code will expire in 15 minutes
          </p>
        </div>

        ${error
          ? html`
              <div class="error-message">
                <p>${error}</p>
              </div>
            `
          : ""}

        <div class="items items--small">
          <div class="input">
            <label class="input__label" for="code">Verification Code</label>
            <input
              id="code"
              type="text"
              inputmode="numeric"
              pattern="[0-9]*"
              maxlength="6"
              class="input__field code-input"
              placeholder="000000"
              .value=${verificationCode}
              @input=${(e: Event) => {
                const input = e.target as HTMLInputElement;
                // Only allow digits
                const cleaned = input.value.replace(/\D/g, "");
                changeVerificationCode(cleaned);
                // Auto-submit when 6 digits entered
                if (cleaned.length === 6) {
                  setTimeout(() => onCodeSubmit(), 100);
                }
              }}
              ?disabled=${isLoading}
              autofocus
            />
          </div>
        </div>

        <div class="buttons">
          <button
            class="button button--full"
            @click=${onCodeSubmit}
            ?disabled=${isLoading || verificationCode.length !== 6}
          >
            ${isLoading ? "Verifying..." : "Verify Code"}
          </button>
        </div>

        <div class="text-center">
          <p class="body help-text">
            Didn't receive the email? Check your spam folder or
            <button
              class="button button--link"
              @click=${onBackToEmail}
              ?disabled=${isLoading}
            >
              try again
            </button>
          </p>
        </div>
      </div>

      <style>
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

        .code-input {
          text-align: center;
          font-size: 32px !important;
          letter-spacing: 8px;
          font-weight: 600;
        }

        .help-text {
          font-size: 12px;
          opacity: 0.8;
        }
      </style>
    `;
  }

  return html``;
}
