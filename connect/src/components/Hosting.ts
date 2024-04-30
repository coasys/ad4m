import { html } from "lit";

export default function Hosting({
    step,
    email,
    changeEmail,
    changeState,
    password,
    changePassword,
    login,
    checkEmail,
    passwordError,
    setHostingStep
}) {
    if (step === 0) {
        return html`
        <div class="input">
        <label class="input__label">EMAIL</label>
        <input
            class="input__field"
            value=${email}
            autofocus
            @input=${(e: any) => changeEmail(e.target.value)}
            @keydown=${(e: any) => e.key === "Enter" && checkEmail()}
        />
        <div class="buttons">
            <button
                class="button button--full button-secondary"
                @click=${() => changeState("start")}
            >
                Back
            </button>
            <button
                class="button button--full button-secondary"
                @click=${() => checkEmail()}
            >
                Next
            </button>
        </div>
    </div>
        `
    }

    // AUthenctication step
    if (step === 1) {
        return html`
            <div class="input">
                <label class="input__label">PASSWORD</label>
                <input
                    class="input__field"
                    value=${password}
                    @input=${(e: any) => changePassword(e.target.value)}
                    type="password"
                    errortext=${passwordError}
                    error=${passwordError}
                    autofocus
                    @keydown=${(e: any) => e.key === "Enter" && login()}
                />
                ${passwordError ? html`<p class="error">${passwordError}</p>` : ""}
                <div class="buttons">
                    <button
                        class="button button--full button-secondary"
                        @click=${() => setHostingStep(0)}
                    >
                        Back
                    </button>
                    <button
                        class="button button--full button-secondary"
                        @click=${() => login()}
                    >

                        Login
                    </button>
                </div>
            </div>
        `
    }

    if (step === 2) {
        return html`
            <div class="text-center">
                <p>
                    Email is not registered. Please follow the <a target="_blank" href="https://hosting.ad4m.dev/">link</a> to register.
                </p>
                <button
                    class="button button--full button-secondary"
                    @click=${() => {
                        setHostingStep(0)
                        changeState("start")
                    }}
                >
                    Back
                </button>
            </div>
        `
    }
}