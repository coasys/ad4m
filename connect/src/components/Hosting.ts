import { html } from "lit";

export default function Hosting({
    step,
    email,
    changeEmail,
    changeState,
    password,
    changePassword,
    login,
    startService,
    loading,
    checkEmail
}) {
    if (step === 0) {
        return html`
        <div class="input">
        <label class="input__label">EMAIL</label>
        <input
            class="input__field"
            value=${email}
            @input=${(e: any) => changeEmail(e.target.value)}
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
                        @click=${() => login()}
                    >

                        Login
                    </button>
                </div>
            </div>
        `
    }
    
    // Starting executor step if not working
    if (step === 2) {
        return html`
            <div class="text-center">
                <a class="button" target="_blank"  @click=${() => !loading ? startService() : null}>
                    ${loading ? html`<div class="md-ring">
                        <div></div>
                        <div></div>
                        <div></div>
                        <div></div>
                    </div>`: html`<div></div>`}
                    Start remote executor
                </a>
                <p>
                    This will spawn a new expecutor on our hosting service or restart one if its paused.
                </p>
            </div>
        `
    }

    if (step === 3) {
        return html`
            <div class="text-center">
                <p>
                    Email is not registered. Please follow the <a target="_blank" href="https://hosting.ad4m.dev/signup">link</a> to register.
                </p>
                <button
                    class="button button--full button-secondary"
                    @click=${() => changeState("start")}
                >
                    Back
                </button>
            </div>
        `
    }
}