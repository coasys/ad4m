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
    loading
}) {
    // AUthenctication step
    if (step === 1) {
        return html`
            <div class="input">
                <label class="input__label">EMAIL</label>
                <input
                    class="input__field"
                    value=${email}
                    @input=${(e: any) => changeEmail(e.target.value)}
                />
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
                    ${loading && html`<div class="md-ring">
                        <div></div>
                        <div></div>
                        <div></div>
                        <div></div>
                    </div>`}
                    Start remote executor
                </a>
                <p>
                    This will spawn a new expecutor on our hosting service or restart one if its paused.
                </p>
            </div>
        `
    }
}