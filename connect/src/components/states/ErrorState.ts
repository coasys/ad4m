import { html } from "lit";

type ErrorType = 'agent-locked' | 'disconnected' | 'request-blocked' | 'invalid-token';

interface ErrorConfig {
  heading: string;
  message: string | null;
  buttonText: string;
}

const ERROR_CONFIGS: Record<ErrorType, ErrorConfig> = {
  'agent-locked': {
    heading: 'Agent locked',
    message: 'Your agent is locked, please unlock it & press reconnect.',
    buttonText: 'Reconnect'
  },
  'disconnected': {
    heading: 'Could not connect to AD4M',
    message: null,
    buttonText: 'Try again'
  },
  'request-blocked': {
    heading: 'Request to AD4M blocked',
    message: 'It\'s possible your browser is blocking requests to your local machine. Please remove any browser shields/protection that may be running for this page. Alternatively allow requests to localhost in your browser settings.',
    buttonText: 'Try again'
  },
  'invalid-token': {
    heading: 'Your session has expired',
    message: 'AD4M needs to grant you permissions again.',
    buttonText: 'Request permissions'
  }
};

export default function ErrorState({ 
  type,
  onAction 
}: {
  type: ErrorType;
  onAction: () => void;
}) {
  const config = ERROR_CONFIGS[type];

  return html`
    <div class="items ${type === 'disconnected' ? 'items--small' : ''}">
      <div class="text-center">
        <h1 class="heading">${config.heading}</h1>
        ${config.message ? html`
          <div class="body">${config.message}</div>
        ` : ''}
      </div>
      
      ${type === 'request-blocked' ? html`
        <div class="buttons">
          <button
            class="button button--full button--secondary"
            @click=${onAction}
          >
            ${config.buttonText}
          </button>
        </div>
      ` : html`
        <div class="text-center">
          <button class="button" @click=${onAction}>
            ${config.buttonText}
          </button>
        </div>
      `}
    </div>
  `;
}
