import { css } from "lit";

/**
 * Shared styles for all ad4m-connect components
 * Includes typography, buttons, inputs, and common utilities
 */
export const sharedStyles = css`
  * {
    box-sizing: border-box;
  }

  /* Typography */
  h1, h2, h3, p {
    margin: 0;
    text-align: center;
  }

  h1 {
    font-size: 28px;
    line-height: 1;
    font-weight: 700;
    margin-bottom: 20px;
    color: #ffffff;
  }

  h2 {
    font-size: 22px;
    line-height: 1;
    font-weight: 700;
    margin-bottom: 20px;
    color: #ffffff;
  }

  h3 {
    font-size: 18px;
    line-height: 1;
    font-weight: 400;
    color: #f0f0f0;
  }

  p {
    margin: 0;
    font-size: 16px;
    line-height: 1.5;
    color: rgba(255, 255, 255, 0.8);
  }

  label {
    font-size: 14px;
    font-weight: 500;
    color: rgba(255, 255, 255, 0.9);
  }

  /* Buttons */
  button {
    display: flex;
    justify-content: center;
    align-items: center;
    gap: 12px;
    height: 48px;
    padding: 0 24px;
    border-radius: 8px;
    font-size: 16px;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.2s ease;
    border: none;
    outline: none;
  }

  button:focus-visible {
    outline: 2px solid var(--ac-primary-color);
    outline-offset: 2px;
  }

  button svg {
    width: 25px;
    height: 25px;
  }

  button.primary {
    background: var(--ac-primary-color);
    color: #000000;
  }

  button.primary:hover:not(:disabled) {
    background: var(--ac-primary-color-light);
  }

  button.primary:focus-visible:not(:disabled) {
    outline: 2px solid var(--ac-primary-color-light);
    outline-offset: 2px;
    box-shadow: 0 0 0 4px rgba(255, 255, 255, 0.2);
  }

  button.secondary {
    background: #00091e5c;
    color: var(--ac-primary-color);
    border: 1px solid var(--ac-primary-color);
  }

  button.secondary:hover:not(:disabled) {
    background: #00091e73;
    color: var(--ac-primary-color-light);
  }

  button.secondary:focus-visible:not(:disabled) {
    outline: 2px solid var(--ac-primary-color-light);
    outline-offset: 2px;
    box-shadow: 0 0 0 4px rgba(255, 255, 255, 0.2);
  }

  button.ghost {
    background: transparent;
    color: rgba(255, 255, 255, 0.7);
  }

  button.ghost:hover:not(:disabled) {
    color: #ffffff;
    background: rgba(255, 255, 255, 0.05);
  }

  button.ghost:focus-visible:not(:disabled) {
    outline: 2px solid rgba(255, 255, 255, 0.7);
    outline-offset: 2px;
    background: rgba(255, 255, 255, 0.05);
  }

  button.link {
    background: transparent;
    color: var(--ac-primary-color);
    text-decoration: none;
    height: auto;
    padding: 12px 24px;
  }

  button.link:hover {
    text-decoration: underline;
  }

  button.link:focus-visible {
    outline: 2px solid var(--ac-primary-color);
    outline-offset: 2px;
    text-decoration: underline;
  }

  button:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  button.full {
    width: 100%;
  }

  /* Inputs */
  input {
    width: 100%;
    height: 48px;
    padding: 0 16px;
    border-radius: 8px;
    box-shadow: 0 0 0 1px var(--ac-border-color-light);
    background: #00091e5c;
    color: #ffffff;
    font-size: 18px;
    border: none;
    outline: none;
    transition: all 0.2s ease;
  }

  input:focus, input:hover {
    box-shadow: 0 0 0 1px var(--ac-primary-color);
    background: #00091e73;
  }

  input:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  input::placeholder {
    color: rgba(255, 255, 255, 0.4);
  }

  input[type="number"]::-webkit-inner-spin-button,
  input[type="number"]::-webkit-outer-spin-button {
    -webkit-appearance: none;
    margin: 0;
  }

  input[type="number"] {
    -moz-appearance: textfield;
  }

  /* Common layout utilities */
  .row {
    display: flex;
    align-items: center;
    gap: 12px;
  }
    
  .header {
    text-align: center;
  }

  .container {
    display: flex;
    flex-direction: column;
    gap: 30px;
  }

  .box {
    display: flex;
    flex-direction: column;
    align-items: center;
    background: rgba(128, 178, 201, 0.14);
    box-shadow: 0 0 0 1px var(--ac-border-color-light);
    border-radius: 8px;
    padding: 20px;
    gap: 25px;
    transition: all 0.3s ease;
  }

  .state {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .state.danger {
    margin-bottom: -8px;
  }

  .state.success svg {
    width: 26px;
    height: 26px;
  }

  .state.danger svg {
    width: 30px;
    height: 30px;
  }

  .state.success p,
  .state.success svg {
    color: var(--ac-success-color);
  }

  .state.danger p,
  .state.danger svg {
    color: var(--ac-danger-color);
  }

  .back-button {
    position: absolute;
    top: 20px;
    left: 20px;
    cursor: pointer;
  }

  .back-button svg {
    width: 28px;
    height: 28px;
    color: white;
    opacity: 0.5;
  }

  .button-row {
    display: flex;
    gap: 12px;
  }

  .center {
    display: flex;
    justify-content: center;
  }

  .form-group {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  /* Error and info boxes */
  .error-box {
    padding: 16px;
    background: rgba(239, 68, 68, 0.1);
    border: 1px solid rgba(239, 68, 68, 0.3);
    border-radius: 8px;
    color: #fca5a5;
    font-size: 14px;
  }

  .info-box {
    background: rgba(145, 227, 253, 0.05);
    border: 1px solid rgba(145, 227, 253, 0.2);
    border-radius: 8px;
    padding: 20px;
  }
`;
