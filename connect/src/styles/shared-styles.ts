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

  button.danger {
    background: var(--ac-danger-color);
  }

  button.danger:hover:not(:disabled) {
    background: #c8165b;
  }

  button.danger:focus-visible:not(:disabled) {
    outline: 2px solid var(--ac-danger-color);
    outline-offset: 2px;
    box-shadow: 0 0 0 4px rgba(244, 54, 127, 0.2);
  }

  button:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  button.full {
    width: 100%;
  }

  .back-button,
  .close-button {
    position: absolute;
    top: 20px;
    cursor: pointer;
  }

  .back-button {
    left: 20px;
  }

  .close-button {
    right: 20px;
  }

  .back-button svg,
  .close-button svg {
    width: 28px;
    height: 28px;
    color: white;
    opacity: 0.5;
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

  .center {
    display: flex;
    justify-content: center;
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

  .box-header {
    display: flex;
    align-items: center;
    gap: 10px;
    margin-bottom: -8px;
  }

  .box-header h3 {
    font-weight: 600;
  }

  .box-header svg {
    width: 24px;
    height: 24px;
    stroke-width: 2;
    flex-shrink: 0;
    color: var(--ac-primary-color);
  }

  /* State indicators */
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
`;
