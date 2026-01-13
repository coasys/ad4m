import { css } from "lit";

/**
 * Shared styles for all ad4m-connect components
 * Includes typography, buttons, inputs, and common utilities
 */
export const sharedStyles = css`
  /* Typography */
  h1, h2, h3 {
    color: #ffffff;
    margin: 0;
  }

  h1 {
    font-size: 28px;
    line-height: 1;
    font-weight: 700;
    margin-bottom: 20px;
  }

  h2 {
    font-size: 22px;
    line-height: 1;
    font-weight: 700;
    margin-bottom: 20px;
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

  button.primary {
    background: var(--ac-primary-color);
    color: #000000;
  }

  button.primary:hover:not(:disabled) {
    filter: brightness(1.1);
  }

  button.secondary {
    background: rgba(145, 227, 253, 0.15);
    color: var(--ac-primary-color);
    border: 1px solid rgba(145, 227, 253, 0.3);
  }

  button.secondary:hover:not(:disabled) {
    background: rgba(145, 227, 253, 0.25);
  }

  button.ghost {
    background: transparent;
    color: rgba(255, 255, 255, 0.7);
  }

  button.ghost:hover:not(:disabled) {
    color: #ffffff;
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
    border: 1px solid rgba(145, 227, 253, 0.3);
    background: rgba(145, 227, 253, 0.05);
    color: #ffffff;
    font-size: 14px;
    outline: none;
    transition: all 0.2s ease;
  }

  input:focus {
    border-color: var(--ac-primary-color);
    background: rgba(145, 227, 253, 0.1);
  }

  input:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  input::placeholder {
    color: rgba(255, 255, 255, 0.4);
  }

  /* Common layout utilities */
  .container {
    display: flex;
    flex-direction: column;
    gap: 30px;
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
