import React from 'react';
import { render, screen } from '@testing-library/react';
import App from './App';

test('renders Ad4min', () => {
  render(<App />);
  const linkElement = screen.getByText(/Ad4min/i);
  expect(linkElement).toBeInTheDocument();
});
