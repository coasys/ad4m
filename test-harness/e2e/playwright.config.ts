import { defineConfig } from '@playwright/test';

export default defineConfig({
  testDir: '.',
  testMatch: '**/*.spec.ts',
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 1 : 0,
  workers: 1,
  timeout: 120_000,      // 2 min per test (executor startup is slow)
  globalTimeout: 10 * 60_000,  // 10 min total

  reporter: [
    ['html', { open: 'never' }],
    ['list'],
  ],

  use: {
    screenshot: 'only-on-failure',
    trace: 'on-first-retry',
    video: 'off',
  },

  projects: [
    {
      name: 'sfu-e2e',
      use: {
        browserName: 'chromium',
        launchOptions: {
          args: [
            '--use-fake-device-for-media-stream',
            '--use-fake-ui-for-media-stream',
            '--allow-file-access-from-files',
            '--disable-web-security',
            '--no-sandbox',
          ],
        },
      },
    },
  ],
});
