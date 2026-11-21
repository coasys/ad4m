import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'happy-dom',
    // The tests' frames are fakes; an iframe must not fetch the auth domain.
    environmentOptions: { happyDOM: { settings: { disableIframePageLoading: true, handleDisabledFileLoadingAsSuccess: true } } },
    include: ['src/**/*.test.ts'],
    // Node 25+ has its own `localStorage` global, which hides happy-dom's (and is undefined without
    // `--localstorage-file`).
    poolOptions: { threads: { execArgv: ['--no-experimental-webstorage'] }, forks: { execArgv: ['--no-experimental-webstorage'] } },
  },
});
