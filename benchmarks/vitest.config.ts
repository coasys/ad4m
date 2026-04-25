import { defineConfig } from 'vitest/config';

export default defineConfig({
  esbuild: {
    tsconfigRaw: {
      compilerOptions: {
        experimentalDecorators: true,
        emitDecoratorMetadata: true,
      },
    },
  },
  test: {
    testTimeout: 600_000,
    hookTimeout: 300_000,
    sequence: { concurrent: false },
    reporters: ['verbose'],
    include: ['src/orm-benchmarks/**/*.bench.ts'],
    isolate: false,
  },
});
