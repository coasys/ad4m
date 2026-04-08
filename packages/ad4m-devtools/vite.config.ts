import { defineConfig } from 'vite';
import preact from '@preact/preset-vite';
import { resolve } from 'path';
import { copyFileSync, mkdirSync } from 'fs';

export default defineConfig({
  plugins: [
    preact(),
    {
      name: 'copy-static',
      closeBundle() {
        // Copy static files to dist
        const staticFiles = [
          'src/extension/manifest.json',
          'src/extension/devtools.html',
          'src/extension/panel.html',
          'src/styles/panel.css',
        ];
        for (const f of staticFiles) {
          const name = f.split('/').pop()!;
          try {
            copyFileSync(f, `dist/${name}`);
          } catch {}
        }
      }
    }
  ],
  build: {
    outDir: 'dist',
    emptyOutDir: true,
    rollupOptions: {
      input: {
        panel: resolve(__dirname, 'src/extension/panel.tsx'),
        devtools: resolve(__dirname, 'src/extension/devtools.ts'),
        background: resolve(__dirname, 'src/extension/background.ts'),
        'content-script': resolve(__dirname, 'src/extension/content-script.ts'),
      },
      output: {
        entryFileNames: '[name].js',
        chunkFileNames: '[name].js',
        assetFileNames: '[name].[ext]',
      },
    },
  },
});
