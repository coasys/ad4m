import preact from "@preact/preset-vite";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [preact()],
  build: {
    target: ["safari11"],
    rollupOptions: {
      external: (id) => id.startsWith("@tauri-apps/"),
    },
  },
  server: {
    port: 3000,
  },
});
