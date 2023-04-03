import { defineConfig } from "vite";
import preact from "@preact/preset-vite";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [preact()],
  build: {
    target: ["safari11"],
  },
  server: {
    port: 3000,
  },
});
