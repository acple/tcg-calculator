import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  plugins: [tailwindcss()],
  build: {
    target: "esnext",
    modulePreload: false,
    sourcemap: true,
    rolldownOptions: {
      treeshake: {
        annotations: true,
        moduleSideEffects: false,
      },
    },
  },
  worker: {
    rolldownOptions: {
      treeshake: {
        annotations: true,
        moduleSideEffects: false,
      },
    },
  },
  base: "./",
});
