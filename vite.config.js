import { defineConfig } from "vite";

// TODO: When @tailwindcss/vite supports Vite 8:
// - Replace @tailwindcss/postcss with @tailwindcss/vite in package.json
// - Add `import tailwindcss from "@tailwindcss/vite"` and `plugins: [tailwindcss()]`
// - Remove .postcssrc.json
export default defineConfig({
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
  base: "./",
});
