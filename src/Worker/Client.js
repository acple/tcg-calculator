// FFI: Vite statically detects the `new Worker(new URL(..., import.meta.url))` pattern
// to bundle and content-hash the worker entry point.
export const createWorker = () => new Worker(new URL("/worker.js", import.meta.url), { type: "module" });
