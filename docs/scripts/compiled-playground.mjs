import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);

export async function loadPlayground(
  bundle = process.env.HYDRA_PLAYGROUND_BUNDLE ||
    fileURLToPath(
      new URL(
        "../static/head-playground-runtime/compiled/all.js",
        import.meta.url
      )
    )
) {
  const timers = new Set();
  const scheduleTimeout = globalThis.setTimeout;
  const scheduleInterval = globalThis.setInterval;
  const exit = process.exit;
  const track =
    (schedule) =>
    (...args) => {
      const timer = schedule(...args);
      timers.add(timer);
      return timer;
    };
  const close = () => {
    for (const timer of timers) {
      clearTimeout(timer);
      clearInterval(timer);
    }
  };
  const ready = new Promise((resolve) => {
    globalThis.hydraCremReady = resolve;
  });
  globalThis.setTimeout = track(scheduleTimeout);
  globalThis.setInterval = track(scheduleInterval);
  // GHC exits after registering the callback; keep it available for requests.
  process.exit = (code) => {
    if (code)
      throw new Error(`The compiled runtime exited with status ${code}.`);
  };
  let timeout;
  try {
    delete require.cache[require.resolve(bundle)];
    require(bundle);
    await Promise.race([
      ready,
      new Promise((_, reject) => {
        timeout = scheduleTimeout(
          () =>
            reject(new Error("The compiled playground did not become ready.")),
          30000
        );
      }),
    ]);
    return {
      evaluate: (request) =>
        JSON.parse(globalThis.hydraCremEvaluate(JSON.stringify(request))),
      close,
    };
  } catch (error) {
    close();
    throw error;
  } finally {
    clearTimeout(timeout);
    process.exit = exit;
    globalThis.setTimeout = scheduleTimeout;
    globalThis.setInterval = scheduleInterval;
  }
}
