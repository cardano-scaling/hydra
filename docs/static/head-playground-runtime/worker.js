self.hydraCremReady = () => self.postMessage({ type: "ready" });
self.onmessage = ({ data: { id, request } }) => {
  try {
    const result = JSON.parse(self.hydraCremEvaluate(JSON.stringify(request)));
    self.postMessage({ id, result });
  } catch (error) {
    self.postMessage({ id, error: String(error) });
  }
};
try {
  importScripts("./compiled/all.js");
} catch (error) {
  self.postMessage({ type: "error", error: String(error) });
}
