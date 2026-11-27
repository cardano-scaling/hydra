import { useEffect, useRef, useState } from "react";
import useBaseUrl from "@docusaurus/useBaseUrl";

export default function useProtocol() {
  const workerUrl = useBaseUrl("/head-playground-runtime/worker.js");
  const [config, setConfig] = useState({ operators: 2, users: 2 });
  const [history, setHistory] = useState([]);
  const [cursor, setCursor] = useState(0);
  const [state, setState] = useState(null);
  const [ready, setReady] = useState(0);
  const [waiting, setWaiting] = useState(true);
  const [error, setError] = useState(null);
  const worker = useRef(null);
  const sequence = useRef(0);

  useEffect(() => {
    const instance = new Worker(workerUrl);
    worker.current = instance;
    const fail = (message) => {
      setWaiting(false);
      setError(message);
    };
    instance.onmessage = ({ data }) => {
      if (data.type === "ready") {
        instance.loaded = true;
        setReady((generation) => generation + 1);
      } else if (data.type === "error") {
        fail(
          "The protocol runtime could not load. Reload the page to try again."
        );
      } else if (data.id === sequence.current) {
        setWaiting(false);
        if (data.error || data.result?.status === "error") {
          fail(data.error || data.result.reason);
        } else {
          setState(data.result);
          setError(null);
        }
      }
    };
    instance.onerror = (event) =>
      fail(event.message || "The protocol runtime stopped.");
    const timeout = setTimeout(() => {
      if (!instance.loaded)
        fail(
          "The protocol runtime is taking too long to load. Reload the page to try again."
        );
    }, 45000);
    return () => {
      clearTimeout(timeout);
      instance.terminate();
      worker.current = null;
    };
  }, [workerUrl]);

  useEffect(() => {
    if (!ready || !worker.current?.loaded) return;
    setWaiting(true);
    worker.current.postMessage({
      id: ++sequence.current,
      request: { setup: config, actions: history.slice(0, cursor) },
    });
  }, [ready, config, history, cursor]);

  const act = (action) => {
    if (waiting) return;
    setWaiting(true);
    setHistory((current) => [...current.slice(0, cursor), action]);
    setCursor(cursor + 1);
  };
  const seek = (next) => {
    if (waiting || next === cursor) return;
    setWaiting(true);
    setCursor(next);
  };
  const reset = (next = config) => {
    setWaiting(true);
    setHistory([]);
    setCursor(0);
    setConfig({ ...next });
  };
  return { state, waiting, error, config, history, cursor, act, seek, reset };
}
