import React from "react";
import styles from "./styles.module.css";

export default function Inspector({ state }) {
  return (
    <details className={styles.inspector}>
      <summary>See what the nodes know</summary>
      <p>
        Each operator has its own state. These values and decisions come from
        the running Hydra protocol.
      </p>
      {state.nodes.map((node, i) => (
        <details key={node.name}>
          <summary>
            {node.name} · snapshot {node.snapshot} · Head version {node.version}
          </summary>
          <p>
            Local outputs: {node.local.join(", ") || "none"}. Confirmed and
            settled outputs: {node.confirmed.join(", ") || "none"}.
          </p>
          {node.candidate != null && (
            <p>
              Collecting snapshot {node.candidate}: {node.signers.length} of{" "}
              {state.nodes.length} signatures received.
            </p>
          )}
          {state.decisions
            .filter((entry) => entry.operator === i)
            .map((entry, j) => (
              <div key={j}>
                <h4>Decisions for {entry.input.tag}</h4>
                <ol>
                  {entry.steps.map((step, k) => (
                    <li key={k}>
                      {step.label}
                      {typeof step.answer === "boolean"
                        ? ` — ${step.answer ? "yes" : "no"}`
                        : ""}
                    </li>
                  ))}
                </ol>
                <details>
                  <summary>Input and protocol result</summary>
                  <pre>
                    {JSON.stringify(
                      { input: entry.input, outcome: entry.outcome },
                      null,
                      2
                    )}
                  </pre>
                </details>
              </div>
            ))}
          <details>
            <summary>Current protocol state</summary>
            <pre>{JSON.stringify(node.state, null, 2)}</pre>
          </details>
        </details>
      ))}
    </details>
  );
}
