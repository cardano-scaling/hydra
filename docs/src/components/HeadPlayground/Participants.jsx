import React from "react";
import { operators, operatorStatus } from "./model.mjs";
import styles from "./styles.module.css";

export default function Participants({ state, config, waiting, onChange }) {
  const ops = operators(state);
  const users = state.people.filter((p) => p.role === "user");
  const setup = state.phase === "setup";
  const reset = (operators = ops.length, users = config.users) =>
    onChange({ operators, users });
  return (
    <div className={styles.setup}>
      <div className={styles.counts}>
        <label>
          Operators{" "}
          <select
            aria-label="Number of operators"
            disabled={!setup || waiting}
            value={ops.length}
            onChange={(event) =>
              reset(Number(event.target.value), users.length)
            }
          >
            {[1, 2, 3, 4].map((n) => (
              <option key={n} value={n}>
                {n}
              </option>
            ))}
          </select>
        </label>
        <label>
          Wallet users{" "}
          <select
            aria-label="Number of wallet users"
            disabled={!setup || waiting}
            value={users.length}
            onChange={(event) => reset(ops.length, Number(event.target.value))}
          >
            {[0, 1, 2, 3, 4].map((n) => (
              <option key={n} value={n}>
                {n}
              </option>
            ))}
          </select>
        </label>
        <button
          type="button"
          className={styles.reset}
          disabled={waiting}
          onClick={() => reset()}
        >
          Start over
        </button>
      </div>
      {!setup && <p className={styles.hint}>Start over to change the group.</p>}
      <div className={styles.people}>
        <div>
          <p>Operators run nodes and sign snapshots.</p>
          <div className={styles.operators} aria-label="Head operators">
            {ops.map((op) => {
              const status = operatorStatus(state, op.id);
              return (
                <div
                  key={op.id}
                  className={styles.operator}
                  data-active={state.phase === "open"}
                  data-operator={op.id}
                  data-leader={status.leader}
                  data-signed={status.signed}
                  aria-label={`${op.name} · ${status.label}`}
                  title={status.label}
                >
                  {op.name}
                  <span aria-hidden="true">{status.marks}</span>
                </div>
              );
            })}
          </div>
          {state.signing && (
            <p className={styles.signingStatus} aria-live="polite">
              {state.signing.status === "next"
                ? `Next snapshot ${state.signing.number}`
                : `Snapshot ${state.signing.number} · ${
                    state.signing.signers.length
                  }/${ops.length} signatures received${
                    state.signing.status === "confirmed" ? " · confirmed" : ""
                  }`}
            </p>
          )}
        </div>
        <div>
          <p>Wallet users connect through a node.</p>
          {users.length === 0 && <p>Operators can use their own wallets.</p>}
          {users.map((user) => (
            <label key={user.id} className={styles.connection}>
              <strong>{user.name}</strong>
              <span>uses</span>
              <select
                aria-label={`${user.name}’s connection`}
                value={user.node}
                disabled={!setup || waiting}
                onChange={(event) => {
                  const connections = users.map((p) =>
                    Number(
                      (p.id === user.id ? event.target.value : p.node).split(
                        "-"
                      )[1]
                    )
                  );
                  onChange({ ...config, connections });
                }}
              >
                {ops.map((op) => (
                  <option key={op.id} value={op.id}>
                    {op.name}’s node
                  </option>
                ))}
              </select>
            </label>
          ))}
        </div>
      </div>
    </div>
  );
}
