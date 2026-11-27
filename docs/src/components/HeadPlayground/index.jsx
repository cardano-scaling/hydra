import React, { useEffect, useRef, useState } from "react";
import BrowserOnly from "@docusaurus/BrowserOnly";
import Board from "./Board";
import Participants from "./Participants";
import Inspector from "./Inspector";
import Payment from "./Payment";
import { CHIP_VALUE, person, targets } from "./model.mjs";
import useProtocol from "./useProtocol";
import styles from "./styles.module.css";

export default function HeadPlayground() {
  const protocol = useProtocol();
  const { state, waiting, error, config, history, cursor, act, seek } =
    protocol;
  const [selected, setSelected] = useState(null);
  const [boardKey, setBoardKey] = useState(0);
  const [showHistory, setShowHistory] = useState(false);
  const [reducedMotion, setReducedMotion] = useState(false);
  const moveOptions = useRef(null);
  useEffect(() => {
    const media = window.matchMedia("(prefers-reduced-motion: reduce)");
    const update = () => setReducedMotion(media.matches);
    update();
    media.addEventListener("change", update);
    return () => media.removeEventListener("change", update);
  }, []);
  if (!state)
    return (
      <section className={styles.playground} aria-label="Hydra Head playground">
        <p role={error ? "alert" : "status"}>
          {error || "Loading the Hydra protocol…"}
        </p>
      </section>
    );
  const setup = state.phase === "setup";
  const disabled = waiting || Boolean(error);
  const action = state.action;
  const value = state.totals;
  const selectedFund = state.funds.find((f) => f.id === selected);
  const reset = (next) => {
    protocol.reset(next);
    setSelected(null);
    setBoardKey((key) => key + 1);
    setShowHistory(false);
  };
  const moveFunds = (fundId, target) => {
    act({
      type: "move",
      fund: fundId,
      owner: target.owner,
      layer: target.layer,
    });
    setSelected(null);
  };
  const selectFunds = (fundId, keyboard) => {
    setSelected(fundId);
    if (keyboard)
      window.requestAnimationFrame(() =>
        moveOptions.current?.querySelector("button")?.focus()
      );
  };
  const messages = showHistory ? state.messages : state.messages.slice(-1);
  return (
    <section
      className={styles.playground}
      aria-label="Hydra Head playground"
      aria-busy={waiting}
      onPointerCancelCapture={() => {
        setSelected(null);
        setBoardKey((key) => key + 1);
      }}
      onKeyDownCapture={(event) => {
        if (event.key === "Escape") {
          setSelected(null);
          setBoardKey((key) => key + 1);
        }
      }}
    >
      <Participants
        state={state}
        config={config}
        waiting={waiting}
        onChange={reset}
      />

      {error && <p role="alert">{error} Use Back or Start over to continue.</p>}
      <div className={styles.timeline} role="group" aria-label="Replay steps">
        <button
          type="button"
          disabled={waiting || cursor === 0}
          onClick={() => seek(0)}
          aria-label="Rewind to beginning"
        >
          First
        </button>
        <button
          type="button"
          disabled={waiting || cursor === 0}
          onClick={() => seek(cursor - 1)}
        >
          Back
        </button>
        <span aria-live="polite">
          Step {cursor} of {history.length}
        </span>
        <button
          type="button"
          disabled={waiting || cursor === history.length}
          onClick={() => seek(cursor + 1)}
        >
          Forward
        </button>
        <button
          type="button"
          disabled={waiting || cursor === history.length}
          onClick={() => seek(history.length)}
          aria-label="Fast-forward to latest step"
        >
          Latest
        </button>
      </div>

      <div
        className={styles.action}
        data-pending={Boolean(state.pending)}
        aria-live="polite"
        aria-atomic="true"
      >
        <div>
          <span className={styles.status}>
            {waiting
              ? "Processing…"
              : state.pending
              ? "Request in progress · funds paused"
              : setup
              ? "Ready to begin"
              : state.phase === "opening"
              ? "Opening on L1"
              : "Ready"}
          </span>
          <h3>{action.title}</h3>
          <details className={styles.actionDetails}>
            <summary>Details</summary>
            <p>{action.detail}</p>
          </details>
        </div>
        {action.button && (
          <button
            type="button"
            className={styles.primary}
            disabled={disabled}
            onClick={() => {
              act({ type: setup ? "open" : "step" });
              setSelected(null);
            }}
          >
            {action.button}
          </button>
        )}
      </div>

      <div className={styles.ledgerHeaders}>
        <div>
          <h3>Cardano · L1</h3>
          <strong>{value.wallets} ADA in wallets</strong>
          <span>
            {value.backing} ADA backing the Head
            {value.deposits > 0 ? ` · ${value.deposits} ADA in deposits` : ""}
          </span>
        </div>
        <div>
          <h3>Hydra · L2</h3>
          <strong>{value.spendable} ADA confirmed & available</strong>
          {value.pending > 0 && <span>{value.pending} ADA pending</span>}
        </div>
      </div>
      <BrowserOnly fallback={<p>Loading the funds board…</p>}>
        {() => (
          <Board
            key={boardKey}
            state={state}
            disabled={disabled}
            selected={selected}
            onSelect={selectFunds}
            onMove={moveFunds}
            reducedMotion={reducedMotion}
          />
        )}
      </BrowserOnly>
      {!waiting &&
        !error &&
        selectedFund &&
        targets(state, selected).length > 0 && (
          <div
            className={styles.moveOptions}
            ref={moveOptions}
            role="group"
            aria-label="Move selected funds"
          >
            <strong>
              {person(state, selectedFund.owner).name}’s {CHIP_VALUE} ADA
            </strong>
            {targets(state, selected).map((target) => (
              <button
                key={`${target.layer}-${target.owner}`}
                type="button"
                onClick={() => moveFunds(selected, target)}
              >
                {target.label}
              </button>
            ))}
            <button type="button" onClick={() => setSelected(null)}>
              Cancel
            </button>
          </div>
        )}
      <p className={styles.hint}>
        {state.pending
          ? "Finish the current step to move more funds."
          : setup
          ? "Open the Head to move funds."
          : "Drag or select funds to deposit, pay, or withdraw."}
      </p>

      {value.backing > 0 && (
        <Payment
          state={state}
          disabled={disabled}
          onPay={(funds, owner) => {
            act({ type: "pay", funds, owner });
            setSelected(null);
          }}
        />
      )}

      <section className={styles.messages} aria-label="Who sends what">
        <div className={styles.messageHeading}>
          <h3>Who sends what</h3>
          {state.messages.length > 1 && (
            <button
              type="button"
              aria-expanded={showHistory}
              onClick={() => setShowHistory(!showHistory)}
            >
              {showHistory
                ? "Hide history"
                : `History (${state.messages.length})`}
            </button>
          )}
        </div>
        {messages.length === 0 ? (
          <p>No messages yet.</p>
        ) : (
          <ol>
            {messages.map((item, index) => (
              <li key={`${state.messages.length - messages.length + index}`}>
                <div className={styles.route}>
                  <strong>{item.from}</strong>
                  <span aria-label="to">→</span>
                  <strong>{item.to}</strong>
                  {item.api && <small>{item.api}</small>}
                </div>
                {showHistory && <p>{item.text}</p>}
              </li>
            ))}
          </ol>
        )}
      </section>
      <Inspector state={state} />
    </section>
  );
}
