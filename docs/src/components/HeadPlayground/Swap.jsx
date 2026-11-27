import React, { useState } from "react";
import { fundsValue, person, targets } from "./model.mjs";
import styles from "./styles.module.css";

export default function Swap({ state, disabled, onSwap }) {
  const [buyerId, setBuyerId] = useState("");
  const [tokenId, setTokenId] = useState("");
  const tokens = state.funds.filter((fund) => fund.tokens > 0);
  const token =
    tokens.find((fund) => fund.id === tokenId) ||
    tokens.find((fund) => fund.place === "l2") ||
    tokens[0];
  const seller = person(state, token.owner);
  const buyers = state.people.filter((p) => p.id !== seller.id);
  const buyer = buyers.find((p) => p.id === buyerId) || buyers[0];
  const payment = state.funds.filter(
    (fund) =>
      fund.owner === buyer?.id &&
      !fund.tokens &&
      targets(state, fund.id).some(
        (target) => target.layer === "l2" && target.owner === seller.id
      )
  );
  const paused = disabled || Boolean(state.pending);
  const canSwap =
    payment.length >= 2 &&
    targets(state, token.id).some(
      (target) => target.layer === "l2" && target.owner === buyer?.id
    );
  return (
    <form
      className={styles.payment}
      aria-label="Swap token for ADA"
      onSubmit={(event) => {
        event.preventDefault();
        if (!paused && canSwap) {
          setTokenId(token.id);
          onSwap(token.id, buyer.id);
        }
      }}
    >
      <h3>Swap token for ADA</h3>
      <fieldset disabled={paused || !buyer}>
        <label>
          Gives
          <select
            aria-label="Token output"
            value={token.id}
            onChange={(event) => setTokenId(event.target.value)}
          >
            {tokens.map((fund) => (
              <option key={fund.id} value={fund.id}>
                {person(state, fund.owner).name}: {fundsValue([fund])}
              </option>
            ))}
          </select>
        </label>
        <label>
          Receives{" "}
          {payment.length >= 2
            ? fundsValue(payment.slice(0, 2))
            : "two ADA outputs"}{" "}
          from
          <select
            aria-label="Token buyer"
            value={buyer?.id || ""}
            onChange={(event) => setBuyerId(event.target.value)}
          >
            {!buyer && <option value="">No other participant</option>}
            {buyers.map((p) => (
              <option key={p.id} value={p.id}>
                {p.name}
              </option>
            ))}
          </select>
        </label>
        <button type="submit" disabled={!canSwap}>
          Agree & swap
        </button>
      </fieldset>
      <p className={styles.hint} aria-live="polite">
        {paused
          ? "Swap paused until this step finishes."
          : canSwap
          ? "Both wallets agree. One NewTx exchanges both sides on L2."
          : "Deposit the token and another person’s two ADA outputs first."}
      </p>
    </form>
  );
}
