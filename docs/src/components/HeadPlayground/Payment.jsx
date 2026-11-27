import React, { useState } from "react";
import { CHIP_VALUE, person, targets } from "./model.mjs";
import styles from "./styles.module.css";

export default function Payment({ state, disabled, onPay }) {
  const [senderId, setSenderId] = useState("");
  const [recipientId, setRecipientId] = useState("");
  const [count, setCount] = useState(1);
  const available = state.funds.filter(
    (fund) => fund.place === "l2" && !fund.reservation
  );
  const senders = state.people.filter((p) =>
    available.some((f) => f.owner === p.id)
  );
  const sender = senders.find((p) => p.id === senderId) || senders[0];
  const recipients = state.people.filter((p) => p.id !== sender?.id);
  const recipient =
    recipients.find((p) => p.id === recipientId) || recipients[0];
  const funds = available.filter((f) => f.owner === sender?.id);
  const selected = funds.slice(0, Math.min(count, funds.length));
  const canSend =
    selected.length > 0 &&
    recipient &&
    selected.every((fund) =>
      targets(state, fund.id).some(
        (target) => target.layer === "l2" && target.owner === recipient.id
      )
    );
  const paused = disabled || Boolean(state.pending);
  const reason = paused
    ? "Payment paused until this step finishes."
    : !sender
    ? "Deposit funds to make a payment."
    : !recipient
    ? "Start over with another participant."
    : null;

  return (
    <form
      className={styles.payment}
      aria-label="Send a payment"
      onSubmit={(event) => {
        event.preventDefault();
        if (!paused && canSend)
          onPay(
            selected.map((fund) => fund.id),
            recipient.id
          );
      }}
    >
      <h3>Send a payment</h3>
      <fieldset disabled={paused || !sender || !recipient}>
        <label>
          From
          <select
            aria-label="Payment sender"
            value={sender?.id || ""}
            onChange={(event) => {
              setSenderId(event.target.value);
              setCount(1);
            }}
          >
            {!sender && <option value="">No available L2 funds</option>}
            {senders.map((p) => (
              <option key={p.id} value={p.id}>
                {p.name}
              </option>
            ))}
          </select>
        </label>
        <label>
          To
          <select
            aria-label="Payment recipient"
            value={recipient?.id || ""}
            onChange={(event) => setRecipientId(event.target.value)}
          >
            {!recipient && <option value="">No other participant</option>}
            {recipients.map((p) => (
              <option key={p.id} value={p.id}>
                {p.name}
              </option>
            ))}
          </select>
        </label>
        <label>
          Amount
          <select
            aria-label="Payment amount"
            value={selected.length}
            onChange={(event) => setCount(Number(event.target.value))}
          >
            {!funds.length && <option value={0}>0 ADA</option>}
            {funds.map((_, i) => (
              <option key={i} value={i + 1}>
                {(i + 1) * CHIP_VALUE} ADA
              </option>
            ))}
          </select>
        </label>
        <button type="submit" disabled={!canSend}>
          Send payment
        </button>
      </fieldset>
      <p className={styles.hint} aria-live="polite">
        {reason ||
          `${selected.length} output${selected.length === 1 ? "" : "s"} · ${
            sender.name
          }’s wallet → ${person(state, sender.node).name}’s node · NewTx`}
      </p>
    </form>
  );
}
