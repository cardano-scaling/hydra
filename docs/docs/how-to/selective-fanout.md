---
sidebar_position: 6.5
---

# Selective fanout

When a head is closed and ready to fan out, the plain `Fanout` command distributes the
**whole** head at once. With a _selective_ (partial) fanout you instead choose exactly
which `UTXO` to distribute, and in which order — paying the on-chain cost only for what
you care about, and leaving the rest in the head to fan out later.

This how-to assumes we are in a similar situation as in the [Getting
started](../getting-started) or [Testnet tutorial](../tutorial): a head has been
`Close`d and the node has emitted a `ReadyToFanout` message.

Open a WebSocket session if you haven't already:

```shell
websocat "ws://127.0.0.1:4001?history=no"
```

First, find out what is still in the head. The confirmed snapshot UTxO is available over
HTTP:

```shell
curl localhost:4001/snapshot/utxo | jq
```

Pick the subset you want to fan out now and send a `PartialFanout` client input with it
under `utxoToFanout`. For example, to fan out a single `UTXO`:

```json title="Websocket API"
{ "tag": "PartialFanout", "utxoToFanout": { "<txin>#<ix>": { /* the matching output */ } } }
```

The node submits the corresponding layer 1 transaction (automatically splitting it across
several transactions if the selection is too large to fit in one) and replies with a
`HeadPartiallyFannedOut` message:

```json title="Example HeadPartiallyFannedOut"
{
  "tag": "HeadPartiallyFannedOut",
  "headId": "...",
  "distributedUTxO": { /* what this step put back on layer 1 */ },
  "remainingUTxO": { /* what is still in the head */ }
}
```

Use `remainingUTxO` to choose your next selection. Keep issuing `PartialFanout` commands —
selecting the entire remaining set when you want to finish — until the head is drained. On
the final step the node automatically submits the transaction that burns the head tokens
and emits `HeadIsFinalized` with the complete distributed `utxo`.

:::info Selective fanout is sticky

Once you send the first `PartialFanout` for a head, the plain `Fanout` command is no longer
accepted for it (you will receive a `CommandFailed`). Continue with `PartialFanout`
commands until the head is empty.

:::

:::tip Multiple parties

Any party can drive the next step. After one party's `PartialFanout` lands, the others
simply observe it and wait — they will not automatically drain the remainder. So you can
start a selective fanout on one node and continue selecting from another (useful if the
initiating node goes offline), driving each step sequentially.

:::

To confirm, query the funds of the wallet on layer 1 from a `cardano-node`:

```shell
cardano-cli query utxo \
  --address ${WALLET_ADDR} \
  --output-json | jq
```
