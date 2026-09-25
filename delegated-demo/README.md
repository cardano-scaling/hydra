# delegated-demo: operators as mediators

A 3-node Hydra head where the node operators are **mediators**: they run the
infrastructure and pay for the head's on-chain transactions, but they own none of
the funds inside the head. Two people who are not part of the network, **Anna** and
**Elsa**, bring their own funds in, exchange money on L2, and withdraw to L1.

This is the counterpart to the sibling [`demo`](../demo), where the operators
commit their own funds.

## Topology

| role   | who         | API port | node-to-node | owns funds? |
| ------ | ----------- | -------- | ------------ | ----------- |
| mediator | alice     | 4001     | 5001         | no          |
| mediator | bob       | 4002     | 5002         | no          |
| mediator | carol     | 4003     | 5003         | no          |
| user     | Anna      | via 4001 | -            | yes         |
| user     | Elsa      | via 4002 | -            | yes         |

Anna talks to mediator alice, Elsa to mediator bob. Carol is a pure mediator that
signs snapshots but serves no user, showing the head is a shared substrate across
interchangeable mediators. Anna and Elsa get freshly generated Cardano keys at
seed time (`devnet/credentials/{anna,elsa}.{sk,vk}`), funded from the faucet.

## Running it

Interactive, in tmux (recommended):

```shell
nix develop .#delegated-demo
run-delegated-demo
```

This opens a `cardano-node` window (node + seeding), a `mediators` window (the
three hydra-nodes), and an `actors` window with Anna's menu, Elsa's menu, and a
live L1-vs-head balance observer.

One-command bring-up, via process-compose:

```shell
nix run .#delegated-demo
```

This also runs a `hydra-tui` connected to the pure mediator carol (`:4003`), giving
an operator's view of the head: status, parties and the shared head UTxO set, with
only carol's node fuel in the Funds tab (mediators hold no head funds).

## The actor menu

Each of Anna and Elsa drives `delegated-demo/actor.sh`, a bash/websocat menu over
the mediator node's HTTP/WebSocket API:

- `[i]` init the head (requested through the mediator, which pays for it)
- `[c]` commit: deposit the largest L1 UTxO into the head (external commit)
- `[d]` (Elsa only) deposit on L1 directly, client-side, with no operator
- `[s]` send N lovelace to the other user on L2 (`NewTx`)
- `[w]` withdraw all in-head funds back to L1 (`decommit`)
- `[l]` refresh, `[q]` quit

Under the hood these use `POST /commit`, `NewTx` over the WebSocket, and
`POST /decommit`, with `cardano-cli` building and the owner's own key signing each
transaction. See `lib.sh` for the exact calls.

## Elsa's client-side deposit (no operator)

`[c]` commit relies on the mediator's `POST /commit` to draft the deposit and
co-fund it from the operator's wallet. Elsa can instead build her deposit entirely
herself and submit it straight to the cardano-node, so the operators never draft or
co-sign it, they only observe it on-chain and increment the head.

`delegated-demo/elsa-deposit.sh` does this. The Hydra-specific part, the deposit
inline datum (`(headId, deadline, [Commit])`, where each `Commit` embeds the CBOR of
a Plutus V3 `TxOut`), is built client-side by `delegated-demo/js` using a Cardano
JavaScript library (`@harmoniclabs/cbor`, which gives the byte-level control needed
to match Hydra's on-chain encoding). `cardano-cli` then assembles, signs and submits
the transaction. Elsa reads the public head id from the head, derives the deposit
script address from the embedded `hydra-plutus/plutus.json`, deposits one L1 UTxO
whole and pays the L1 fee from a second, so she needs at least two UTxOs. Because she
pays her own L1 fee now, her net L1 change nets to the transfer amount less that fee.

The `js` dependencies install on first run (`npm install`); run once with network
access, e.g. `npm --prefix delegated-demo/js install`.

## Automated happy path

With the cluster running, from the repository root:

```shell
delegated-demo/scenario.sh
```

It opens the head, has Anna commit 800 ada via the mediator and Elsa deposit 500 ada
client-side, sends 100 ada from Anna to Elsa on L2, asserts the in-head balances,
then has Elsa withdraw to L1 and asserts her L1 balance grew by the transferred
amount (within the L1 fee she paid for her own deposit). This is the observable proof
that the transfer settled on L1 (devnet). The head is left open, so you can keep
transacting with the actor menus.
