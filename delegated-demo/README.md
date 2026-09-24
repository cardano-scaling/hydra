# delegated-demo: operators as mediators

A 3-node Hydra head where the node operators are **mediators**: they run the
infrastructure and pay for the head's on-chain transactions, but they own none of
the funds inside the head. Two people who are not part of the network, **Anna** and
**Elsa**, bring their own funds in, exchange money on L2, and withdraw to L1.

This is the counterpart to the sibling [`demo`](../demo), where the operators
commit their own funds.

## Why this works

Since [ADR-033](../docs/adr/2026-03-10_033-directly-open-head.md) Hydra has no
initial-commit phase. `Init` opens the head with an empty UTxO set, and every
commit is a deposit signed by whoever owns the committed UTxO. The mediator node
only drafts and balances the deposit transaction; it never holds the owner's key.
So committing funds "for people who do not belong to the network" is the native
commit path. The only novelty here is that the operators commit nothing: fund
ownership is decoupled from node operation.

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
- `[s]` send N lovelace to the other user on L2 (`NewTx`)
- `[w]` withdraw all in-head funds back to L1 (`decommit`)
- `[l]` refresh, `[q]` quit

Under the hood these use `POST /commit`, `NewTx` over the WebSocket, and
`POST /decommit`, with `cardano-cli` building and the owner's own key signing each
transaction. See `lib.sh` for the exact calls.

## Automated happy path

With the cluster running, from the repository root:

```shell
delegated-demo/scenario.sh
```

It opens the head, has Anna commit 800 ada and Elsa 500 ada, sends 100 ada from
Anna to Elsa on L2, asserts the in-head balances, then has Elsa withdraw to L1 and
asserts her L1 balance grew by exactly the transferred amount. This is the
observable proof that the transfer settled on L1 (devnet). The head is left open,
so you can keep transacting with the actor menus.
