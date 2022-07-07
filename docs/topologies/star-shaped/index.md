---
sidebar_label: 'Star-shaped Network'
sidebar_position: 3
---

# Star-Shaped Head Network

:hammer_and_wrench: This document is a work in progress

This document details the behaviour of so-called _Star-shaped Hydra Network_.

## Summary

A _Star-shaped Hydra Network_ or more precisely a Star-shaped Heads Network is comprised of:

* A central _Server_ node providing "Head-as-a-service", with low expected downtime, probably operated by some company or organisation with enough resources to host this service,
* _Client_ nodes, either DApp instances, or mobile/personal wallets, which might not be always online and possibly can come and go.

![Star-shaped Heads Network](./star-shaped-general.jpg)

Client nodes want to be able to interact with each other efficiently, at a low cost, using L2 solution, with all the Hydra safety guarantees, but without bearing the operational burden of operating an always online "full" Hydra node (eg. using an embedded version of the node, or a lightweight version). There might be a lot of them, say in the 100s or even 1000s but they are not always all live and up at the same time.

Client nodes establish pairwise Heads (eg. _channels_) with the server: This setup is simpler than with a normal multiparty head because the server has as a well-known identity and the client can always provide the needed parameters (keys, IP) to the server when setting up the Head using some specific service whose definition is outside of the scope of this document.

Transactions a client node posts in "its" Head should be _reflected_ by the server into the other Heads it maintain.

_Questions_:
* Is it expected the pairwise Heads to have varying "durations", eg. a client comes, opens a Head, does some stuff, and closes it but the other Heads maintained by the same server stay _Open_?
* How does the server provided guarantees preserving the basic _Safety property_ of Hydra Heads for each of the pairwise heads?
  * What the diagram suggest is to use _Hash Time-Lock Contracts_ ([HTLC](https://docs.lightning.engineering/the-lightning-network/multihop-payments/hash-time-lock-contract-htlc)) which ensures the Client can always get its UTxO back if the server does not properly route the transaction to its destination
* What kind of transaction should be supported? HTLC are good for payments-style transactions but not for DApps for example, or they would need to be adapted
  * There seems to be an implicit assumption that the server can "route" a transaction in one Head to the proper Head which implies it "understands" the addresses of UTxO posted in Heads

## On-Chain Transactions

The following transaction diagram represents the lifecycle of 2 pairwise Heads between **A**lice, **B**ob and **S**erver.

![Star-shaped Network On-Chain](./star-shaped-txs.png)

_Remarks_:

* This assumes the transactions happening in one head are reflected in the other head, thus resulting in a (strongly) consistent final UTxO `c`
  * This means both heads must start with the _same_ $U_0$ set which I don't know how can be done (highlighted in red in the diagram)
  * If the final UTxO set is consistent, then it can be fanned-out by any party, which means one `ν_head` can stay dangling and become unspendable as it would recreating an already existing UTxO (grayed out transaction in the diagram)
* The lifecycle of the heads are tied: When one is closed, the other is closed. The server will ensure that it is the case.

## Off-Chain Transactions

The following picture represents the sequence of messages exchanged between the various `Node`s in order to ensure propagation of transactions.

![Star-shaped Network Off-Chain Protocol](./off-chain-protocol.png)

_Remarks_:

* The Server is represented by 2 nodes, `M(A)` and `M(B)`,
* The `newTx` issued by `Alice` through her node will be propagated by Server to `Bob`'s node as `newTx` too
* This diagram does not represent any possible additional transactions the Server would need to post in order to provide guarantees to either or both `Alice` and `Bob` (e.g an in-head HTLC transaction)
* In order to ensure consistency of snapshots, the Server is assumed to always be the leader, ie. the one triggering the emission of a snapshot.
