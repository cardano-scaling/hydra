---
sidebar_label: 'Star-shaped network'
sidebar_position: 4
---

# Star-shaped head network

:::note

:hammer_and_wrench: This document is a work in progress.

:::

This document details the behavior of the so-called _star-shaped Hydra network_.

## Summary

A _star-shaped Hydra network_, or more precisely a star-shaped heads network, comprises:

* A central _server_ node that provides 'head-as-a-service' with low expected downtime, likely operated by a company or organization equipped to host this service.
* _Client_ nodes, which may be DApp instances or mobile/personal wallets. These nodes might not always be online and can come and go.

![Star-shaped Heads Network](./star-shaped-general.jpg)

Client nodes aim to interact with each other efficiently and at low cost using a layer 2 solution, with all the Hydra safety guarantees, but without the operational burden of maintaining an always-online 'full' Hydra node. This might involve using an embedded version of the node or a lightweight variant. Potentially, hundreds or even thousands of such client nodes exist, though they are not always active simultaneously.

Client nodes establish pairwise heads (eg, _channels_) with the server. This setup is simpler than with a normal multiparty head because the server has a well-known identity, and the client can always provide the needed parameters (keys, IP) to the server when setting up the head using a specific service, the details of which are outside the scope of this document.

Transactions that a client node posts in 'its' head should be _reflected_ by the server into the other heads it maintains.

_Questions_:
* Is it expected for the pairwise heads to have varying 'durations'? For example, a client comes, opens a head, conducts some activities, and closes it, while other heads maintained by the same server stay _open_.
* How does the server provide guarantees preserving Hydra heads' basic _safety property_ for each pairwise head?
  *  The diagram suggests using _Hash Time-Lock Contracts_ ([HTLC](https://docs.lightning.engineering/the-lightning-network/multihop-payments/hash-time-lock-contract-htlc)) to ensure the client can always retrieve its UTXOs if the server does not properly route the transaction to its destination.
* What kinds of transactions should be supported? HTLCs are suitable for payment-style transactions but not necessarily for DApps, unless adapted.
  * It seems to be assumed implicitly that the server can 'route' a transaction from one head to the appropriate head, implying it 'understands' the addresses of UTXOs posted in heads.

## On-chain transactions

The diagram below represents the lifecycle of two pairwise heads between **Alice**, **Bob**, and the **server**.

![Star-shaped Network On-Chain](./star-shaped-txs.png)

_Remarks_:

* It is assumed that transactions occurring in one head are reflected in the other, resulting in a consistently final UTXO `c`
  * Both heads must start with the _same_ initial UTXO set, which is challenging to implement (highlighted in red in the diagram)
  * If the final UTXO set is consistent, then it can be fanned out by any party. However, this could result in one `ν_head` staying dangling and becoming unspendable as it would recreate an already existing UTXO (grayed out transaction in the diagram).
* The lifecycles of the heads are interconnected: when one is closed, the other must also be closed, as ensured by the server.

## Off-chain transactions

The following diagram depicts the sequence of messages exchanged between various `node`s to ensure transaction propagation.

![Star-shaped Network Off-Chain Protocol](./off-chain-protocol.png)

_Remarks_:

* The server is represented by two nodes, `M(A)` and `M(B)`
* The `newTx` issued by `Alice` through her node will be propagated by the server to `Bob`'s node as `newTx` as well
* This diagram does not account for any additional transactions the server might need to post to provide guarantees to either Alice or Bob (for example, an in-head HTLC transaction)
* To ensure consistency of snapshots, the server is presumed to always act as the leader, ie, the one triggering the emission of a snapshot.
