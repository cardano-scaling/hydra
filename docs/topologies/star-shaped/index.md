---
sidebar_label: 'Star-shaped Network'
sidebar_position: 4
---

# Star-Shaped Head network

:hammer_and_wrench: This document is a work in progress

This document details the behaviour of so-called _Star-shaped Hydra Network_.

## Summary

A _Star-shaped Hydra Network_, or more precisely a Star-shaped Heads network, comprises:

* A central _Server_ node that provides "Head-as-a-service" with low expected downtime, likely operated by a company or organization equipped to host this service.
* _Client_ nodes, which may be DApp instances or mobile/personal wallets. These nodes might not always be online and can in


![Star-shaped Heads Network](./star-shaped-general.jpg)

Client nodes aim to interact with each other efficiently and at low cost using an L2 solution, with all the Hydra safety guarantees, but without the operational burden of maintaining an always-online "full" Hydra node. This might involve using an embedded version of the node or a lightweight variant. Potentially, hundreds or even thousands of such client nodes exist, though they are not always active simultaneously.

Client nodes establish pairwise Heads (e.g., _channels_) with the server. This setup is simpler than with a normal multiparty head because the server has a well-known identity, and the client can always provide the needed parameters (keys, IP) to the server when setting up the Head using a specific service, the details of which are outside the scope of this document.

Transactions that a client node posts in 'its' Head should be _reflected_ by the server into the other Heads it maintains.

_Questions_:
* Is it expected the pairwise Heads to have varying 'durations'. For example, a client comes, opens a Head, conducts some activities, and closes it, while other Heads maintained by the same server stay _Open_?
* How does the server provided guarantees preserving the basic _Safety property_ of Hydra Heads for each of the pairwise heads?
  *  The diagram suggests using _Hash Time-Lock Contracts_ ([HTLC](https://docs.lightning.engineering/the-lightning-network/multihop-payments/hash-time-lock-contract-htlc)) to ensure the Client can always retrieve its UTXOs if the server does not properly route the transaction to its destination.
* What kinds of transactions should be supported? HTLCs are suitable for payment-style transactions but not necessarily for DApps, unless adapted.
  * It seems to be assumed implicitly that the server can "route" a transaction from one Head to the appropriate Head, implying it "understands" the addresses of UTXOs posted in Heads.

## On-Chain transactions

The diagram below represents the lifecycle of two pairwise Heads between **Alice**, **Bob**, and the **Server**.

![Star-shaped Network On-Chain](./star-shaped-txs.png)

_Remarks_:

* It is assumed that transactions occurring in one head are reflected in the other, resulting in a consistently final UTXO `c`
  * Both heads must start with the _same_ initial UTXO set, which is challenging to implement (highlighted in red in the diagram)
  * If the final UTXO set is consistent, then it can be fanned out by any party. However, this could result in one `ν_head` staying dangling and becoming unspendable as it would recreate an already existing UTXO (grayed out transaction in the diagram)
* The lifecycles of the heads are interconnected: when one is closed, the other must also be closed, as ensured by the server.

## Off-Chain transactions

The following diagram depicts the sequence of messages exchanged between various `Node`s to ensure transaction propagation.

![Star-shaped Network Off-Chain Protocol](./off-chain-protocol.png)

_Remarks_:

* The server is represented by two nodes, `M(A)` and `M(B)`
* The `newTx` issued by `Alice` through her node will be propagated by the server to `Bob`'s node as `newTx` as well
* This diagram does not account for any additional transactions the server might need to post to provide guarantees to either Alice or Bob (for example, an in-head HTLC transaction)
* To ensure consistency of snapshots, the server is presumed to always act as the leader, i.e, the one triggering the emission of a snapshot.