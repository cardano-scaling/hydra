# Frequently Asked Questions

> And their answers!

<details>
<summary>When Hydra?</summary>

Our <a href="https://github.com/orgs/input-output-hk/projects/21/">roadmap</a> is publicly available on Github. Note that there are multiple sections behind tabs to view it from different angles (as release packages, as quarters, etc...)  

</details>

<details>
<summary>When is the Hydra Head protocol a good fit?</summary>

The Hydra Head protocol is well-suited for any situation where a known set of participants know each other well-enough to agree on building a network but don't trust one another enough with funds management to do so without ways to secure their assets backed by the possibility to settle disputes on the layer 1.

</details>

<details>
<summary>Can I run Plutus scripts inside a head?</summary>

Yes! Transactions running between head participants are full-blown Alonzo transactions. They carry scripts, and spend UTxO in all-the-same manner as layer 1 transactions. Incidentally, each Hydra node is running a Cardano ledger and maintaining a ledger state. However, DApps which currently rely on the PAB for on-chain interactions will fall short when it comes to driving the execution of a Plutus contract inside a head. Indeed, the PAB is currently tightly coupled to the Cardano layer 1 chain; it is a Cardano client that interacts with the chain using the node-to-client mini-protocols (chain-sync, state-query, tx-submission). Hydra nodes do not expose such protocols (yet), making it incompatible with the PAB.

</details>

<details>
<summary>Can a third-party run a Hydra node on behalf of a wallet owners (e.g. running managed Hydra Heads)?</summary>

Totally! This is similar for instance to [Phoenix](https://phoenix.acinq.co/) in Bitcoin Lightning: a non-custodial managed lightning node. As an end-user, one still have full control on the keys and funds, but the underlying infrastructure is managed on one's behalf (provided fees). This however implies some form of trust between the service provider and the user. Indeed, the user implicitly trusts the service provider to, for instance, properly handle contestations and closure of a head.   
</details>

<details>
<summary>What is the relationship between Hydra heads and Hydra nodes?</summary>

It is (at least\*) a **one-to-many** relationship. Each Hydra head is comprised of several Hydra nodes. We are currently aiming for up to 100 nodes per head as a stretch goal. Heads are independent and form an isolated network. It is possible to have infinitely many heads running in parallel. 

_(\*) It is possible to make Hydra nodes support multiple heads making it a many-to-many relationship._

</details>

<details>
<summary>Is the Hydra Head protocol a side-chain?</summary>

No it isn't. In fact, there are two crucial facts that discards heads from being seen as side-chains:

  1. There's no guaranteed data availability on Hydra. Said differently, transactions are (a) only known of the head participants, and (b) typically forgotten as soon as they're processed. Indeed, there's no block in a Hydra head and also no incentive for participants to either keep the history around or make it available to users outside of the head.

  2. A head network is static, new participants cannot join and have to be decided upfront. The network is thus very much isolated / private, and not reachable by any peer. Hydra heads are really channels between a set of well-known participants.

</details>

<details>
<summary>If the Hydra Head ledger is configured with a non-zero transaction fee, where do the paid fees go?</summary>

Setting protocol parameters with `fee > 0` will enforce that transactions in the Hydra Head (layer 2) are consuming more than they produce. On the layer 1, however, the UTxO stay untouched and the total value does not change and a difference is accrued. Right now, when settling an agreed state from the L2 on the L1 during fanout, this difference will be spendable by the Head participant which does post the `fanoutTx`.

</details>

<details>
<summary> Is it possible to us a different protocol parameters in the Hydra Head?</summary>

In principle, yes! The ledger used for L2 transactions is configurable and can use the same or different protocol parameters as the L1. **But there is a catch!** If UTxOs get snapshotted on the L2, they can only be fanned out on the L1 **exactly** like they were snapshotted.

Let's look at two examples:

  1. `minUTxOValue = 0`: Outputs with no "ADA" in their value in the L2 would be disallowed on L1 -> this makes fanout not possible. Using partial fanout as also considered within [this feature](https://github.com/input-output-hk/hydra-poc/issues/190) would only disallow fanout of affected UTxOs.
  2. `maxTxExecutionUnits(L2) > maxTxExecutionUnits(L1)`: Outputs payed to scripts which are too expensive to validate on L1 -> will be fanned out, but become unspendable.
  
With great power, comes great responsibility. 

</details>
