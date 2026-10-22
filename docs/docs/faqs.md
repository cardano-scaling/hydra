---
title: FAQs
---
# Frequently asked questions

<details>
<summary>What is Hydra?</summary>

Hydra is a family of layer 2 protocols designed to address network scalability
capabilities. Hydra Head is the first in this protocol suite, providing the
foundation on which to build out further scalability.

</details>

<details>
<summary>When Hydra?</summary>

The project is available on all Cardano networks (including mainnet), and
[release notes](https://github.com/cardano-scaling/hydra/releases) describe
the features and compatibility requirements of each version. Track ongoing
work in the [repository](https://github.com/cardano-scaling/hydra/issues).

</details>

<details>
<summary>What is the difference between layer 1 and layer 2?</summary>

Layer 1 solutions provide the foundational infrastructure of a blockchain
network, while layer 2 solutions introduce supplementary protocols or
mechanisms to improve scalability and functionality <a
href="https://www.essentialcardano.io/article/layer-1-and-layer-2-all-you-need-to-know">Read
more in this blog post.</a>

</details>

<details>
<summary>Is the Hydra Head protocol secure?</summary>

Hydra is based on peer-reviewed research and tested implementations, but its
security depends on operational and protocol assumptions. An honest party
must retain signed snapshots and observe L1 in time to contest an outdated
close. Progress requires all parties to sign. Deposits rely on Cardano
settlement assumptions, and partial fanout uses a
[KZG trusted setup](./dev/architecture/partial-fanout.md#trusted-setup).
Review the [known limitations](./known-issues.md) and
[operating guidance](./how-to/operating-hydra.md) before using a head.

</details>

<details>
<summary>So what’s this I have heard about ‘1M TPS’?</summary>

This has been previously referenced as a theoretical maximum, but the reality
is more nuanced. For a start, with Cardano’s ‘transactions within
transactions’ EUTXO model, TPS itself isn’t a useful metric. A Hydra Head is
like a small community within a larger group. Initially, these communities
operate independently. So, adding up their metrics to get a total picture
isn't accurate. Since Hydra heads use the EUTXO model, they can process
transactions simultaneously without conflicts, especially with good
networking, which optimizes resource usage. As the project progresses, we're
constantly evaluating its real-world performance in terms of throughput and
finality. For more details, read <a
href="https://iohk.io/en/blog/posts/2021/09/17/hydra-cardano-s-solution-for-ultimate-scalability">this</a> blog post, see the latest
benchmarking data <a href="../benchmarks">here</a>, and look at the
<a href="../benchmarks/scenarios">scenario benchmarks</a> for peak sustained and
end-to-end TPS across head sizes, UTxO shapes, and incremental commit/decommit
modes.

</details>

<details>
<summary>Can anyone use the Hydra Head protocol?</summary>

Yes, it's designed to be accessible for developers and end users alike,
requiring minimal changes to existing applications to integrate with the
protocol. However, it is important to note that Hydra is not a network
upgrade, and it's not like flipping a switch on Cardano to make it fast -
instead, applications need to adopt and build on Hydra heads to benefit from
it.

</details>

<details>
<summary>When is the Hydra Head protocol a good fit?</summary>

The Hydra Head protocol is well-suited for situations where a known set of
participants know each other well enough to agree on building a network but
don’t trust each other enough to manage funds without securing their assets.
This security is backed by the possibility of settling disputes on layer 1.

</details>

<details>
<summary>Can I run Plutus scripts inside a head?</summary>

Yes! Transactions running between head participants are Conway
transactions. They carry scripts and spend UTXOs in the same manner as layer 1
transactions. Each Hydra node runs a Cardano ledger and maintains a ledger
state.

</details>

<details>
<summary>Can a third party run a Hydra node on behalf of wallet owners (eg, running managed Hydra heads)?</summary>

A provider can host nodes, but custody and availability depend on who holds
the Hydra signing keys and who monitors L1. The current node signs snapshots
locally; a service with client-held snapshot keys needs additional signing
infrastructure. See [managed heads](/topologies/managed) for the proposal and
[delegated heads](/topologies/delegated-head) for a different operating model.
</details>

<details>
<summary>What is the relationship between Hydra heads and Hydra nodes?</summary>

Each `hydra-node` process manages one active head. A head has up to 29
parties; mirrors can run additional instances of an existing party. Multiple
heads run independently, with separate node processes and funding. A service
can host several such processes, but this does not provide built-in routing
or atomic transfers between heads.

</details>

<details>
<summary>Is the Hydra Head protocol a sidechain?</summary>

No, it isn't. There are two crucial reasons why Hydra heads are not considered
sidechains:

1. There's no guaranteed data availability in Hydra. Transactions are (a) only
   known to head participants and (b) typically forgotten once processed.
   There are no blocks in a Hydra head, and participants have no incentive to
   keep the history or make it available to users outside the head.

2. A head network is static; new participants cannot join and must be decided
   upfront. The network is isolated and private, accessible only to a set of
   well-known participants.

</details>

<details>

<summary>If the Hydra Head ledger is configured with a non-zero transaction fee, where do the paid fees go?</summary>

Setting protocol parameters with `fee > 0` ensures that Hydra Head (layer 2)
transactions consume more than they produce. On layer 1, the UTXO remains
unchanged, and the difference accrues. Currently, when settling an agreed
state from layer 2 on layer 1 during fanout, this difference becomes
spendable by the head participant who posts the `fanoutTx`.

</details>

<details>
<summary> Is it possible to use different protocol parameters in the Hydra head?</summary>

Yes, the ledger used for layer 2 transactions in a Hydra head is configurable,
allowing for the same or different protocol parameters as those used in
layer 1. **However, there is an important caveat to consider**:

If UTXOs are snapshotted on layer 2, they must be fanned out on layer 1
**exactly** as they were recorded in the snapshot.

### Examples

1. **Minimum ADA per output (`utxoCostPerByte = 0`)**:
   - Outputs with no 'ada' on layer 2 would be disallowed on layer 1,
   preventing their fanout. This restriction makes direct fanout impossible
   for such outputs. Even using [partial fanout](./dev/architecture/partial-fanout.md) would not
   permit the fanout of affected UTXOs.

2. **Maximum transaction execution units (`maxTxExecutionUnits(L2) > maxTxExecutionUnits(L1)`)**:
   - Outputs directed to scripts, which are too costly to validate on layer 1,
   can still be fanned out but will become unspendable due to exceeding the
   allowable execution limits on layer 1.

Use the [ledger parameter guidance](./configuration.md#ledger-parameters) to
keep outputs compatible with L1, including sufficient space for fanout overhead.

</details>

<details>
<summary>How do I get involved?</summary>

Join public monthly meetings to engage with the Hydra team and contribute to
its open governance. These meetings provide a platform for community
developers to stay updated on the latest developments, ask questions directly
to the team, and share their ideas. Start building on Hydra like SundaeLabs,
Modulo-P, Obsidian Systems, MLabs, and others!

</details>
