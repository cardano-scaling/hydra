# Frequently Asked Questions

<details>
<summary>What is Hydra?</summary>
​​Hydra is a family of layer 2 protocols designed to address network scalability capabilities. Hydra Head is the first in this protocol suite, providing the foundation on which to build out further scalability.
</details>

<details>
<summary>When Hydra?</summary>
The project is available on all Cardano networks (including mainnet) and releases with new features become available every four to six weeks. The roadmap is publicly available on <a href="https://github.com/orgs/input-output-hk/projects/21/views/25">Github</a> 
</details>

<details>
<summary>What is the difference between layer 1 and layer 2?</summary>
Layer 1 solutions provide the foundational infrastructure of a blockchain network, while layer 2 solutions introduce supplementary protocols or mechanisms to improve scalability and functionality <a href="https://www.essentialcardano.io/article/layer-1-and-layer-2-all-you-need-to-know">Read more in this blog post.</a> 
</details>

<details>
<summary>Is Hydra Head secure?</summary>
Absolutely. Hydra protocols were born out of IOG research, got peer-reviewed, and are implemented using test-driven development. The Hydra Head protocol is a true layer 2 and can fall back directly onto the Cardano layer 1 – hence inheriting the security model of the Cardano blockchain.
</details>

<details>
<summary>So what’s this I have heard about ‘1M TPS’?</summary>
This has been previously referenced as a theoretical maximum but the reality is more nuanced. For a start with Cardano’s ‘transactions within transactions’ EUTXO model TPS itself isn’t a useful metric. A Hydra Head is like a small community within a larger group. Initially these communities operate independently. So adding up their metrics to get a total picture isn't accurate. Since Hydra Heads use the EUTXO model they can process transactions simultaneously without conflicts especially with good networking which optimizes resource usage. As the project progresses we're constantly evaluating its real-world performance in terms of throughput and finality. For more info, read <a href="https://example.com/more-info">this</a> and the latest benchmarking data is shared <a href="https://example.com/latest-data">here</a>.
</details>

<details>
<summary>Can anyone use Hydra head?</summary>
Yes, it's designed to be accessible for developers and end users alike, requiring minimal changes to existing applications to integrate with Hydra Head. However, it is important to note that Hydra is not a network upgrade and it's not like flipping a switch on Cardano to make it fast - instead, applications need to adopt and build on Hydra heads to benefit from it.
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
<summary> Is it possible to use different protocol parameters in the Hydra Head?</summary>

Yes, the ledger used for Layer 2 (L2) transactions in a Hydra head is configurable, allowing for the same or different protocol parameters as those used in Layer 1 (L1). **However, there is an important caveat to consider**:

If UTXOs are snapshotted on L2, they must be fanned out on L1 **exactly** as they were recorded in the snapshot.

### Examples

1. **Minimum UTXO Value (`minUTxOValue = 0`)**:
   - Outputs with no 'ada' on L2 would be disallowed on L1, preventing their fanout. This restriction makes direct fanout impossible for such outputs. Even using partial fanout, as considered in [this feature](https://github.com/input-output-hk/hydra/issues/190), would not permit the fanout of affected UTXOs.

2. **Maximum Transaction Execution Units (`maxTxExecutionUnits(L2) > maxTxExecutionUnits(L1)`)**:
   - Outputs that are directed to scripts, which are too costly to validate on L1, can still be fanned out but will become unspendable due to exceeding the allowable execution limits on L1.

**Remember**, with great power comes great responsibility. It is crucial to carefully manage and align the L1 and L2 settings to ensure seamless operability and avoid unintended consequences in transaction processing.

</details>

<details>
<summary>How do I get involved?</summary>
Join public monthly meetings to engage with the Hydra team and contribute to its open governance. These meetings provide a platform for community developers to stay updated on the latest developments, ask questions directly to the team, and share their ideas. Start building on Hydra like SundaeLabs, Modulo-P, Obsidian Systems, MLabs, and others!
</details>