Welcome to the Hydra Proof-of-Concept (POC) documentation.

This technical documentation does contain some additional information about the
architecture and inner workings of a `hydra-node` and the [Hydra Head
protocol](https://eprint.iacr.org/2020/299.pdf).

:warning: This project is still prototypical and exploratory work - it is NOT ready for production (yet). :warning:

Thus, the documentation here is also a work in progress and is certainly not
complete. However, we do want to improve it and would like to hear from any
[questions](https://github.com/input-output-hk/hydra-poc/#question-contributing)
you might have (so we can at the very least compile an FAQ).

# Hydra Head protocol

The greater [vision of
Hydra](https://iohk.io/en/blog/posts/2020/03/26/enter-the-hydra-scaling-distributed-ledgers-the-evidence-based-way/)
involves a whole suite of layer-two protocols to achieve greater scalability in
many different use cases.

The [Hydra Head](https://eprint.iacr.org/2020/299.pdf) protocol is one of them
and forms the foundation for more advanced deployment scenarios and introduces
isomorphic, multi-party state channels. This is also the protocol on which we
focused most so far and implemented a proof of concept for.

There exist various flavors and extensions of the Hydra Head protocol, but let's
have a look at a full life cycle of a basic Hydra Head and how it allows for
isomorphic state transfer between layer 1 and layer 2.

![](images/hydra-head-lifecycle.svg)

A Hydra Head is formed by a group of online and responsive participants. They
**init** a Head by announcing several Head-specific parameters including the
participants list. Then each of the participants **commits** unspent transaction
outputs (UTXO) from the Cardano main-chain to it, before all the UTXO are
**collected** and made available in a Hydra Head as initial state (**U0**).

While open, they can use the Hydra Head via a `hydra-node` just the same as they
would be using the Cardano blockchain via a `cardano-node` by submitting
transactions to it (that's the **isomorphism** property). When UTXO are spent
and new UTXO are created in a Hydra Head, all participantes are required to
acknowledge and agree on the new state in so-called snapshots (**U1..n**)

Any participant can **close** the Head using an agreed state, when for example
they wish to use some UTXO on the mainnet or another party misbehaves or stalls
the Head evolution. There is a mechanism to **contest** the final state on the
main chain for a Head-specific contestation period, which a **fanout**
transaction does distribute in the end.

This is not the full picture though, as the protocol also allows to **abort**
Head initialization and protocol extensions for incremental commits and
decommits, as well as optimistic head closures (without contestation period) are
possible.

# Hydra Node Architecture

We use _Architecture Decision Records (ADR)_ for a lightweight technical
documentation about our principles and significant design decisions. The
architecture itself then is just a result of all accepted ADRs, which have not
been deprecated or superseeded. An up-to-date index of still relevant ADRs is
kept [here](./adr/README.md).

Please refer to the [inline documentation](https://input-output-hk.github.io/hydra-poc/haddock/hydra-node/index.html)
of each module for more details.

# On-chain verification (OCV)

An important part of the Hydra Head protocol is how it is secured on-chain. For
that matter, Hydra Head protocol transactions like `collectComTx`, `closeTx`,
`fanoutTx` etc. use Plutus script validators to check certain properties. For
example, that the multi-signed UTXO state is honored when distributing funds on
Head finalization.

Currently we only implement a basic, naiive version of those OCV algorithms and
take several short-cuts to have smaller, releasable increments and manage
limitations due to Plutus execution budgets or script sizes along the way. The
current structure of transactions, involved scripts and their datums/redeemers
is documented on examples of the [full](./images/on-chain-full.jpg) and
[abort](./images/on-chain-abort.jpg) on-chain life-cycles of a Hydra Head.
