# 14. Token usage in Hydra Scripts

Date: 2022-02-07

## Status

Accepted

## Context

* The Hydra on-chain-verification scripts are used to validate Hydra protocol transactions and ensure they are lawful.
* At least these two properties need to be enforced:
    - Authentication: ensure that only Head participants can, for example, `abort` a Head
    - Contract continuity: ensure that a Head was `init`ialized before it can be opened by a `collectCom` tx.
* The Hydra Head paper introduces **participation tokens (PT)** and a **state thread token (ST)** for that matter.
* Such tokens (a.k.a native assets) are identified by the `PolicyId`, that is the hash of their `MintingPolicyScript` (a.k.a `CurrencySymbol` in plutus), and a `ByteString`, the socalled `AssetId` (a.k.a as `TokenName` in plutus) (see [shelley-ma ledger spec](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.shelley-ma/latest/download-by-type/doc-pdf/shelley-ma#subsection.3.2))
* There can be multiple Hydra Heads on a network and a `hydra-node` need to distinguish individual Head instances or even (later) keep track of multiple Heads. Concretely, this means that we need to infer a Head identifier (`HeadId`) from observing each of the Hydra protocol transactions. 

## Decision

* We solve both challenges by defining that ST and PTs shall use the same `AssetId` and thus have the same `CurrencySymbol`
* The `MintingPolicyScript` shall be parameterized by `TxOutRef` to yield a unique `CurrencySymbol` per Head
(similar to the [`OneShotCurrency`](https://github.com/input-output-hk/plutus/tree/1efbb276ef1a10ca6961d0fd32e6141e9798bd11/plutus-use-cases/src/Plutus/Contracts/Currency.hs) example)
* ST and one PT per participant are minted in the `initTx`
* The `TokenName` of the ST can be any well-known `ByteString`, e.g. `"HydraHeadV1"`
* The `TokenName` of the PTs needs to be the `PubKeyHash` of the respective participant

## Consequences

* Heads can be identified by looking for the `ST` in `init`, `collectCom`, `close`, `contest` or `fanout` transactions, or the `PT` in `commit` transactions. In both cases, the `CurrencySymbol == HeadId`
* Our scripts become simpler as the ledger checks that ST/PT are paid forward (phase 1 validation), instead of OCV scripts needing to check datums
* The datum produced by `commit` txs (and consumed by `collectCom`) is `Just SerializedTxOut`, which is simpler than also keeping the participant which committed in the datum (compare to 0.3.0).
* The `v_head` script validator does not need to be parameterized, which makes discoverying new Heads (and also tracking them for metrics) easier as the address to watch for is common to all Heads (of the same `v_head` version).
* The `v_head` script (path) for the abort life-cycle can be implemented already much safer by checking that all PTs are burned on the `abort` transaction (counting inputs in 0.3.0).
* Updated diagrams for the [full](../images/on-chain-full.jpg) and [abort](../images/on-chain-abort.jpg) on-chain life-cycles of a Hydra Head.
