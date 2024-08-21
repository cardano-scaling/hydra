# Known issues and limitations

Before running a Hydra node on the Cardano mainnet, it is important to be aware of several known issues and limitations. Operating a Hydra node requires a deep understanding of the underlying infrastructure, and you may risk your funds if you are unfamiliar with the implementation and usage processes.

### Head protocol limits

Due to the limitations on transaction sizes and execution budgets on Cardano, the Hydra protocol has the following constraints:

- The protocol can only handle a maximum number of participants in a head (see [the cost of CollectCom transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-collectcom-transaction)). When attempting to configure too many peers, the Hydra node will inform you of the current configured maximum.

Currently, participants may be denied access to their funds by other protocol participants at different stages within a Hydra head because of the complexity or size of the UTXO being committed or created while the head is open:

- The Hydra head cannot be _finalized_ if it holds more than approximately 80 assets (see [the cost of FanOut transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-fanout-transaction) for latest numbers), although it can be _closed_
- Tokens that are minted and not burned within an open Hydra head will prevent the head from being _finalized_
- If one or more participants commit UTXOs that are too large to be processed together in a `CollectCom` or  `Abort` transaction, the Hydra head will remain stuck in the _initialising_ stage.

See these resources for additional information about reducing the risk of locking up funds in a Hydra head:

* [Directly open heads](https://github.com/cardano-scaling/hydra/issues/1329)
* [Always abortable head](https://github.com/cardano-scaling/hydra/issues/699)
* [Limit size/complexity of UTXOs in the head](https://github.com/cardano-scaling/hydra/issues/698)
* [Only sign closable snapshots](https://github.com/cardano-scaling/hydra/issues/370).

### Training wheels

There is a hard-coded limit on the **mainnet** where only up to 100 ada can be committed into the Hydra head. This is a safety precaution and will be increased as more experience is gained in running Hydra heads on the mainnet.

### Known bugs

Refer to the project repository issue tracker for [known issues](https://github.com/input-output-hk/hydra/issues?q=is%3Aissue+is%3Aopen+label%3A%22bug+%3Abug%3A%22). If you discover any security-relevant problems, please follow our [security policy](https://github.com/input-output-hk/hydra?tab=security-ov-file#readme).
