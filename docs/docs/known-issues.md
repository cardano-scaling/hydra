## **Known issues and limitations**

Before running a Hydra node on the Cardano mainnet, it is important to be aware of several known issues and limitations. Operating a Hydra node requires a deep understanding of the underlying infrastructure, and you may risk your funds if you are unfamiliar with the implementation and usage processes.

**Head protocol limits**

Due to the limitations on transaction sizes and execution budgets on Cardano, the Hydra protocol has the following constraints:

**Maximum number of participants. **The protocol can handle only a limited number of participants in a Hydra head. The exact limit is determined by the cost of the ``Collectcom`` transaction. When attempting to configure too many peers, the Hydra node will inform you of the current maximum allowed participants upon startup. For more information,[ see the cost of the collection transaction.](https://hydra.family/head-protocol/unstable/benchmarks/transaction-cost/#cost-of-collectcom-transaction)

* **Access to funds.** Participants may be denied access to their funds by other protocol participants at different stages within a Hydra head because of the complexity or size of the UTXO being committed or created while the head is open:
    * The Hydra head cannot be finalized if it holds more than approximately 80 assets (as per the cost of the ``Fanout`` transaction). Although the head can still be closed, finalization may not be possible under these conditions.
    * Tokens that are minted and not burned within an open Hydra head will prevent the head from being finalized.
    * If one or more participants commit UTXOs that are too large to be processed together in a ``CollectCom`` or  ``Abort`` transaction, the Hydra head will remain stuck in the initializing stage.

See these resources for additional information about reducing the risk of locking up funds in a Hydra head:

* [Directly open heads](https://github.com/input-output-hk/hydra/issues/1329)
* [Always abortable head](https://github.com/input-output-hk/hydra/issues/699)
* [Limit size/complexity of UTXOs in the head](https://github.com/input-output-hk/hydra/issues/698)
* [Only sign closable snapshots](https://github.com/input-output-hk/hydra/issues/370)

### Training wheels

There is a hard-coded limit on the **mainnet** where only up to 100 ada can be committed into the Hydra head. This is a safety precaution and will be increased as more experience is gained in running Hydra heads on the mainnet.

### Known bugs

Refer to the project repository issue tracker for [known issues](https://github.com/input-output-hk/hydra/issues?q=is%3Aissue+is%3Aopen+label%3A%22bug+%3Abug%3A%22). If you discover any security-relevant problems, please follow our [security policy](https://github.com/input-output-hk/hydra?tab=security-ov-file#readme).