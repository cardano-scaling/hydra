# Known issues & limitations

Please be aware of the following known issues and limitations before running `hydra-node` on the Cardano `--mainnet`; as an incredibly technical project, Hydra in its current form requires an elevated level of understanding of the underlying infrastructure. Without the expertise required to operate a `hydra-node`, users may put their funds at risk if they are unfamiliar with the implementation and usage process.

### Head protocol limits

As transaction sizes and execution budgets are limited on Cardano:

- The protocol can only handle a maximum number of participants in a Head (see [cost of collectcom transaction](/benchmarks/transaction-cost/#cost-of-collectcom-transaction)). Upon startup, the `hydra-node` will inform you of the current configured maximum when trying to configure too many peers.

It's currently possible to be denied access to funds by other protocol participants at various stages in a Hydra Head because of the complexity or size of the UTxO being committed or created while the Head is open:

- The head cannot be _finalized_ if holding more than ~80 assets (see [cost of fanout transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-fanout-transaction) for latest numbers), although it can be _closed_
- Tokens minted and not burnt in an _open_ head will prevent it from being _finalized_
- If one or more participants commit UTxO too large to be processed together in a `CollectCom` or `Abort` transaction, the Head will be stuck in the _initialising_ stage

There are couple of items in the roadmap around reducing the risk of locking up funds in a Hydra Head:

- [Directly open heads](https://github.com/input-output-hk/hydra/issues/1329)
- [Always abortable Head](https://github.com/input-output-hk/hydra/issues/699)
- [Limit size/complexity of UTxOs in the Head](https://github.com/input-output-hk/hydra/issues/698)
- [Only sign closable snapshots](https://github.com/input-output-hk/hydra/issues/370)

### Training wheels

- There is a hard-coded limit on **mainnet** network where you can
  only commit up to 100 ADA into the Hydra head. This is only a safety
  precaution and is going to be increased as we gain more experience
  in running Hydra heads on the mainnet.
  
### Known bugs

Refer also to the project repository issue tracker for [known issues](https://github.com/input-output-hk/hydra/issues?q=is%3Aissue+is%3Aopen+label%3A%22bug+%3Abug%3A%22) with the code. Should you discover any security relevant problems, please follow our [security policy](https://github.com/input-output-hk/hydra?tab=security-ov-file#readme).
