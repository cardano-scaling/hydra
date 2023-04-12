# Known issues & limitations

Please be aware of the following limitations before running hydra-node with `--mainnet`, as they could put your funds at risk.

### Head protocol

#### Layer-1/Layer-2

The current transaction size on mainnet is limited to 16KB, a limitation which has the following consequences:
  - The protocol can only handle a maximum number of participants by Head (see [cost of collectcom transaction](/benchmarks/transaction-cost/#cost-of-collectcom-transaction)). Upon startup, the `hydra-node` will inform you of the current configured maximum when trying to configure too many peers.
  - Each party can only commit zero or one utxo into a Head.

It's possible to have funds locked at various stages in a Hydra Head because of the complexity or size of the UTxO being committed or created while the Head is open:
  - The head cannot be _finalized_ if holding more than ~100 assets (see [cost of fanout transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-fanout-transaction) for latest numbers), although it can be _closed_
  - If one or more  participants commit UTxO too large to be processed together in a `CollectCom` or `Abort` transaction, the Head will be stuck in the _initialising_ stage
  - Tokens minted and not burnt in an _open_ head will prevent it from being _finalized_
  - Committing reference inputs from L1 to a Head is problematic and the hydra-node will prevent this should a client try to do so. Note that a `Commit` transaction could perfectly be crafted outside of the hydra-node and would therefore put the Head in an uncloseable state
  - Using reference inputs on the L2 is non problematic as they will be committed back on the L1 along with all the other UTxO

#### Networking

Note the messages exchanged through the _Hydra Network_ layer between participants are neither authenticated, authorized, nor encrypted. It's therefore advised that operators requiring confidentiality and identification of participants run hydra-node connected through some kind of VPN or on top of encrypted channels.

Also, while the Hydra Head protocol guarantees safety of a participant's funds, it does not guarantee liveness, so all parties involved in a Hydra Head must be online and reactive for the protocol to make progress.

#### Remediations

There are couple of ideas in the roadmap around reducing the risk of loosing funds in a Hydra Head:
  - [Always abortable Head](https://github.com/input-output-hk/hydra/issues/699)
  - [Limit size/complexity of UTxOs in the Head](https://github.com/input-output-hk/hydra/issues/698)
  - [Only sign closable snapshots](https://github.com/input-output-hk/hydra/issues/370)

### hydra-node

Independently from the Head protocol itself, the way the hydra-node is implemented has the following consequences:

- The hydra-node will reject attempts to `Commit` reference scripts UTxO
- There is a hard-coded limit on **mainnet** network where you can only commit up to 100 ADA into the Hydra head which is meant to prevent the users from _shooting themselves in the foot_ until we have more experience running Hydra Heads on the mainnet.
- The internal wallet of `hydra-node` which is used to drive Hydra protocol transactions requires a UTXO to be marked as "fuel" (see [user manual](/docs/getting-started/demo/with-docker/#seeding-the-network)
- You _need_ to match the hydra-node version with the appropriate scripts published for this release and make sure to choose the correct network w.r.t the given scripts reference.

### hydra-tui

- TUI crashes when user tries to post a new transaction wihout any UTXO remaining.

- Recipient addresses to send money to in the TUI are inferred from the current UTXO set. If a party does not commit a UTXO or consumes all its UTXO in a Head, it won't be able to send or receive anything anymore.
