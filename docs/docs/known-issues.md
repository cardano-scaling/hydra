# Known issues & limitations

Please be aware of the following limitations before running hydra-node
on the Cardano `--mainnet`; as an incredibly technical project, Hydra
in its current form requires an elevated level of understanding of the
underlying infrastructure. Without the expertise required to operate a
hydra-node, users may put their funds at risk if they are unfamiliar
with the implementation and usage process.

### Head protocol

#### Layer-1/Layer-2

The current transaction size on mainnet is limited to 16KB, a limitation which has the following consequences:

- The protocol can only handle a maximum number of participants in a
  Head (see [cost of collectcom
  transaction](/benchmarks/transaction-cost/#cost-of-collectcom-transaction)). Upon
  startup, the `hydra-node` will inform you of the current
  configured maximum when trying to configure too many peers.

It's currently possible to be denied access to funds by other protocol
participants at various stages in a Hydra Head because of the
complexity or size of the UTxO being committed or created while the
Head is open:

- The head cannot be _finalized_ if holding more than ~60 assets
  (see [cost of fanout
  transaction](https://hydra.family/head-protocol/benchmarks/transaction-cost/#cost-of-fanout-transaction)
  for latest numbers), although it can be _closed_
- Tokens minted and not burnt in an _open_ head will prevent it from being _finalized_
- If one or more participants commit UTxO too large to be processed
  together in a `CollectCom` or `Abort` transaction, the Head will
  be stuck in the _initialising_ stage
- Committing reference scripts from L1 to a Head is problematic and
  the hydra-node will prevent this should a client try to do
  so. Note that a `Commit` transaction could perfectly be crafted
  outside of the hydra-node and would therefore put the Head in an
  uncloseable state
- Using reference scripts on the L2 is non problematic as they will
  be committed back on the L1 along with all the other UTxO

There are couple of items in the roadmap around reducing the risk of loosing funds in a Hydra Head:

- [Always abortable Head](https://github.com/input-output-hk/hydra/issues/699)
- [Limit size/complexity of UTxOs in the Head](https://github.com/input-output-hk/hydra/issues/698)
- [Only sign closable snapshots](https://github.com/input-output-hk/hydra/issues/370)

#### Networking

The messages exchanged through the _Hydra Network_ layer between
participants are neither authenticated, authorized, nor encrypted
which means communications between Hydra nodes are not protected. It's
advised that operators requiring confidentiality and/or identification
of participants run hydra-node connected through some kind of VPN or
on top of encrypted channels until this is addressed in the software
(see [#727](https://github.com/input-output-hk/hydra/issues/727))

Also, while the Hydra Head protocol guarantees safety of a
participant's funds, it does not guarantee liveness, so all parties
involved in a Hydra Head must be online and reactive for the protocol
to make progress. This means that, should one or several participants'
Hydra node crash, become unreachable from other Hydra nodes, or is
disconnected from the Cardano network, no more transactions can happen
in the Head and it must be closed.

### hydra-node

Independently from the Head protocol itself, the way the hydra-node is
implemented has the following consequences:

- There is a hard-coded limit on **mainnet** network where you can
  only commit up to 100 ADA into the Hydra head. This is only a safety
  precaution and is going to be increased as we gain more experience
  in running Hydra heads on the mainnet.

### hydra-tui

- TUI crashes when user tries to post a new transaction wihout any UTXO remaining.

- Recipient addresses to send money to in the TUI are inferred from
  the current UTXO set. If a party does not commit a UTXO or consumes
  all its UTXO in a Head, it won't be able to send or receive anything
  anymore.
