```
module Hydra.Protocol.Setup where

open import Hydra.Protocol.Prelude
```

#import "/template.typ": *
#import "/macros.typ": *

#pagebreak()
= Protocol Setup <sec:setup>
In order to create a head-protocol instance, an initiator invites a set of
participants (the initiator being one of them) to join by announcing to them the
protocol parameters.

- For on-chain transaction authentication (Cardano) purposes, each party
  $party_i$ generates a corresponding key pair $(msVK_i, msSK_i)$
  and sends their verification key $msVK_i$ to all other parties. In
  the case of Cardano, these are Ed25519 keys.

- For off-chain signing (Hydra) purposes, a very basic multisignature scheme (MS, as defined in @sec:multisig) based on EdDSA using Ed25519 keys is used:
  - $msKeyGen$ is Ed25519 key generation (requires no parameters)
  - $msSign$ creates an EdDSA signature
  - $msCombVK$ is concatenation of verification keys into an ordered list
  - $msComb$ is concatenation of signatures into an ordered list
  - $msVfy$ verifies the "aggregate" signature by verifying each individual EdDSA signature under the corresponding Ed25519 verification key

  To help distinguish on- and off-chain key sets, Cardano verification
  keys are written $cardanoKey$, while Hydra verification keys are
  indicated as $hydraKey$ for the remainder of this document.

- Each party $party_i$ generates a hydra key pair and sends their hydra verification key to all other parties.

- Each party $party_i$ computes the aggregate key from the received verification keys, stores the aggregate key,
  their signing key as well as the number of participants $nop$.

- Each party establishes pairwise communication channels to all other parties. That is, every network message received from a specific party is checked for (channel) authentication. It is the implementer's duty to find a suitable authentication process for the communication channels.

- All parties agree on a contestation period $Tcontest$.

If any of the above fails (or the party does not agree to join the head in the
first place), the party aborts the initiation protocol and ignores any further
action. Finally, at least one of the participants posts the $mtxInit$ transaction
onchain as described next in @sec:on-chain.

The parameters each party stores after a successful setup are captured by an
Agda record (@agda-appendix; verification keys and the contestation period are kept
abstract, the latter as a duration in time units).

```agda
record HeadParameters : Set where
  field
    n                  : ℕ          -- number of participants (n)
    cardanoVKeys       : List VKey  -- per-party Cardano verification keys k_C
    hydraVKeys         : List VKey  -- per-party Hydra verification keys k_H
    aggregateKey       : VKey       -- aggregate Hydra key k̃_H
    contestationPeriod : ℕ          -- T_contest
```
