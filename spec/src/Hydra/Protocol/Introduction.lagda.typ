```
module Hydra.Protocol.Introduction where
```

#import "/template.typ": *
#import "/macros.typ": *

= Introduction

This document specifies the 'Coordinated Hydra Head' protocol to be implemented
as the first version of Hydra Head on Cardano - *Hydra HeadV1*. The
protocol is derived from variants described in the original paper
@hydrahead20, but was further simplified to make a first implementation on
Cardano possible.

Note that the format and scope of this document is (currently) also inspired by
the paper and hence does not include a definition of the networking protocols or
concrete message formats. #todo[Add: network specification (message formats)] It
is structured similarly, but focuses on a single variant, and avoids
indirections and unnecessary generalizations. The document is kept in sync with
the reference implementation available on Github~@hydra-repo.

First, a high-level overview of the protocol and how it differs from legacy
variants of the Head protocol is given in @sec:overview. Relevant
definitions and notations are introduced in @sec:prel, while
@sec:setup describes protocol setup and assumptions. Then, the
actual on-chain transactions of the protocol are defined in
@sec:on-chain, before the off-chain protocol part specifies
behavior of Hydra parties off-chain and ties the knot with on-chain transactions
in @sec:offchain. At last, @sec:security gives the
security definition, properties and proofs for the Coordinated Head protocol.
