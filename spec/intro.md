# Introduction

This document specifies the 'Coordinated Hydra Head' protocol to be
implemented as the first version of Hydra Head on Cardano - **Hydra
HeadV1**. The protocol is derived from variants described in the
original paper [@hydrahead20], but was further simplified to make a
first implementation on Cardano possible.

Note that the format and scope of this document is (currently) also
inspired by the paper and hence does not include a definition of the
networking protocols or concrete message formats. It is structured
similarly, but focuses on a single variant, and avoids indirections and
unnecessary generalizations. The document is kept in sync with the
reference implementation available on Github [@hydra-repo].
[Red]{style="color: red"} sections indicate that they are currently not
covered or missing in the implementation, where
[blue]{style="color: blue"} parts mean a difference in how it is
realized.

First, a high-level overview of the protocol and how it differs from
legacy variants of the Head protocol is given in
Section [\[sec:overview\]](#sec:overview){reference-type="ref"
reference="sec:overview"}. Relevant definitions and notations are
introduced in Section [\[sec:prel\]](#sec:prel){reference-type="ref"
reference="sec:prel"}, while
Section [\[sec:setup\]](#sec:setup){reference-type="ref"
reference="sec:setup"} describes protocol setup and assumptions. Then,
the actual on-chain transactions of the protocol are defined in
Section [\[sec:on-chain\]](#sec:on-chain){reference-type="ref"
reference="sec:on-chain"}, before the off-chain protocol part specifies
behavior of Hydra parties off-chain and ties the knot with on-chain
transactions in
Section [\[sec:offchain\]](#sec:offchain){reference-type="ref"
reference="sec:offchain"}. At last,
Section [\[sec:security\]](#sec:security){reference-type="ref"
reference="sec:security"} gives the security definition, properties and
proofs for the Coordinated Head protocol.
