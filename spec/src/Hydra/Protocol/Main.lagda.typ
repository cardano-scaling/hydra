#import "/template.typ": *
#import "/macros.typ": *

#show: body => hydra-spec(
  title: "Hydra HeadV2 Specification: Coordinated Head protocol",
  subtitle: "DRAFT",
  authors: (
    (name: "Sebastian Nagel", email: "sebastian.nagel@iohk.io"),
    (name: "Sasha Bogicevic", email: "sasha.bogicevic@iohk.io"),
    (name: "Franco Testagrossa", email: "franco.testagrossa@iohk.io"),
    (name: "Daniel Firth", email: "daniel.firth@iohk.io"),
    (name: "Noon van der Silk", email: "noon.vandersilk@iohk.io"),
    (name: "Veronika Romashkina", email: "veronika.romashkina@iohk.io"),
  ),
  body,
)

```
module Hydra.Protocol.Main where

import Hydra.Protocol.Prelude
import Hydra.Protocol.Introduction
import Hydra.Protocol.Overview
import Hydra.Protocol.Preliminaries
import Hydra.Protocol.Setup
import Hydra.Protocol.OnChain
import Hydra.Protocol.OffChain
import Hydra.Protocol.Security
-- The machine-checked §7 proof terms (typecheck-only; the rendered §7 shows the model + statements).
import Hydra.Protocol.SecurityProofs
-- Extractable decidable reference checker + the bridge proving it reflects the on-chain
-- validity bundles (Tier 2 differential-testing; not rendered in the document).
import Hydra.Protocol.Reference
import Hydra.Protocol.ReferenceBridge
```

#include "Introduction.typ"
#include "Overview.typ"
#include "Preliminaries.typ"
#include "Setup.typ"
#include "OnChain.typ"
#include "OffChain.typ"
#include "Security.typ"

#pagebreak()

= Agda formalisation <agda-appendix>

The specification above is _literate Agda_: every definition, validity bundle, and proof is
machine-checked by Agda (`agda Main.lagda.typ`) as part of building this document. To keep the body
readable, the rendered Agda is collected here rather than shown inline; each block appears under the
section it supports, in document order, and the body links here in place. This appendix is the
rendered form of the same typechecked source, not a copy of it.

#agda-appendix-mode.update(true)

#context {
  let blocks = query(<agda-src>)
  let cur = none
  for b in blocks {
    if b.value.secnum != cur {
      heading(level: 2, b.value.sec)
      cur = b.value.secnum
    }
    raw(b.value.src, lang: "agda", block: true)
  }
}

#bibliography("/short.bib", style: "springer-basic")
