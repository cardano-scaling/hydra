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

#bibliography("/short.bib", style: "springer-basic")
