# Prior spike

This directory holds the proposals, investigations, migration comparisons, and
superseded results. Most were archived from revision `a6c4966f8`; the original
conflicting-decommit trace comes from `86e74b177`, before its confirmation repair. The implemented design is
documented in the [Crem/Program review guide](../head-logic-review.md).

| Document | Contents |
| --- | --- |
| [Original sketch](original-sketch.md) | The interpreted-program proposal, proposed module layout, and settlement-backend ideas. |
| [Agda generation proposal](agda-generated.md) | Speculative generation of production Haskell from Agda; no generated production rules. |
| [Lab record](lab.md) | The resolved-change experiment, maintenance mutation, discovery runs, and earlier validation records. |
| [Demonstration](demonstration.md) | Registry corruption, retry-expiry exploration, and settlement/rollback investigations. |
| [Bug investigations](bugs.md) | How the defects were reproduced, repaired, or retained as recovery-policy limits. |
| [Capacity investigation](capacity.md) | The #2927 analysis, historical stall, and reasoning behind the request contract. |
| [Earlier measurements](measurements.md) | Results for intermediate implementations and experimental resolved replay. Raw results are in `measurements/`. |
| [Workflow comparisons](workflows/README.md) | Paired frozen-program and Crem/Program traces from the migration. |
| [Diagrams](diagrams.md) | Every archived `.mmd` rendered with its explanation and source link. |
| [Original experiment index](experiments.md) | The index used before separating current documentation from the archive. |

Historical claims and commands belong to the revisions named in each document.
Some tapes use superseded formats or primitive-query sequences. Keep these as
records; generating them with current code can change what they demonstrate.
Raw measurement commands, hashes, and timestamps remain as recorded.

Current [tracing commands](../head-logic/tools.md),
[protocol checks](../head-logic/checks.md), and
[performance results](../head-logic/measurements.md) live outside this archive.
The [central diagram index](../head-logic-diagrams.md) also lists every archived
source, separately from the current diagrams.
