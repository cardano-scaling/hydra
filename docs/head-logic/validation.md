# Crem/Program validation

The validation covers input execution, state application, persisted recovery,
and selected independent protocol properties. It does not prove the complete
Haskell implementation or distributed progress.

## Test locations

| Tests | Coverage |
| --- | --- |
| [ProgramSpec](../../hydra-node/test/Hydra/HeadLogic/ProgramSpec.hs), [SnapshotProgramSpec](../../hydra-node/test/Hydra/HeadLogic/SnapshotProgramSpec.hs) | Ordered outcomes, wait/reject boundaries, primitive replies, snapshot rounds, and bounded Agda comparisons. |
| [RulesSpec](../../hydra-node/test/Hydra/HeadLogic/RulesSpec.hs), [InputWorkflowSpec](../../hydra-node/test/Hydra/HeadLogic/InputWorkflowSpec.hs) | Full-input dispatch, admission, client commands, settlement adoption, rollback, and fanout. |
| [MachineSpec](../../hydra-node/test/Hydra/HeadLogic/MachineSpec.hs), [CoordinatedSpec](../../hydra-node/test/Hydra/HeadLogic/CoordinatedSpec.hs) | Ownership, uninterrupted versus reconstructed runtime, confirmation evidence, reservation, settlement, and serialized recovery. |
| [CapacitySpec](../../hydra-node/test/Hydra/HeadLogic/CapacitySpec.hs), [IncrementalWorkflowSpec](../../hydra-node/test/Hydra/HeadLogic/IncrementalWorkflowSpec.hs) | Bounded selection/pruning, stale proposals, carried capacity, decommit dependencies, and real Cardano regressions. |
| [HeadLogicSpec](../../hydra-node/test/Hydra/HeadLogicSpec.hs), [HeadLogicSnapshotSpec](../../hydra-node/test/Hydra/HeadLogicSnapshotSpec.hs) | Production dispatcher behavior, transaction registry integrity, and snapshot acceptance. |

Compatibility tests compare complete ordered outcomes and states with frozen
implementations. Serialized event and checkpoint round trips check the persisted
projection. Deliberately changed behavior has independent acceptance tests rather
than an altered frozen reference. Arbitrary open-state fixtures align head IDs;
separate tests check rejection of stale and foreign proposals.

See [protocol checks](checks.md) for the independent invariants and each Agda
projection's exclusions. Existing protocol, simulation, persistence, and golden
tests also run against the node.

## Recorded verification

The implementation and performance verification on 2026-11-06 covers production
revision `968ea82f7` and benchmark revision `bdc5bcb0d`.

| Check | Result |
| --- | --- |
| Full `just check` | 1,437 node, 278 transaction, 116 Agda, and 18 chain-observer tests pass; unchanged Plutus checks reused. |
| Specification and extraction | Agda typechecking, PDF, trust-ledger checks, and extraction drift pass. |
| Focused checks | 123 capacity/workflow tests pass in a fresh REPL; four compatibility properties pass 1,000 cases each. |
| TUI | All 24 tests pass against the current Nix-built node with `TERM=xterm-256color TASTY_NUM_THREADS=1`. |
| Cluster measurements | All 16 runs confirm 60,000 transactions with zero invalid/refused submissions and complete fanout. Performance regressions are recorded in [measurements](measurements.md). |
| Capacity driver/search | The driver confirms snapshot 5 after 30 deliveries. The search visits 100 states with a 40-delivery bound and no failure; it is truncated at the state limit. |
| Saved scenarios | All five version-2 reply tapes validate. |
| Diagrams | All 58 current and 31 archived sources render and appear in the [central index](../head-logic-diagrams.md). |
| Imports | All 93 changed handwritten Haskell files use explicit or qualified imports, except the permitted Preludes. |
| Final checks | `just lint` and `just check` pass. |

The subsequent [adversarial review](adversarial-review.md) adds executable
witnesses for a conflicting decommit and unbound verification keys. Both
reproduce after rebasing onto master `0aa8987dc` (PR #2927), and all 12
[focused upstream capacity checks](capacity.md#merged-fix-and-regression-tests)
pass in a fresh test REPL. The review's pre-rebase HeadLogic run passed 452 of 453
tests: the snapshot UTxO strictness test failed in interpreted mode and passed
in the compiled `-O1` binary. That mode discrepancy remains unexplained.
The new trace brings the central index to 59 current and 31 archived diagrams.
After the rebase, `just lint` and `just check` pass. The compiled node suite runs
all 1,448 tests successfully, including the strictness check; unchanged package
and specification checks are reused. The Agda extraction is regenerated during
conflict resolution and matches the capacity port's existing generated code.

## Commands

For same-package edits, load the test suite in a REPL and run the relevant group:

```sh
cabal repl hydra-node:test:tests
```

```text
:main -p "/Capacity/"
:main -p "/coordinated state owners/"
```

For cross-package changes, restart the REPL or use `just test`; reloading does
not rebuild its dependencies. Finish with the repository checks:

```sh
just test hydra-node HeadLogic
just test hydra-agda
nix develop .#spec --command hydra-agda/regenerate.sh
python3 scripts/head-logic-diagrams.py --check
just lint
just check
```

`just check` reuses unchanged package checks. TUI and cluster tests run separately;
the [measurement commands](measurements.md#reproduce-the-cluster-comparison) use
explicit node binaries and include init, transaction confirmation, close, and
fanout.
