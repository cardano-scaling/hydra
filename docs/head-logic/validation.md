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
| [SnapshotDecommitSpec](../../hydra-node/test/Hydra/HeadLogic/SnapshotDecommitSpec.hs) | ReqSn before ReqDec, payload promotion, competing reservations, invalid signatures, duplicate protection, settlement ordering, side-loading, serialized recovery and bounded Agda agreement. |
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

### Crem API integration, 2026-11-07

The [Crem API integration](crem-dependency.md) covers all eight workflows and
six state owners. ReqTx, AckSn, Decommit, ReqDec and Settlement lose reply-handling
phases. Their frozen machines include the current decommit fixes and the previous
workflow runner. Their comparisons omit no instructions. Rules whose bodies are
unchanged compare the previous and current runners.

| Check | Result |
| --- | --- |
| Crem library tests | 95 Hspec tests pass, including reply-selected destinations, composition, hoisting, metadata removal, inspector replacement and description-only checks. |
| Crem doctests and demo | Four doctests pass. The demo runs both replies and prints the topology, instructions and retained-state observations. |
| Forbidden transition | The allowed counterpart compiles; `Accepted → Checking` fails specifically on its `AllowedTransition` constraint. |
| Saved patch | Applies to a clean v0.1.1.0 tree; all 21 changed files match the local checkout byte for byte. |
| HeadLogic | All 527 tests pass, including 45 new migration checks: 35 exact fixtures, 1,000 arbitrary ReqTx cases, 300 cases each for Decommit and ReqDec, dependent transaction prefixes, all 31 input fixtures, and phase inspection after execution and reconstruction. |
| Bounded Agda reference | The equations typecheck with Agda 2.7.0.1; all 116 reference tests and hermetic extraction drift checks pass. Regeneration changes only `OffChainReference.hs`. |
| Diagrams | The compiled CLI compares 22 examples. All 31 input traces and all four generated owner diagrams are unchanged byte for byte. Five rule topologies are shorter; the others are unchanged. All 92 sources remain indexed. |
| Lint | `just lint` passes, including compilation with warnings treated as errors. Existing golden files are unchanged. |
| Full `just check` | All 1,512 node, 278 transaction, 116 Agda and 18 chain-observer tests pass. Specification, extraction drift, formatting and unused-code checks pass; unchanged checks are reused. |
| Imports and links | Changed handwritten Haskell uses explicit or qualified imports, except the permitted Preludes. Changed Markdown's local links resolve. |
| Real nodes | All three offline-mode checks pass against the full migration: transaction confirmation after restart, persistence rotation and recovery, and two-party networked confirmation. |

The two-party check fails during etcd startup when this environment's HTTP proxy
handles its advertised `0.0.0.0` peer addresses. All three checks pass with those
local addresses excluded from the proxy; no node or test code changes are needed:

```sh
NO_PROXY="${NO_PROXY:+$NO_PROXY,}0.0.0.0,127.0.0.1,localhost" \
  no_proxy="${no_proxy:+$no_proxy,}0.0.0.0,127.0.0.1,localhost" \
  just test hydra-cluster 'Offline mode'
```

This is a targeted cluster check, not a full cluster or TUI suite run.

### Carried-decommit repair, 2026-11-06

Verification on 2026-11-06, including the carried-decommit repair:

| Check | Result |
| --- | --- |
| `just test hydra-node HeadLogic` | All 482 tests pass, including 10 carried-decommit tests, the Cardano witness-stripped duplicate regression, 17 owner tests and the merged capacity regressions. |
| Full `just check` | 1,467 node, 278 transaction, 116 Agda and 18 chain-observer tests pass; unchanged Plutus checks reused. |
| Specification and extraction | Agda typechecking, PDF, trust-ledger checks and extraction drift pass. |
| Diagrams | All 60 current and 32 archived sources are indexed. The new carried-decommit trace renders in Chromium. |
| Imports and links | All 20 changed handwritten Haskell files use explicit or qualified imports, except the permitted Preludes; changed Markdown links resolve. |
| Final checks | `just lint` and `just check` pass. Existing golden files are unchanged. |

The branch includes master `0aa8987dc` (PR #2927) and its 12 capacity regressions.
The [state-transition analysis](state-transitions.md) records the earlier
reservation and committee-binding repairs, the decommit phase relation and the
compiler check rejecting `AwaitingSettlement → DecommitPending`. The topology is
unchanged by the carried-payload repair.

## Carried-decommit regression

The new regression first failed by emitting `ReqSn 0 2 [20] Nothing Nothing`:
confirmation had lost the decommit received through ReqSn. It now confirms
snapshot 2 with that transaction carried forward. The
[repair and executable example](state-transitions.md#fixed-a-certificate-without-its-carried-transaction)
use the normal dispatcher, ledger and verifier.

Tests also cover competing reservations, invalid final signatures, duplicate and
expired ReqTx, a witness-stripped Cardano duplicate, settlement before the last
acknowledgement, side-loading and old checkpoints missing the payload. Every
retained event prefix in the sequence tests checks independently corrected replay
and serialized events and checkpoints. Bounded Agda comparisons cover payload
selection and the same-version carry obligation; their exclusions are explicit.

Frozen handlers remain unchanged. Compatibility tests use known decommit
payloads or exclude the corrected paths; the regressions assert the new behavior
independently. The earlier 24 conflicting-decommit delivery schedules still pass.

The REPL reports a `TxIn` thunk in the existing snapshot UTxO strictness test. The
compiled test passes in both the 482-test run and the full node suite. This mode
discrepancy remains unexplained; it is not introduced by this repair.

The confirmation and carried-payload repairs have not been benchmarked end to
end. [Measurements](measurements.md#cost-of-the-confirmation-repairs) records their
additional work separately from the earlier throughput results. Earlier cluster
and TUI runs also belong to the revisions identified there.

## Commands

For same-package edits, load the test suite in a REPL and run the relevant group:

```sh
cabal repl hydra-node:test:tests
```

```text
:main -p "/Capacity/"
:main -p "/coordinated state owners/"
:main -p "/snapshot decommit retention/"
```

Restart the test REPL when the node library, test library or another package
changes: `:reload` does not rebuild those dependencies. `just test` also rebuilds them. Finish with the repository checks:

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
