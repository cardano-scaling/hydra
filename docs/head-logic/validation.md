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

Verification on 2026-11-06, after the confirmation and phase repairs:

| Check | Result |
| --- | --- |
| Full `just check` | 1,456 node, 278 transaction, 116 Agda, and 18 chain-observer tests pass; unchanged Plutus checks reused. |
| Specification and extraction | Agda typechecking, PDF, trust-ledger checks, and extraction drift pass. |
| Focused checks | All 471 HeadLogic tests pass, including 17 owner tests and the merged capacity regressions. |
| Forbidden transition | `NoDecommit → DecommitPending` typechecks; `AwaitingSettlement → DecommitPending` fails with the missing Crem edge. |
| Diagrams | All 59 current and 32 archived sources are indexed; the changed owner graph and both conflict traces render. |
| Imports and links | Changed handwritten Haskell imports are explicit or qualified, except the permitted Preludes; changed Markdown links resolve. |
| Final checks | `just lint` and `just check` pass. |

The earlier cluster/TUI runs and performance measurements belong to the revisions
identified in [measurements](measurements.md); they are not reruns of this repair.

## Confirmation and phase repairs

The branch is rebased onto master `0aa8987dc` (PR #2927), including its 12
capacity regressions. The follow-up repairs the conflicting reservation and
committee-binding findings, separates the trusted replay adapter from live
commands, and introduces the three decommit phases described in
[state transitions](state-transitions.md).

- All 471 compiled HeadLogic tests pass, including the 17 owner tests and the
  upstream capacity regressions. The owner tests cover 24 conflicting delivery
  orders, serialized recovery, retained dependencies, multi-input release,
  context-bound evidence and certificate-based admission.
- Agda extraction and the full specification build pass, including the decommit
  transition relation, forbidden-edge proof, admission premise and independent
  bounded predicates.
- The positive Crem edge compiles; changing only its source to
  `AwaitingSettlement` fails with the missing `AllowedTransition` instance.
- All 91 Mermaid sources are indexed: 59 current and 32 archived. The updated
  owner graph and both conflict traces render in Chromium. Local links and
  explicit imports in the changed handwritten Haskell files are checked.
- The normal-interpreter example for the additional
  [missing carried transaction](state-transitions.md#unresolved-a-certificate-without-its-carried-transaction)
  reproduces the rejected next request. It characterises an unresolved defect.
- `just lint` and the complete `just check` pass, including persistence, golden,
  simulation and specification checks. Existing golden files are unchanged.

Earlier interpreted-mode runs reported a `TxIn` thunk in the snapshot UTxO
strictness test. The compiled test passes, including in the current 471-test run;
the earlier mode discrepancy remains unexplained. Existing golden files are
unchanged. The new confirmation work has not been benchmarked end to end; the
[measurement page](measurements.md#cost-of-the-confirmation-repairs) states its
additional cost separately from earlier results.

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
