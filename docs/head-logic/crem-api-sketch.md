# Testing the Crem API changes in Hydra

The Crem checkout is at `~/dev/w/tmp/crem-headlogic-sketch`, on branch
`headlogic-api-sketch`. It starts at `v0.1.1.0`, commit
`f06f8b47ac83d3e7448493bdee374c5f952f7f4c`. Hydra's ignored
`cabal.project.local` includes it as a local package. The
[saved patch](crem-api-sketch.patch) reproduces the library changes; the temporary
[Nix override](../../nix/crem-api-sketch.nix) applies that patch for sandboxed
builds. The public package version and lock file are unchanged.

This experiment uses the new API throughout the node's Crem workflows and state
owners. Five rules lose a phase used to handle a primitive reply. Exact comparisons
check their previous machines. Network messages, retained events, checkpoints,
terminal semantics, and Program instruction order stay the same.

## Code to read

Start with [Workflow.hs](../../hydra-node/src/Hydra/HeadLogic/Workflow.hs).
`choose` wraps Crem's `ChooseTransition`; `next` constructs a destination with
an allowed-edge constraint. A Program can now choose its destination after a
primitive returns:

```haskell
choose $ do
  result <- Program.applyTransaction currentSlot localUTxO tx
  pure $ case result of
    Right after -> next $ DepositCheck runtime after
    Left err -> next $ Retry runtime err
```

The actual [ReqTx rule](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs) also
records its existing named decision before this query. Its ledger reply goes
straight from `CheckOutputs` to `CheckDeposits` or `RetryOrInvalidate`; the
intermediate `CheckLedgerResult` phase and payload are gone.

[AckSn](../../hydra-node/src/Hydra/HeadLogic/Rules/AckSn.hs) similarly moves from
`CheckComplete` directly to `ConfirmAndContinue` after verification. Confirmation
still requires the private `VerifiedSnapshot` value. Invalid verification still
rejects through Program, discarding the signature staged earlier in the attempt.
The partial-ack path still records its signature twice.

Client [Decommit](../../hydra-node/src/Hydra/HeadLogic/Rules/Decommit.hs) and
[ReqDec](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqDec.hs) also remove
`CheckLedgerResult`: each handles failure or enters its output checks directly.
The ledger check still precedes the empty-output check. ReqDec still waits while
its retry budget remains and invalidates after expiry.

[Settlement](../../hydra-node/src/Hydra/HeadLogic/Rules/Settlement.hs) removes
`CheckVerification`. Candidate selection now verifies and optionally adopts the
snapshot before entering `RequestNext`. Finalization still precedes verification
and survives invalid signatures. Both branches enter the same next phase here;
the new API makes this notation consistent, but the old API could also express
this particular simplification.

ReqSn, Recover and input admission retain their existing boundaries. Those phases
separate protocol work, rather than storing a reply just to examine it next.
All eight workflows use `Workflow.workflow` to attach phase descriptions and the
same completion convention. Each rule's `statePhase` matches its constructors;
labels come from the phase names and do not inspect payloads.

`Workflow.run` uses `Crem.Executable`, so execution has no rendering constraints.
The rules retain their topology metadata for diagrams. The two owner compositions
in [Machine](../../hydra-node/src/Hydra/HeadLogic/Machine.hs) and
[Coordinated](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated.hs) use
`withoutRendering` in `start`. Their `components` definitions also supply the
diagrams: there is only one definition of each composition. All six owners have
phase descriptions, including deposit restoration and rollback. Their runtime
state can contain opaque machines and intentionally lazy projections, so state
checks report `NotInspected`.

The following paths are relative to the Crem checkout:

| Code | Change |
| --- | --- |
| `src/Crem/BaseMachine.hs` | `Transition` carries each branch's allowed-edge evidence; `ChooseTransition` selects it inside an effect. `withStateInspector` attaches observations to a restored existential state. |
| `src/Crem/StateMachine.hs` | `Executable` runs without singleton or rendering instances. `withoutRendering` drops those dictionaries from a composition while preserving its actions and inspectors. `Basic` remains available for diagrams. |
| `src/Crem/StateInspection.hs`, `src/Crem/Inspect.hs` | Optional descriptions and explicit check results survive execution, composition, hoisting and decider replay. |
| `examples/Crem/Example/Transaction.hs`, `Transaction/Program.hs` | A small runnable Program-in-Crem example with both ledger replies. |
| `spec/Crem/ApiSketchSpec.hs`, `typecheck/ForbiddenTransition.hs` | Runtime coverage and a compile-fail test for an effect-selected edge outside the topology. |

## What the comparisons cover

[RulesSpec](../../hydra-node/test/Hydra/HeadLogic/RulesSpec.hs) compares the actual
programs with [ReqTx](../../hydra-node/testlib/Test/Hydra/HeadLogic/Crem/BeforeApi/ReqTx.hs),
[AckSn](../../hydra-node/testlib/Test/Hydra/HeadLogic/Crem/BeforeApi/AckSn.hs),
[Decommit](../../hydra-node/testlib/Test/Hydra/HeadLogic/Crem/BeforeApi/Decommit.hs),
[ReqDec](../../hydra-node/testlib/Test/Hydra/HeadLogic/Crem/BeforeApi/ReqDec.hs) and
[Settlement](../../hydra-node/testlib/Test/Hydra/HeadLogic/Crem/BeforeApi/Settlement.hs)
frozen immediately before this API migration. These references include the
current decommit repairs and retain the previous `Basic`-based workflow runner.
Unlike the older handler migration, this comparison
filters no decisions and excludes no decommit states.

The shared [comparison harness](../../hydra-node/testlib/Test/Hydra/HeadLogic/Crem/Comparison.hs)
checks complete ordered outcomes, instruction traces, primitive arguments and
replies, staged payloads, replay and serialized resulting state. Fixtures cover
success, waits, expiry, collisions, duplicates, early/partial/final acknowledgements,
and invalid signatures. Settlement fixtures also exercise stale versions and
nonmatching round numbers. Bounded arbitrary ReqTx, Decommit and ReqDec states
check both ledger branches; dependent transaction sequences check every input
prefix. Unchanged ReqSn, Recover and input bodies compare the old and new runners.

[MachineSpec](../../hydra-node/test/Hydra/HeadLogic/MachineSpec.hs) and
[CoordinatedSpec](../../hydra-node/test/Hydra/HeadLogic/CoordinatedSpec.hs) check
owner descriptions after execution and reconstruction. Bottom-valued UTxOs and
transaction sequences check that phase inspection does not force their lazy
projections. Existing event-prefix tests compare full state and serialized
recovery with the independent reducer.

The [specification](../../spec/src/Hydra/Protocol/OffChain.lagda.typ) records the
unchanged outcome boundary. Bounded Agda equations check receipt retention,
invalid-final-acknowledgement rejection, decommit check precedence, and settlement
adoption versus version advancement. They do not prove the Haskell implementation
equivalent; the differential tests compare it.

The [diagram index](../head-logic-diagrams.md) renders the shortened topologies.
The corresponding instruction traces are unchanged. [Validation](validation.md#local-crem-api-experiment-2026-11-07)
records the checks for this revision.

## Reproduce the experiment

The checkout already exists in this workspace. To create it elsewhere, run from
the Hydra checkout:

```sh
mkdir -p "$HOME/dev/w/tmp"
git clone --branch v0.1.1.0 https://github.com/marcosh/crem.git "$HOME/dev/w/tmp/crem-headlogic-sketch"
git -C "$HOME/dev/w/tmp/crem-headlogic-sketch" switch -c headlogic-api-sketch
git -C "$HOME/dev/w/tmp/crem-headlogic-sketch" apply "$PWD/docs/head-logic/crem-api-sketch.patch"
```

Add this to `cabal.project.local`, using an absolute path for your checkout:

```cabal
packages: /home/noon/dev/w/tmp/crem-headlogic-sketch

package crem
  tests: False
  benchmarks: False
```

With GHC 9.6.7 and Cabal, run the library checks from that checkout:

```sh
cabal test crem-spec -ferrors --test-show-details=direct
cabal run crem-doctests -ferrors -- --no-nix
bash scripts/check-transition-types.sh
cabal run transaction-sketch
```

The doctest `--no-nix` option uses Cabal's package environment inside Hydra's Nix
shell. The demo prints a Mermaid topology, both decision traces, and retained-state
checks; rendering stays outside the rule.

Then, from Hydra:

```sh
just test hydra-node 'free programs and crem phases'
cabal run hydra-node:head-logic-lab -- compare-crem /tmp/crem-api-diagrams
just lint
just check
```

When changing the library, restart any Hydra REPL: reloading does not rebuild a
dependency. Refresh the saved patch before Nix checks, which cannot see the local
checkout:

```sh
git -C "$HOME/dev/w/tmp/crem-headlogic-sketch" -c diff.suppressBlankEmpty=true \
  diff --binary --no-ext-diff v0.1.1.0 > docs/head-logic/crem-api-sketch.patch
```

New files must be tracked or marked with `git add -N` to enter that patch. Keep
the temporary Nix override and the local library on the same API revision. The
node changes require the patched API; released Crem does not provide it.

## Compatibility and limits

The old `ActionResult` and `Basic` constructors remain usable. Exhaustive matches
need the new constructors. `machineAsGraph` now returns an `Either`: an
`Executable` leaf cannot supply a topology diagram, so rendering reports that
missing metadata instead of producing an incomplete graph.

The unconditional `NoThunks (StateMachineT ...)` instance is removed. It accepted
`Basic _` without checking its contents. Callers now choose how to handle
`NotInspected`. Descriptions and checks are independent; the callbacks cover
retained state data, not action closures or effect environments. Inspection is
not a persistence API.

Crem still permits self-transitions, and the author still chooses the topology.
The changes do not replace Hydra's owner commands, round checks, candidate
evidence, or ledger/crypto primitives. Removing five internal phases is a concrete
maintenance benefit; it is not evidence of a TPS improvement. This API revision
has no new performance measurement.
