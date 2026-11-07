# Crem dependency

The node uses API extensions to Crem 0.1.1.0. The unpatched release cannot build
this branch. The library lives outside the Hydra checkout; the patch kept here
makes the same source available to sandboxed builds.

| Build | Crem source |
| --- | --- |
| Local Cabal | `~/dev/w/tmp/crem-headlogic-sketch`, included by the ignored `cabal.project.local`. |
| Nix, including Windows | [crem.nix](../../nix/crem.nix) fetches commit `f06f8b47ac83d3e7448493bdee374c5f952f7f4c` (`v0.1.1.0`) and applies [crem-api.patch](../../nix/crem-api.patch). |

The patch is an active build dependency, not an archived experiment. It includes
the library changes, their tests and a standalone Program example. The public
package version and Hydra's lock file are unchanged. Cabal does not apply the Nix
patch automatically: use the local checkout described below.

## Where Hydra uses it

[Programs inside crem](../head-logic-crem-workflows.md) explains the execution
model and rules; [retained-event machines](../head-logic-crem.md) explains state
ownership. The library extensions support those definitions:

| Extension | Hydra use |
| --- | --- |
| `Transition`, `ChooseTransition` | [Workflow.hs](../../hydra-node/src/Hydra/HeadLogic/Workflow.hs) exposes `choose` and `next`. A Program reply selects a destination carrying its allowed-edge evidence. ReqTx, AckSn, Decommit, ReqDec and Settlement no longer need separate phases just to handle replies. |
| `Executable`, `withoutRendering` | Workflows execute without rendering dictionaries. [Machine.hs](../../hydra-node/src/Hydra/HeadLogic/Machine.hs) and [Coordinated.hs](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated.hs) drop rendering metadata from the same compositions used for diagrams. |
| `InspectableState`, `withStateInspector` | All eight workflows and six state owners describe their current phase, including after reconstruction. Descriptions match constructors without evaluating retained payloads. They report `NotInspected` for strictness checks. |

In the Crem checkout, read `src/Crem/BaseMachine.hs`, `src/Crem/StateMachine.hs`,
`src/Crem/StateInspection.hs` and `src/Crem/Inspect.hs`. The standalone example is
in `examples/Crem/Example/Transaction.hs` and `Transaction/Program.hs`;
`spec/Crem/ApiSketchSpec.hs` and `typecheck/ForbiddenTransition.hs` cover runtime
behavior and a forbidden edge.

[RulesSpec](../../hydra-node/test/Hydra/HeadLogic/RulesSpec.hs) compares complete
ordered outcomes, instructions, primitive replies and serialized state against
the frozen machines in
[BeforeApi](../../hydra-node/testlib/Test/Hydra/HeadLogic/Crem/BeforeApi).
Those copies are active test references, not alternative production paths. Keep
them through migration review; any later removal must retain the independent
regressions and protocol properties. [Validation](validation.md#crem-api-integration-2026-11-07)
records the library, node, recovery and Agda checks. This API revision has no new
end-to-end performance measurement.

## Set up a Cabal checkout

The checkout already exists in this workspace. To create it elsewhere, run from
Hydra:

```sh
mkdir -p "$HOME/dev/w/tmp"
git clone --branch v0.1.1.0 https://github.com/marcosh/crem.git "$HOME/dev/w/tmp/crem-headlogic-sketch"
git -C "$HOME/dev/w/tmp/crem-headlogic-sketch" switch -c hydra-integration
git -C "$HOME/dev/w/tmp/crem-headlogic-sketch" apply "$PWD/nix/crem-api.patch"
```

Add this to `cabal.project.local`, replacing the package path with your checkout's
absolute path:

```cabal
packages: /home/noon/dev/w/tmp/crem-headlogic-sketch

package crem
  tests: False
  benchmarks: False
```

With GHC 9.6.7 and Cabal, run the library checks from the Crem checkout:

```sh
cabal test crem-spec -ferrors --test-show-details=direct
cabal run crem-doctests -ferrors -- --no-nix
bash scripts/check-transition-types.sh
cabal run transaction-sketch
```

The doctest `--no-nix` option uses Cabal's package environment inside Hydra's Nix
shell. The demo prints a Mermaid topology, both decision traces and retained-state
checks. Then, from Hydra:

```sh
just test hydra-node 'free programs and crem phases'
cabal run hydra-node:head-logic-lab -- compare-crem /tmp/crem-api-diagrams
just lint
just check
```

## Maintain or replace the patch

When changing Crem, restart any Hydra REPL: reloading does not rebuild a
dependency. Refresh the patch from Hydra before Nix checks, which cannot see the
local checkout:

```sh
git -C "$HOME/dev/w/tmp/crem-headlogic-sketch" -c diff.suppressBlankEmpty=true \
  diff --binary --no-ext-diff v0.1.1.0 > nix/crem-api.patch
```

New files must be tracked or marked with `git add -N` to enter that patch. Keep
the patch and local checkout in sync; verify that the patch applies to a clean
`v0.1.1.0` tree and reproduces the changed library files before running the checks.

The override can go once Hydra pins a Crem release or fork revision containing
these APIs. At that point, update the Cabal dependency, remove the local Crem
stanza, the Nix module and patch, and its references in the native and Windows
projects. Run the same migration tests, `just lint` and `just check` against the
replacement. Renaming or deleting the patch alone would break the build.

## Compatibility and limits

The old `ActionResult` and `Basic` constructors remain usable. Exhaustive matches
need the new constructors. `machineAsGraph` now returns an `Either`: an
`Executable` leaf cannot supply a topology diagram, so rendering reports missing
metadata instead of producing an incomplete graph.

The unconditional `NoThunks (StateMachineT ...)` instance is removed. It accepted
`Basic _` without checking its contents. Callers now choose how to handle
`NotInspected`. Descriptions and checks are independent; callbacks cover retained
state data, not action closures or effect environments. Inspection is not a
persistence API.

Crem still permits self-transitions, and the author still chooses the topology.
These changes do not replace Hydra's owner commands, round checks, candidate
evidence, or ledger and cryptographic primitives.
